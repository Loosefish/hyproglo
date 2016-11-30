module Mpd where

import Hpg.Prelude

import Control.Monad.Except (runExcept)

import Data.Array as A
import Data.Foreign (fail, ForeignError(..))
import Data.Foreign.Class (read, readProp, class IsForeign)
import Data.Foreign.Null (unNull)
import Data.Int (fromString)
import Data.List ((:))
import Data.List as L
import Data.String as S
import Data.Tuple (lookup)

import Network.HTTP.Affjax (AJAX, post)

import Model (Album(..), Artist(..), PlayState(..), Song(..), Status(..))


type Assoc a b = List (Tuple a b)


data Mpd = Mpd (Either String String)
instance mpdIsForeign :: IsForeign Mpd where
    read value = do
        result <- unNull <$> readProp "result" value
        status <- unNull <$> readProp "status" value
        error <- unNull <$> readProp "error" value
        case [result, status, error] of
            [Just r, _, _] -> pure $ case S.stripSuffix (Pattern "\nOK\n") r of
                Nothing -> Mpd (Left r)
                Just content -> Mpd (Right content)

            [_ , _, Just e] -> failForeign $ "Error: " <> e

            [_ , Just s, _] -> failForeign $ "Status: " <> s

            _ -> failForeign "Unexpected response"
      where
        failForeign = fail <<< ForeignError


type MpdEffect = forall eff. Aff (ajax :: AJAX | eff) Mpd


class FromMpd a where
  parseOne :: Assoc String String -> Maybe a


fetchOne :: forall a eff. (FromMpd a) => String -> Aff (ajax :: AJAX | eff) (Maybe a)
fetchOne q = parseOne <<< toAssoc <$> queryMpd q


parseMany :: forall a. (FromMpd a) => Mpd -> Array a
parseMany = A.fromFoldable <<< L.mapMaybe parseOne <<< group <<< toAssoc


fetch :: forall a eff. (FromMpd a) => String -> Aff (ajax :: AJAX | eff) (Array a)
fetch q = parseMany <$> queryMpd q


-- Basic

queryMpd :: String -> MpdEffect
queryMpd code = do
    result <- post "/mpd" code
    let response = result.response
    pure case runExcept $ read response of
        Right mpd -> mpd
        Left _ -> Mpd (Left "Invalid response")


toAssoc :: Mpd -> Assoc String String
toAssoc (Mpd (Left _)) = Nil
toAssoc (Mpd (Right content)) = L.mapMaybe (splitOnce ": ") lines
  where
    lines = L.fromFoldable $ S.split (Pattern "\n") content


splitOnce :: String -> String -> Maybe (Tuple String String)
splitOnce p s = case S.indexOf (Pattern p) s of
    Just i -> Just $ Tuple (S.take i s) (S.drop (i + S.length p) s)
    _ -> Nothing


group :: Assoc String String -> List (Assoc String String)
group Nil = Nil
group (head@(Tuple key _) : rest) = group' (head : Nil) rest
  where
    group' current Nil = current : Nil
    group' current (p : ps) | fst p == key = current : group' (p : Nil) ps
    group' current (p : ps) = group' (p : current) ps


quote :: String -> String
quote s = "\"" <> escape s <> "\""


escape :: String -> String
escape = S.replaceAll (Pattern "\"") (Replacement "\\\"")


toBoolean :: String -> Boolean
toBoolean "1" = true
toBoolean _ = false


fromBoolean :: Boolean -> String
fromBoolean true = "1"
fromBoolean false = "0"


-- Artist
instance fromMpdArtist :: FromMpd Artist where
    parseOne pairs = do
        name <- lookup "AlbumArtist" pairs <|> lookup "Artist" pairs
        pure $ Artist { name }

fetchAlbumArtists :: forall eff. Aff (ajax :: AJAX | eff) (Array Artist)
fetchAlbumArtists = fetch "list AlbumArtist"


-- Album
instance fromMpdAlbum :: FromMpd Album where
    parseOne pairs = do
        artist <- parseOne pairs
        date <- lookup "Date" pairs
        title <- lookup "Album" pairs
        pure $ Album { artist, date, title }

fetchAlbums :: forall eff. Artist -> Aff (ajax :: AJAX | eff) (Array Album)
fetchAlbums (Artist { name }) =
    fetch $ "list Album AlbumArtist " <> quote name <> " group AlbumArtist group Date"


fetchAlbums' :: forall eff. Artist -> Aff (ajax :: AJAX | eff) (Array (Tuple Album (Array Song)))
fetchAlbums' (Artist { name }) = do
    songs <- fetch $ "find AlbumArtist " <> quote name
    let albums = A.nub $ A.mapMaybe (\(Song { album }) -> album) songs
    pure $ map (\a -> Tuple a $ filter a songs) albums
  where
    filter album songs =
        A.filter (\(Song s) -> maybe false ((==) album) s.album) songs


-- Song
instance fromMpdSong :: FromMpd Song where
    parseOne pairs = do
        file <- lookup "file" pairs
        title <- lookup "Title" pairs
        time <- fromString =<< lookup "Time" pairs
        artist <- lookup "Artist" pairs <|> lookup "AlbumArtist" pairs
        let disc = lookup "Disc" pairs
        let track = lookup "Track" pairs
        let album = parseOne pairs
        pure $ Song { artist, disc, file, time, title, track, album }

fetchSongs :: forall eff. Album -> Aff (ajax :: AJAX | eff) (Array Song)
fetchSongs (Album { artist: Artist artist, date: date, title: title }) =
    fetch $ S.joinWith " "
        ["find Album", quote title, "Date", quote date, "AlbumArtist", quote artist.name]


-- Control

data MpdCmd = Play (Maybe Int) | Clear | AddSong Song | AddAlbum Album

sendCmds :: Array MpdCmd -> MpdEffect
sendCmds cmds = queryMpd $ S.joinWith "\n" $ map raw cmds
  where
    raw cmd = case cmd of
      Play i -> "play " <> maybe "" show i
      Clear -> "clear"
      AddSong (Song { file }) -> "add " <> quote file
      AddAlbum (Album { artist, date, title }) ->
        S.joinWith " "
            [ "findadd"
            , "AlbumArtist", quote $ (\(Artist { name }) -> name) artist
            , "Date", quote date
            , "Album", quote title
            ]
          
sendCmd ::  MpdCmd -> MpdEffect
sendCmd cmd = sendCmds [cmd]


-- Status

instance fromMpdStatus :: FromMpd Status where
    parseOne pairs = do
        repeat <- toBoolean <$> lookup "repeat" pairs
        random <- toBoolean <$> lookup "random" pairs
        single <- toBoolean <$> lookup "single" pairs
        playState <- parseOne pairs
        playlistLength <- fromString =<< lookup "playlistlength" pairs
        let time = parseTime =<< lookup "time" pairs
        pure $ Status { repeat, random, single, playState, time, playlistLength }
      where
        parseTime t = case (map fromString $ S.split (S.Pattern ":") t) of
            [Just elapsed, Just total] -> Just $ Tuple elapsed total
            _ -> Nothing

    
instance fromMpdPlayState :: FromMpd PlayState where
  parseOne pairs = case lookup "state" pairs of
    Just "play" -> Just Playing
    Just "pause" -> Just Paused
    Just "stop" -> Just Stopped
    _ -> Nothing


fetchStatus :: forall eff. Aff (ajax :: AJAX | eff) (Maybe Status)
fetchStatus = fetchOne "status"


currentSong :: forall eff. Aff (ajax :: AJAX | eff) (Maybe Song)
currentSong = fetchOne "currentsong"


fetchStatusSong :: forall eff. Aff (ajax :: AJAX | eff) (Tuple (Maybe Status) (Maybe Song))
fetchStatusSong = do
    pairs <- toAssoc <$> queryMpd "status\ncurrentsong"
    let status = parseOne pairs
    let song = parseOne pairs
    pure $ Tuple status song
