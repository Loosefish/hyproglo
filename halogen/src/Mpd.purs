module Mpd (fetchAlbumArtists, fetchAlbums, fetchSongs) where

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

import Model


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


-- Artist

fetchAlbumArtists :: forall eff. Aff (ajax :: AJAX | eff) (Array Artist)
fetchAlbumArtists =
    A.fromFoldable <<< parseArtists <$> queryMpd "list AlbumArtist"


parseArtists :: Mpd -> List Artist
parseArtists = L.mapMaybe parseArtist <<< group <<< toAssoc


parseArtist :: Assoc String String -> Maybe Artist
parseArtist pairs = do
    name <- lookup "AlbumArtist" pairs <|> lookup "Artist" pairs
    pure $ Artist { name }


-- Album

fetchAlbums :: forall eff. Artist -> Aff (ajax :: AJAX | eff) (Array Album)
fetchAlbums (Artist { name }) =
    A.fromFoldable <<< L.sortBy (comparing (\(Album a) -> a.date)) <<< parseAlbums <$> queryMpd query
  where
    query = "list Album AlbumArtist " <> quote name <> " group AlbumArtist group Date"


parseAlbums :: Mpd -> List Album
parseAlbums = L.mapMaybe parseAlbum <<< group <<< toAssoc

parseAlbum :: Assoc String String -> Maybe Album
parseAlbum pairs = do
    artist <- parseArtist pairs
    date <- lookup "Date" pairs
    title <- lookup "Album" pairs
    pure $ Album { artist, date, title }


-- Song

fetchSongs :: forall eff. Album -> Aff (ajax :: AJAX | eff) (Array Song)
fetchSongs (Album { artist: Artist artist, date: date, title: title }) =
    A.fromFoldable <<< parseSongs <$> queryMpd query
  where
    query = S.joinWith " "
        $ ["find Album", quote title, "Date", quote date, "AlbumArtist", quote artist.name]


parseSongs :: Mpd -> List Song
parseSongs = L.mapMaybe parseSong <<< group <<< toAssoc


parseSong :: Assoc String String -> Maybe Song
parseSong pairs = do
    file <- lookup "file" pairs
    title <- lookup "Title" pairs
    time <- fromString =<< lookup "Time" pairs
    artist <- lookup "Artist" pairs <|> lookup "AlbumArtist" pairs
    let disc = lookup "Disc" pairs
    let track = lookup "Track" pairs
    pure $ Song { artist, disc, file, time, title, track }


-- Basic

queryMpd :: forall eff. String -> Aff (ajax :: AJAX | eff) Mpd
queryMpd code = do
    result <- post "/cgi-bin/mpd" code
    let response = result.response
    pure case runExcept $ read response of
        Right mpd -> mpd
        Left _ -> Mpd (Left "Invalid response")


toAssoc :: Mpd -> Assoc String String
toAssoc (Mpd (Left _)) = Nil
toAssoc (Mpd (Right content)) = L.mapMaybe (splitOnce ": ") lines
  where
    lines = L.fromFoldable $ S.split (Pattern "\n") content
    splitOnce p s =
        (\ i -> Tuple (S.take i s) (S.drop (i + S.length p) s))
        <$> S.indexOf (Pattern p) s


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