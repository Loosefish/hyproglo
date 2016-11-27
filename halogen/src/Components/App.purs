module Components.App where

import Hpg.Prelude

import Data.Array ((:), catMaybes)

import Halogen as H
import Halogen (Component, ChildF, ParentDSL, ParentHTML, ParentState)
import Halogen.Component (query')
import Halogen.Component.ChildPath (ChildPath, cpL, cpR, (:>))
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed (href, src)

import Components.Albums as CAL
import Components.Artists as CAR
import Components.Playlist as CP
import Model
import Util (toClass, nbsp, fa, trimDate, onClickDo, formatTime)
import Mpd (currentSong, fetchStatus, queryMpd)


type State =
    { view :: View
    , song :: Maybe Song
    , status :: Maybe Status
    }

data View = Artists | Albums Artist | Playlist
data Query a
    = SetView View a
    | Update a
    | SendCmd String a
data Flag = Random | Repeat | Single

type ChildState = Either CAR.State (Either CAL.State CP.State)
type ChildQuery = Coproduct CAR.Query (Coproduct CAL.Query CP.Query)
type ChildSlot = Either CAR.Slot (Either CAL.Slot CP.Slot)

type State' g = ParentState State ChildState Query ChildQuery g ChildSlot
type Query' = Coproduct Query (ChildF ChildSlot ChildQuery)


pathArtists :: ChildPath CAR.State ChildState CAR.Query ChildQuery CAR.Slot ChildSlot
pathArtists = cpL


pathAlbums :: ChildPath CAL.State ChildState CAL.Query ChildQuery CAL.Slot ChildSlot
pathAlbums = cpR :> cpL


pathPlaylist :: ChildPath CP.State ChildState CP.Query ChildQuery CP.Slot ChildSlot
pathPlaylist = cpR :> cpR


init :: State
init =
    { view: Artists
    , song: Nothing
    , status : Nothing
    }


ui :: forall eff. Component (State' (Aff (AppEffects eff))) Query' (Aff (AppEffects eff))
ui = H.parentComponent { render, eval, peek: Nothing }
  where
    eval :: Query ~> ParentDSL State ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
    eval (SetView (Albums artist) next) = do
      H.modify (_ { view = Albums artist})
      query' pathAlbums CAL.Slot (H.action $ CAL.SetArtist artist)
      pure next

    eval (SetView view next) = do
      H.modify (_ { view = view})
      pure next

    eval (Update next) = do
      status <- H.fromAff $ fetchStatus
      song <- H.fromAff $ currentSong
      H.modify (_ { status = status, song = song})
      query' pathAlbums CAL.Slot (H.action $ CAL.SetCurrentSong song)
      pure next

    eval (SendCmd cmd next) = do
        status <- H.gets _.status
        H.fromAff $ queryMpd cmd
        pure next

    render :: State -> ParentHTML ChildState Query ChildQuery (Aff (AppEffects eff)) ChildSlot
    render state =
        HH.div [toClass "container-fluid"] 
        [ HH.div [toClass "row"]
            [ HH.div [toClass "col-xs-12 col-sm-3 col-md-2 sidebar"] sidebar
            , child state.view
            ]
        ]
      where
        sidebar =
            [ HH.h4 [toClass "page-header"] [HH.text $ "HyProGlo" <> nbsp]
            , HH.ul [toClass "nav nav-sidebar"] [musicLink, playlistLink state.status]
            , HH.div_ $ catMaybes [songInfo <$> state.status <*> state.song, controls <$> state.status]
            ]

        musicLink = navLink active "#/music" $ (fa "database fa-fw") : (HH.text $ nbsp <> "Music") : extra
          where
            active = case state.view of
                Playlist -> false
                _ -> true
            extra = case state.view of
                Albums (Artist { name }) -> [HH.text $ nbsp <> "/" <> nbsp <> name]
                _ -> []

        playlistLink status =
            navLink active "#/playlist" $ catMaybes
                [ Just $ fa "list fa-fw"
                , Just $ HH.text $ nbsp <> "Playlist"
                , playlistBadge <$> status
                ]
          where
            playlistBadge (Status { playlistLength }) =
                HH.span [toClass "badge pull-right"] [HH.text $ show $ playlistLength]
            active = case state.view of
                Playlist -> true
                _ -> false

        navLink active target content =
            HH.li (if active then [toClass "active"] else []) [HH.a [href target] content]

        child Artists = HH.slot' pathArtists CAR.Slot CAR.child
        child (Albums artist) = HH.slot' pathAlbums CAL.Slot $ CAL.child artist
        child Playlist = HH.slot' pathPlaylist CP.Slot CP.child

        controls (Status { random, repeat, single, playState }) = HH.div_
            [ buttonGroup [playButton]
            , buttonGroup
              [ button' false (SendCmd "previous") [fa "fast-backward"]
              , button' false (SendCmd "stop") [fa "stop"]
              , button' false (SendCmd "next") [fa "fast-forward"]
              ]
            , buttonGroup 
              [ button' random (SendCmd $ "random " <> if random then "0" else "1") [fa "random"]
              , button' repeat (SendCmd $ "repeat " <> if repeat then "0" else "1") [fa "repeat"]
              , button' single (SendCmd $ "single " <> if single then "0" else "1") [HH.text "1"]
              ]
            ]
          where
            buttonGroup = HH.div [toClass "btn-group btn-group-sm btn-group-justified"]
            button active = HH.a [toClass $ "btn btn-default" <> if active then " active" else ""]
            button' active action = HH.a [onClickDo action, toClass $ "btn btn-default" <> if active then " active" else ""]
            playButton = case playState of
              Play -> button' false (SendCmd "pause") [fa "pause"]
              _ -> button' false (SendCmd "play") [fa "play"]

        songInfo (Status { time }) (Song { file, title, artist, album }) = HH.div_
            [HH.img [toClass "img-responsive hidden-xs center-block", src $ "/image/" <> file]
            , HH.a (catMaybes [albumsUrl <$> album]) $ catMaybes
                [ Just $ HH.h5 [toClass "text-center"] [HH.text title]
                , Just $ HH.h6 [toClass "text-center"] [HH.text artist]
                , albumInfo <$> album
                , progress time
                ]
            , HH.div [] []
            ]
          where
            albumsUrl (Album { artist }) = href $ "#/music/" <> artistName artist
            albumInfo (Album a) =
                HH.h6 [toClass "text-center"]
                    [HH.text a.title, HH.small_ [HH.text $ nbsp <> "(" <> trimDate a.date <> ")"]]

            progress (Just (Tuple elapsed total)) =
              Just $ HH.div [toClass "progress"] [ HH.div [toClass "progress-bar"]
                                                 $ map HH.text
                                                   [ formatTime elapsed
                                                   , nbsp <> "/" <> nbsp
                                                   , formatTime total
                                                   ]
                                                 ]
            progress _ = Nothing
{--     <div class="progress"> <div class="progress-bar" style="width: 10%;"> 0:17&nbsp;/&nbsp;3:00 </div> </div> --}
