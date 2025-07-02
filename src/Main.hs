{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Map as Map ((!?), adjust, keys, notMember)
import Miso
import Miso.Lens as Lens
import Miso.Media (Media(..), videoHeight, videoWidth)
import Miso.String (MisoString, ms)

import Model

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

theSmallWidth :: Int
theSmallWidth = 400

thePlaylist :: [MisoString]
thePlaylist = []

----------------------------------------------------------------------
-- actions
----------------------------------------------------------------------

data Action
  = ActionAskVideo MisoString
  | ActionAskSwitch
  | ActionAskSize ClipId Media
  | ActionSetSize ClipId Int Int

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ p_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-three-test" ] [ text "source" ]
      , text " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-three-test/" ] [ text "demo" ]
      ]
  , p_ [] 
      [ select_ [ onChange ActionAskVideo ] myOptions
      , button_ [ onClick ActionAskSwitch ] [ mySmall ]
      ]
  , p_ [] myVideo
  ]
  where

    myOptions = 
      option_ [ value_ "" ] [ "--" ] :
      map fmtClip (keys $ model^.modelPlaylist)

    fmtClip cId = let i = cId^.clipId in option_ [ value_ i ] [ text i ]

    mySmall = if model^.modelSmall then "switch to original size" else "switch to small size"

    myVideo = 
      let mPlaying = model^.modelPlaying
          mClip = mPlaying >>= ((model^.modelPlaylist) !?)
      in case (mPlaying, mClip)  of
        (Just i, Just c) -> 
          [ video_ 
            ( src_ (i^.clipId) : 
              fmtSize c ++
              [ id_ "myvideo"
              , controls_ True
              , onCanPlayWith (ActionAskSize i)
              ]
            )
            []
          ]
        _ -> []

    fmtSize c =
      let w = max 1 (c^.clipWidth)
          h = c^.clipHeight
      in if model^.modelSmall
      then [ width_ (ms theSmallWidth), height_ (ms $ theSmallWidth*h `div` w) ]
      else [ width_ (ms w), height_ (ms h) ]

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionAskVideo str) = do
  playlist <- use modelPlaylist
  modelPlaying .= if str == ""  || notMember (mkClipId str) playlist
    then Nothing 
    else Just (mkClipId str)

handleUpdate ActionAskSwitch = 
  modelSmall %= not

handleUpdate (ActionAskSize cId m) = 
  io (ActionSetSize cId <$> videoWidth m <*> videoHeight m)

handleUpdate (ActionSetSize cId w h) = 
  modelPlaylist %= adjust upClip cId
  where
    upClip c = c & clipWidth .~ w
                 & clipHeight .~ h

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do
  let model = mkModel thePlaylist
      app = defaultComponent model handleUpdate handleView
  startComponent app
    { events = defaultEvents <> mediaEvents
    , logLevel = DebugAll
    }

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

