{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle (JSM)
import Miso
import Miso.Lens 
import Miso.Canvas qualified as Canvas
import Miso.String (ms)

import Model
import MyThree

----------------------------------------------------------------------
-- actions
----------------------------------------------------------------------

data Action
  = ActionTime Double
  | ActionSwitchRunning

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView model = div_ [] 
  [ h1_ [] [ "miso-three-test" ]
  , p_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-three-test" ] [ "source" ]
      , " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-three-test/" ] [ "demo" ]
      , " - "
      , button_ [ onClick ActionSwitchRunning ] [ pauseOrRun ]
      ]
  , Canvas.canvas_
      [ width_ (ms canvasWidth)
      , height_ (ms canvasHeight)
      ] 
      initCanvas
      (drawCanvas model)
  ]
  where
    pauseOrRun = if model ^. mRunning then "pause" else "run"

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionTime t) = do
  mTime .= t
  io (ActionTime <$> myGetTime)

handleUpdate ActionSwitchRunning = do
  mRunning %= not

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

myGetTime :: JSM Double
myGetTime = (* 0.001) <$> now

main :: IO ()
main = run $ do
  let model = mkModel
  startComponent
    (component model handleUpdate handleView)
      { logLevel = DebugAll
      , initialAction = Just (ActionTime 0)
      }

