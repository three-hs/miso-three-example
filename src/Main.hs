{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void, when)
import Data.Function ((&))
import Data.Foldable (traverse_)
import GHC.Generics
import Language.Javascript.JSaddle as JS
import Miso
import Miso.Lens qualified as Lens
import Miso.Canvas qualified as Canvas
import Miso.String (ms)

import THREE.BoxGeometry
import THREE.Internal
import THREE.Light
import THREE.Mesh
import THREE.MeshLambertMaterial
import THREE.Object3D
import THREE.PerspectiveCamera
import THREE.PointLight
import THREE.Scene
import THREE.SphereGeometry
import THREE.TextureLoader
import THREE.Vector3
import THREE.WebGLRenderer

import Model

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

canvasWidth, canvasHeight :: Int
canvasWidth = 800
canvasHeight = 600

canvasWidthD, canvasHeightD :: Double
canvasWidthD = fromIntegral canvasWidth
canvasHeightD = fromIntegral canvasHeight

----------------------------------------------------------------------
-- canvas/three context
----------------------------------------------------------------------

data Context = Context
  { renderer  :: THREE.WebGLRenderer.WebGLRenderer
  , scene     :: THREE.Scene.Scene
  , camera    :: THREE.PerspectiveCamera.PerspectiveCamera
  , cube      :: THREE.Mesh.Mesh
  } deriving (Generic, FromJSVal, ToJSVal)

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
  , three_ model
  ]
  where
    pauseOrRun = if model Lens.^. mRunning then "pause" else "run"

three_ :: Model -> View Action
three_ model = 
  Canvas.canvas_
  [ width_ (ms canvasWidth)
  , height_ (ms canvasHeight)
  ] 
  initCanvas
  (drawCanvas model)

initCanvas :: DOMRef -> Three Context
initCanvas domref = do
  scene1 <- THREE.Scene.new 

  light1 <- THREE.PointLight.new
  light1 & intensity .= 300
  light1 ^. position !.. setXYZ 8 8 8
  void $ scene1 & add light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new (geometry1, material1)
  mesh1 & position !. x .= (-1)

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & THREE.MeshLambertMaterial.map .= Just texture2
  geometry2 <- THREE.BoxGeometry.new (1, 1, 1)
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new (70, canvasWidthD/canvasHeightD, 0.1, 100)
  camera1 & position !. z .= 6

  renderer1 <- THREE.WebGLRenderer.new (Just domref)
  renderer1 & setSize (canvasWidth, canvasHeight, True)

  pure $ Context renderer1 scene1 camera1 mesh2
  
drawCanvas :: Model -> Context -> Three ()
drawCanvas model Context {..} = 
  when (model Lens.^. mRunning) $ do
    cube & rotation !. y .= (model Lens.^. mTime)
    renderer & render (scene, camera)

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate (ActionTime t) = do
  mTime Lens..= t
  io (ActionTime <$> myGetTime)

handleUpdate ActionSwitchRunning = do
  mRunning Lens.%= not

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

