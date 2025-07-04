{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Function ((&))
import Data.Foldable (traverse_)
import Miso
import Miso.Canvas as Canvas
import Miso.String (MisoString, ms)

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

import API

----------------------------------------------------------------------
-- parameters
----------------------------------------------------------------------

canvasId :: MisoString
canvasId = "threeCanvas"

canvasWidth, canvasHeight :: Int
canvasWidth = 800
canvasHeight = 600

canvasWidthD, canvasHeightD :: Double
canvasWidthD = fromIntegral canvasWidth
canvasHeightD = fromIntegral canvasHeight

----------------------------------------------------------------------
-- model
----------------------------------------------------------------------

type Model = ()

----------------------------------------------------------------------
-- actions
----------------------------------------------------------------------

type Action = ()

----------------------------------------------------------------------
-- view handler
----------------------------------------------------------------------

handleView :: Model -> View Action
handleView _ = div_ [] 
  [ h1_ [] [ text "miso-three-test" ]
  , p_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-three-test" ] [ text "source" ]
      , text " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-three-test/" ] [ text "demo" ]
      ]
  , three_
  ]

three_ :: View Action
three_ = 
  Canvas.canvas 
  [ id_ canvasId
  , width_ (ms canvasWidth)
  , height_ (ms canvasHeight)
  ] 
  (asyncCallback draw)

draw :: Three ()
draw = do

  {-
  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight
  -}

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
  -- geometry2 <- THREE.BoxGeometry.new (1, 1, 1, Nothing, Nothing, Nothing)
  geometry2 <- THREE.BoxGeometry.new (1, 1, 1, Just 1, Just 1, Just 1)
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]

  camera1 <- THREE.PerspectiveCamera.new (70, canvasWidthD/canvasHeightD, 0.1, 100)
  -- camera1 <- THREE.PerspectiveCamera.new (70, winWidth / winHeight, 0.1, 100)
  camera1 & position !. z .= 6

  renderer1 <- getElementById canvasId >>= myNewWebGLRenderer 
  -- renderer1 <- THREE.WebGLRenderer.new
  renderer1 & setSize (canvasWidth, canvasHeight, True)
  -- renderer1 & setSize (winWidthI, winHeightI, True)

  -- renderer1 & render (scene1, camera1)

  -- TODO handle time and loop using miso
  renderer1 & setAnimationLoop (\_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & rotation !. y .= (time/1000)
    renderer1 & render (scene1, camera1)
    )

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: () -> Effect Model Action

handleUpdate () = pure ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ do
  let model = ()
  startComponent
    (component model handleUpdate handleView)
      { logLevel = DebugAll
      }

