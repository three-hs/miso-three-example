{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad
import Data.Function ((&))
import Data.Foldable (traverse_)
import Language.Javascript.JSaddle 
-- import Data.Map as Map ((!?), adjust, keys, notMember)
import Miso
-- import Miso.Lens as Lens
import Miso.Canvas as Canvas
import Miso.String (MisoString, ms)

import THREE.BoxGeometry
import THREE.Internal
import THREE.Light
import THREE.Mesh
-- import THREE.MeshBasicMaterial
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
handleView () = div_ [] 
  [ p_ []
      [ a_ [ href_ "https://github.com/juliendehos/miso-three-test" ] [ text "source" ]
      , text " - "
      , a_ [ href_ "https://juliendehos.github.io/miso-three-test/" ] [ text "demo" ]
      ]
  , three_
  ]

three_ :: View Action
three_ = Canvas.canvas [] (asyncCallback draw)

draw :: Three ()
draw = do

  winWidth <- winInnerWidth
  winHeight <- winInnerHeight
  let winWidthI = round winWidth
  let winHeightI = round winHeight

  scene1 <- THREE.Scene.new 

  light1 <- THREE.PointLight.new
  light1 & intensity .= 300
  light1 ^. position !.. setXYZ 8 8 8
  scene1 & add light1

  material1 <- THREE.MeshLambertMaterial.new
  geometry1 <- THREE.SphereGeometry.new
  mesh1 <- THREE.Mesh.new (geometry1, material1)
  mesh1 & position !. x .= (-1)

  texture2 <- THREE.TextureLoader.new >>= load "miso.png"
  material2 <- THREE.MeshLambertMaterial.new
  material2 & THREE.MeshLambertMaterial.map .= Just texture2
  geometry2 <- THREE.BoxGeometry.new (1, 1, 1, Just 1, Just 1, Just 1)
  -- geometry2 <- THREE.BoxGeometry.new (1, 1, 1, Nothing, Nothing, Nothing)
  mesh2 <- THREE.Mesh.new (geometry2, material2)
  (mesh2 ^. position) !.. setXYZ 1 0 0 

  traverse_ (`add` scene1) [mesh1, mesh2]
  -- scene1 & add mesh1 >>= add mesh2

  camera1 <- THREE.PerspectiveCamera.new (70, winWidth / winHeight, 0.1, 100)
  camera1 & position !. z .= 6

  renderer1 <- THREE.WebGLRenderer.new
  renderer1 & setSize (winWidthI, winHeightI, True)

  renderer1 & setAnimationLoop (\_ _ [valTime] -> do
    time <- valToNumber valTime
    mesh2 & rotation !. y .= (time/1000)
    renderer1 & render (scene1, camera1)
    )

  domElement renderer1 >>= appendInBody 


{-
draw :: Three ()
draw = do
  width <- windowInnerWidth
  height <- windowInnerHeight
  let value = realToFrac (width `div` height)
  scene <- THREE.Scene.new
  camera <- THREE.PerspectiveCamera.new (75.0, value, 0.1, 1000)
  renderer <- THREE.WebGLRenderer.new
  renderer & THREE.WebGLRenderer.setSize (width, height, True)
  geometry <- THREE.BoxGeometry.new (10,10,10,Nothing,Nothing,Nothing)
  material <- THREE.MeshBasicMaterial.new Nothing
  material & THREE.MeshBasicMaterial.color .= "#000fff"
  cube <- THREE.Mesh.new (geometry,material)
  void (scene & THREE.Object3D.add cube)
  camera & THREE.Object3D.position !. z .= 300
  renderer & THREE.WebGLRenderer.render (scene, camera)
  cube & THREE.Object3D.rotation !. x += 0.1
  cube & THREE.Object3D.rotation !. y += 0.1
-}

----------------------------------------------------------------------
-- update handler
----------------------------------------------------------------------

handleUpdate :: Action -> Effect Model Action

handleUpdate () = pure ()

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

main :: IO ()
main = run $ do

  let model = ()

      app :: Component "app" Model Action
      app = (defaultComponent model handleUpdate handleView)
        { logLevel = DebugAll
        }

  startComponent app

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

