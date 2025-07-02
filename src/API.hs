
{-# LANGUAGE OverloadedStrings #-}

module API 
  ( valToNumber
  , winInnerWidth
  , winInnerHeight
  , myNewWebGLRenderer
  , theCanvas
  , appendInBody
  ) where

import Control.Lens hiding ((#))
import Language.Javascript.JSaddle as JS

import THREE.Internal hiding ((^.))
import THREE.PointLight
import THREE.WebGLRenderer

instance FromJSVal PointLight where
  fromJSVal = pure . Just . PointLight

appendInBody :: JSVal -> JSM ()
appendInBody v = void $ jsg "document" ^. js "body" ^. js1 "appendChild" v

winInnerWidth :: JSM Double
winInnerWidth = valToNumber =<< jsg "window"  ^. js "innerWidth"

winInnerHeight :: JSM Double
winInnerHeight = valToNumber =<< jsg "window"  ^. js "innerHeight"

myNewWebGLRenderer :: JSString -> THREE.Internal.Three WebGLRenderer
myNewWebGLRenderer canvasId = do
  o <- obj
  v <- toJSVal canvasId
  setProp "canvas" v o
  -- o <# "canvas" $ canvasId
  THREE.Internal.new WebGLRenderer "WebGLRenderer" o

theCanvas :: Property WebGLRenderer JSString
theCanvas = property "canvas"


