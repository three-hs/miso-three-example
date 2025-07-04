{-# LANGUAGE OverloadedStrings #-}

module Model where

import Miso.Lens
import Miso.Lens.TH

newtype Model = Model
  { _mIsInitialized :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model False

