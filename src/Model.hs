
module Model where

import Language.Javascript.JSaddle -- (JSVal(..))
import Miso.Lens (Lens, lens)
import Miso.Lens.TH (makeLenses)

instance Eq JSVal where
  JSVal v1 == JSVal v2 = v1 == v2

data Model = Model
  { _mCanvasId  :: Maybe JSVal
  , _mTime      :: Double
  , _mRunning   :: Bool
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model Nothing 0 True

