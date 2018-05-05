module Crypto.LambdaCoin where

import Control.Monad.IO.Class
import Crypto.Saltine

lambdaCoinInit :: MonadIO m => m ()
lambdaCoinInit = liftIO sodiumInit
