module Crypto.LambdaCoin.Utils where

import Crypto.Saltine.Core.Hash

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

type Hash = B.ByteString

hashBinary :: Binary b => b -> Hash
hashBinary = hash . BL.toStrict . encode
