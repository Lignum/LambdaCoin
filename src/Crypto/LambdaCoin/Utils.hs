module Crypto.LambdaCoin.Utils where

import Crypto.Saltine.Core.Hash

import Data.Binary
import Data.List.Split
import Data.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Numeric

type Hash = B.ByteString

hashBinary :: Binary b => b -> Hash
hashBinary = hash . BL.toStrict . encode

hexEncode :: BL.ByteString -> String
hexEncode bs = concat $ leftPad . (`showHex` "") <$> BL.unpack bs where
  leftPad [x]     = ['0', x]
  leftPad (a:b:_) = [a, b]

hexEncode' :: B.ByteString -> String
hexEncode' = hexEncode . BL.fromStrict

hexDecode :: String -> BL.ByteString
hexDecode cs = BL.pack . catMaybes $ fmap (fromIntegral . fst) . listToMaybe . readHex <$> chunksOf 2 cs

hexDecode' :: String -> B.ByteString
hexDecode' = BL.toStrict . hexDecode
