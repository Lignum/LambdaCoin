{-# LANGUAGE LambdaCase #-}
module Crypto.LambdaCoin.MerkleRoot where

import Crypto.Saltine.Core.Hash

import qualified Data.ByteString as B

import Data.List.Split
import Data.Semigroup

merkleRoot :: [B.ByteString] -> B.ByteString
merkleRoot lvs = go $ hash <$> lvs where
  go [x] = x
  go xs  = go . flip fmap (chunksOf 2 xs) $ \case
                                               [a, b] -> hash $ a <> b
                                               [x]    -> hash x
