module Crypto.LambdaCoin.Block where

import Crypto.LambdaCoin.MerkleRoot
import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.Utils

import Data.Binary
import Data.Binary.Orphans
import Data.LargeWord
import Data.Time.Clock

import qualified Data.ByteString.Lazy as BL

type Target = Word256

data BlockHeader
  = BlockHeader { blockHeaderPrevBlock  :: Hash
                , blockHeaderMerkleRoot :: Hash
                , blockHeaderTarget     :: Target
                , blockHeaderTimestamp  :: UTCTime }
  deriving (Show, Eq)

instance Binary BlockHeader where
  put (BlockHeader prev mkrt tgt ts) = put prev >> put mkrt >> put tgt >> put ts
  get = BlockHeader <$> get <*> get <*> get <*> get

data Block
  = Block { blockHeader       :: BlockHeader
          , blockTransactions :: [Transaction] }
  deriving (Show, Eq)

instance Binary Block where
  put (Block hdr txs) = put hdr >> put txs
  get = Block <$> get <*> get

blockMerkleRoot :: Block -> Hash
blockMerkleRoot (Block _ txs) = merkleRoot $ BL.toStrict . encode <$> txs

blockHash :: Block -> Hash
blockHash = hashBinary
