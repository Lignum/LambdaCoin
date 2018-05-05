module Crypto.Transaction where

import Crypto.Utils

import Data.Bifunctor
import Data.Binary

newtype OutputIndex
  = OutputIndex Int
  deriving (Show, Eq, Ord)

instance Binary OutputIndex where
  put (OutputIndex i) = put i
  get = OutputIndex <$> get

data TxInputScript
  = TxInputScript
  deriving (Show, Eq)

instance Binary TxInputScript where
  put _ = pure ()
  get = pure TxInputScript

data TxInput
  = TxInput { txInputPreviousHash :: Hash
            , txInputOutputIndex  :: OutputIndex
            , txInputScript       :: TxInputScript }
  deriving (Show, Eq)

instance Binary TxInput where
  put (TxInput prev oidx iscr) = put prev >> put oidx >> put iscr
  get = TxInput <$> get <*> get <*> get

data TxOutputScript
  = TxOutputScript
  deriving (Show, Eq)

instance Binary TxOutputScript where
  put _ = pure ()
  get = pure TxOutputScript

data TxOutput
  = TxOutput { txOutputValue  :: Int
             , txOutputScript :: TxOutputScript }
  deriving (Show, Eq)

instance Binary TxOutput where
  put (TxOutput val oscr) = put val >> put oscr
  get = TxOutput <$> get <*> get

data Transaction
  = Transaction [TxInput] [TxOutput]
  deriving (Show, Eq)

instance Binary Transaction where
  put (Transaction ins outs) = put ins >> put outs
  get = Transaction <$> get <*> get

transactionID :: Transaction -> Hash
transactionID = hashBinary
