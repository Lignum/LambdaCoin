{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.LambdaCoin
import Crypto.LambdaCoin.Block
import Crypto.LambdaCoin.SQL
import Crypto.LambdaCoin.SQL.Block
import Crypto.LambdaCoin.SQL.Transaction
import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.Utils

import Data.String
import Data.Proxy
import Data.Time.Clock

import qualified Database.SQLite.Simple as SQL

main :: IO ()
main = do
  lambdaCoinInit
  SQL.withConnection "coin.db" $ \conn -> do
    initDB conn
    let tx = Transaction [TxInput "bkrek" (OutputIndex 64) TxInputScript] [TxOutput 230 TxOutputScript, TxOutput 400 TxOutputScript]
    transactionInsert conn tx

    let tx2 = Transaction [TxInput "brok" (OutputIndex 16) TxInputScript] [TxOutput 555 TxOutputScript, TxOutput 666 TxOutputScript]
    transactionInsert conn tx2

    ts <- getCurrentTime
    let header = BlockHeader { blockHeaderPrevBlock  = fromString $ replicate 64 '\0'
                             , blockHeaderMerkleRoot = transactionMerkleRoot [tx, tx2]
                             , blockHeaderTarget     = fromString $ '\x00' : replicate 63 '\xFF'
                             , blockHeaderTimestamp  = ts }
        block  = Block header [tx, tx2]
    blockInsert conn block
    pure ()
--    sqlDelete (Proxy :: Proxy Transaction) conn $ transactionID tx
