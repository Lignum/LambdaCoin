{-# LANGUAGE OverloadedStrings #-}
module Main where

import Crypto.LambdaCoin
import Crypto.LambdaCoin.SQL
import Crypto.LambdaCoin.SQL.Transaction
import Crypto.LambdaCoin.Transaction

import Data.Proxy

import qualified Database.SQLite.Simple as SQL

main :: IO ()
main = do
  lambdaCoinInit
  SQL.withConnection "coin.db" $ \conn -> do
    initDB conn
    let tx = Transaction [TxInput "bkrek" (OutputIndex 64) TxInputScript] [TxOutput 230 TxOutputScript, TxOutput 400 TxOutputScript]
    transactionInsert conn tx
    tx' <- sqlRetrieve conn $ transactionID tx :: IO (Maybe Transaction)
    sqlDelete (Proxy :: Proxy Transaction) conn $ transactionID tx
