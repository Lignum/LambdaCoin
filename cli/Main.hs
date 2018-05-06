{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.LambdaCoin
import Crypto.SQL
import Crypto.Transaction

import Data.Proxy

import qualified Database.SQLite.Simple as SQL

main :: IO ()
main = do
  lambdaCoinInit
  SQL.withConnection "coin.db" $ \conn -> do
    initDB conn
    let tx = Transaction [TxInput "bkrek" (OutputIndex 64) TxInputScript] [TxOutput 230 TxOutputScript, TxOutput 400 TxOutputScript]
    print tx
    transactionInsert conn tx
    tx' <- sqlRetrieve conn $ transactionID tx
    print tx'
    print $ Just tx == tx'
    sqlDelete (Proxy :: Proxy Transaction) conn $ transactionID tx
