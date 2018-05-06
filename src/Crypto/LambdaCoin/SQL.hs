{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Crypto.LambdaCoin.SQL(SQLObject(..)
                           , initDB
                           , sqlEncode
                           , sqlDecode) where

import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.Utils
import Crypto.LambdaCoin.SQL.Utils

import Control.Monad.IO.Class

import Data.Binary
import Data.Proxy

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Database.SQLite.Simple as SQL

initDB :: MonadIO m => SQL.Connection -> m ()
initDB conn = liftIO $ do
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `inputs` ("
                            , "  `id` INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE,"
                            , "  `previous` BLOB NOT NULL,"
                            , "  `outputIndex` INTEGER NOT NULL,"
                            , "  `script` TEXT NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `outputs` ("
                            , "  `id` INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE,"
                            , "  `value` INTEGER NOT NULL,"
                            , "  `script` TEXT NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactionToInput` ("
                            , "  `id` TEXT NOT NULL,"
                            , "  `inputID` INTEGER NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactionToOutput` ("
                            , "  `id` TEXT NOT NULL,"
                            , "  `outputID` INTEGER NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactions` ("
                            , "  `id` TEXT NOT NULL UNIQUE,"
                            , "  PRIMARY KEY(`id`)"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `blocks` ("
                            , "  `hash` TEXT NOT NULL,"
                            , "  `previous` TEXT NOT NULL,"
                            , "  `merkleRoot` TEXT NOT NULL,"
                            , "  `target` TEXT NOT NULL,"
                            , "  `timestamp` TEXT NOT NULL,"
                            , "  PRIMARY KEY(`hash`)"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `blockTransactions` ("
                            , "  `hash` TEXT NOT NULL,"
                            , "  `id` TEXT NOT NULL"
                            , ");" ]

class SQLObject a k | a -> k where
  sqlRetrieve :: MonadIO m => SQL.Connection -> k -> m (Maybe a)
  sqlInsert   :: MonadIO m => SQL.Connection -> k -> a -> m ()
  sqlDelete   :: MonadIO m => Proxy a -> SQL.Connection -> k -> m ()

sqlEncode :: Binary b => b -> String
sqlEncode = hexEncode . encode

sqlDecode :: Binary b => String -> b
sqlDecode = decode . hexDecode
