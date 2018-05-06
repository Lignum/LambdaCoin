{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Crypto.LambdaCoin.SQL(SQLObject(..)
                           , initDB) where

import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.SQL.Utils

import Control.Monad.IO.Class

import Data.Proxy

import qualified Database.SQLite.Simple as SQL

initDB :: MonadIO m => SQL.Connection -> m ()
initDB conn = liftIO $ do
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `inputs` ("
                            , "  `id` INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE,"
                            , "  `previous` BLOB NOT NULL,"
                            , "  `outputIndex` INTEGER NOT NULL,"
                            , "  `script` BLOB NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `outputs` ("
                            , "  `id` INTEGER PRIMARY KEY AUTOINCREMENT UNIQUE,"
                            , "  `value` INTEGER NOT NULL,"
                            , "  `script` BLOB NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactionToInput` ("
                            , "  `id` BLOB NOT NULL,"
                            , "  `inputID` INTEGER NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactionToOutput` ("
                            , "  `id` BLOB NOT NULL,"
                            , "  `outputID` INTEGER NOT NULL"
                            , ");" ]
  SQL.execute_ conn $ query [ "CREATE TABLE IF NOT EXISTS `transactions` ("
                            , "  `id` BLOB UNIQUE,"
                            , "  `block` INTEGER,"
                            , "  PRIMARY KEY(`id`)"
                            , ");" ]

class SQLObject a k | a -> k where
  sqlRetrieve :: MonadIO m => SQL.Connection -> k -> m (Maybe a)
  sqlInsert   :: MonadIO m => SQL.Connection -> k -> a -> m ()
  sqlDelete   :: MonadIO m => Proxy a -> SQL.Connection -> k -> m ()
