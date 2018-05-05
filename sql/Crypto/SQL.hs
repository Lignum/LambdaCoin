{-# LANGUAGE FlexibleContexts #-}
module Crypto.SQL(initDB) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Database.HDBC as SQL
import qualified Database.HDBC.Sqlite3 as SQL

initDB :: MonadIO m => SQL.Connection -> m ()
initDB conn = liftIO $ do
  SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `inputs` ("
                        , "  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
                        , "  `previous` BLOB NOT NULL,"
                        , "  `outputIndex` INTEGER NOT NULL,"
                        , "  `script` BLOB NOT NULL"
                        , ");" ]) []
  SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `outputs` ("
                        , "  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
                        , "  `value` INTEGER NOT NULL,"
                        , "  `script` INTEGER NOT NULL"
                        , ");" ]) []
  SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `transactionToInput` ("
                        , "  `id` BLOB NOT NULL,"
                        , "  `inputID` INTEGER NOT NULL"
                        , ");" ]) []
  SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `transactionToOutput` ("
                        , "  `id` BLOB NOT NULL,"
                        , "  `outputID` INTEGER NOT NULL"
                        , ");" ]) []
  SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `transactions` ("
                        , "  `id` BLOB NOT NULL UNIQUE,"
                        , "  `block` INTEGER,"
                        , "  PRIMARY KEY(`id`)"
                        , ");" ]) []
  SQL.commit conn
  pure ()
