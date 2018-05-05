{-# LANGUAGE FlexibleContexts #-}
module Crypto.SQL(initDB) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Database.HDBC as SQL
import qualified Database.HDBC.Sqlite3 as SQL

checkError :: MonadError Integer m => Integer -> m ()
checkError e = unless (e == toInteger SQL.sqlite_OK) $ throwError e

initDB :: (MonadError Integer m, MonadIO m) => SQL.Connection -> m ()
initDB conn = do
  e <- liftIO $ SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `inputs` ("
                                      , "  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
                                      , "  `previous` BLOB NOT NULL,"
                                      , "  `outputIndex` INTEGER NOT NULL,"
                                      , "  `script` BLOB NOT NULL"
                                      , ");" ]) []
  checkError e
  e <- liftIO $ SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `outputs` ("
                                      , "  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,"
                                      , "  `value` INTEGER NOT NULL,"
                                      , "  `script` INTEGER NOT NULL"
                                      , ");" ]) []
  checkError e
  e <- liftIO $ SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `transactionToInput` ("
                                      , "  `transactionID` BLOB NOT NULL,"
                                      , "  `inputID` INTEGER NOT NULL"
                                      , ");" ]) []
  checkError e
  e <- liftIO $ SQL.run conn (unlines [ "CREATE TABLE IF NOT EXISTS `transactionToOutput` ("
                                      , "  `transactionID` BLOB NOT NULL,"
                                      , "  `outputID` INTEGER NOT NULL"
                                      , ");" ]) []
  checkError e
  e <- liftIO $ SQL.run conn "CREATE TABLE IF NOT EXISTS `transactions` ( `id` BLOB NOT NULL UNIQUE, PRIMARY KEY(`id`) )" []
  checkError e
  liftIO $ SQL.commit conn
  pure ()
