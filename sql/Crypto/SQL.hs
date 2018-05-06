{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings #-}
module Crypto.SQL(SQLObject(..)
                , initDB
                , transactionInsert) where

import Crypto.Transaction
import Crypto.Utils (Hash)

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Traversable
import Data.String
import Data.Proxy

import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow

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

query :: [String] -> SQL.Query
query = fromString . unlines

instance SQLObject Transaction B.ByteString where
  sqlRetrieve conn tid = liftIO . SQL.withTransaction conn $ do
    inputsSql <- SQL.query conn (query [ "SELECT `previous`, `outputIndex`, `script` FROM `transactions`"
                                       , "NATURAL JOIN `transactionToInput`"
                                       , "INNER JOIN `inputs` ON `inputs`.`id` = `inputID`"
                                       , "WHERE `transactions`.`id` = ?" ]) $ SQL.Only tid :: IO [(Hash, Int, BL.ByteString)]

    outputsSql <- SQL.query conn (query [ "SELECT `value`, `script` FROM `transactions`"
                                        , "NATURAL JOIN `transactionToOutput`"
                                        , "INNER JOIN `outputs` ON `outputs`.`id` = `outputID`"
                                        , "WHERE `transactions`.`id` = ?" ]) $ SQL.Only tid :: IO [(Int, BL.ByteString)]

    let inputs  = flip fmap inputsSql $ \(prev, oidx, scr) -> TxInput prev (OutputIndex oidx) $ decode scr
        outputs = flip fmap outputsSql $ \(val, scr) -> TxOutput val $ decode scr

    pure $ if null inputs || null outputs then Nothing
                                          else Just $ Transaction inputs outputs

  sqlInsert conn tid (Transaction ins outs) = liftIO . SQL.withTransaction conn $ do
    SQL.execute conn "INSERT INTO `transactions` (`id`, `block`) VALUES (?, ?)" (tid, Nothing :: Maybe Int)

    let inputs  = flip fmap ins $ \(TxInput prev (OutputIndex oidx) scr) -> (prev, oidx, encode scr)
        outputs = flip fmap outs $ \(TxOutput val scr) -> (val, encode scr)

    inputIDs <- forM inputs $ \r -> do
      SQL.execute conn "INSERT INTO `inputs` (`previous`, `outputIndex`, `script`) VALUES (?, ?, ?)" r
      SQL.lastInsertRowId conn

    forM_ inputIDs $ \iid ->
      SQL.execute conn "INSERT INTO `transactionToInput` (`id`, `inputID`) VALUES (?, ?)" (tid, iid)

    outputIDs <- forM outputs $ \r -> do
      SQL.execute conn "INSERT INTO `outputs` (`value`, `script`) VALUES (?, ?)" r
      SQL.lastInsertRowId conn

    forM_ outputIDs $ \oid ->
      SQL.execute conn "INSERT INTO `transactionToOutput` (`id`, `outputID`) VALUES (?, ?)" (tid, oid)

  sqlDelete _ conn tid = liftIO . SQL.withTransaction conn $ do
    inputIDs  <- SQL.query conn "SELECT `inputID` FROM `transactions` NATURAL JOIN `transactionToInput` WHERE `id` = ?" $ SQL.Only tid :: IO [SQL.Only Int]
    outputIDs <- SQL.query conn "SELECT `outputID` FROM `transactions` NATURAL JOIN `transactionToOutput` WHERE `id` = ?" $ SQL.Only tid :: IO [SQL.Only Int]

    SQL.executeMany conn "DELETE FROM `inputs` WHERE `id` = ?" inputIDs
    SQL.executeMany conn "DELETE FROM `outputs` WHERE `id` = ?" outputIDs

    SQL.executeMany conn "DELETE FROM `transactionToInput` WHERE `inputID` = ?" inputIDs
    SQL.executeMany conn "DELETE FROM `transactionToOutput` WHERE `outputID` = ?" outputIDs

    SQL.execute conn "DELETE FROM `transactions` WHERE `id` = ?" $ SQL.Only tid

transactionInsert :: MonadIO m => SQL.Connection -> Transaction -> m ()
transactionInsert conn tx = sqlInsert conn (transactionID tx) tx
