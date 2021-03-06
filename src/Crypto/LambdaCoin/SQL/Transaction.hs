{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Crypto.LambdaCoin.SQL.Transaction where

import Crypto.LambdaCoin.SQL
import Crypto.LambdaCoin.SQL.Utils
import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.Utils

import Control.Monad
import Control.Monad.IO.Class

import Data.Binary

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Database.SQLite.Simple as SQL

instance SQLObject Transaction B.ByteString where
  sqlRetrieve conn tid' = liftIO $ do
    let tid = hexEncode' tid'
    inputsSql <- SQL.query conn (query [ "SELECT `previous`, `outputIndex`, `script` FROM `transactions`"
                                       , "NATURAL JOIN `transactionToInput`"
                                       , "INNER JOIN `inputs` ON `inputs`.`id` = `inputID`"
                                       , "WHERE `transactions`.`id` = ?" ]) $ SQL.Only tid :: IO [(String, Int, String)]

    outputsSql <- SQL.query conn (query [ "SELECT `value`, `script` FROM `transactions`"
                                        , "NATURAL JOIN `transactionToOutput`"
                                        , "INNER JOIN `outputs` ON `outputs`.`id` = `outputID`"
                                        , "WHERE `transactions`.`id` = ?" ]) $ SQL.Only tid :: IO [(Int, String)]

    let inputs  = flip fmap inputsSql $ \(prev, oidx, scr) -> TxInput (hexDecode' prev) (OutputIndex oidx) $ sqlDecode scr
        outputs = flip fmap outputsSql $ \(val, scr) -> TxOutput val $ sqlDecode scr

    pure $ if null inputs || null outputs then Nothing
                                          else Just $ Transaction inputs outputs

  sqlInsert conn tid' (Transaction ins outs) = liftIO $ do
    let tid = hexEncode' tid'
    SQL.execute conn "INSERT INTO `transactions` (`id`) VALUES (?)" $ SQL.Only tid

    let inputs  = flip fmap ins $ \(TxInput prev (OutputIndex oidx) scr) -> (hexEncode' prev, oidx, sqlEncode scr)
        outputs = flip fmap outs $ \(TxOutput val scr) -> (val, sqlEncode scr)

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

  sqlDelete _ conn tid = liftIO $ do
    inputIDs  <- SQL.query conn "SELECT `inputID` FROM `transactions` NATURAL JOIN `transactionToInput` WHERE `id` = ?" $ SQL.Only tid :: IO [SQL.Only Int]
    outputIDs <- SQL.query conn "SELECT `outputID` FROM `transactions` NATURAL JOIN `transactionToOutput` WHERE `id` = ?" $ SQL.Only tid :: IO [SQL.Only Int]

    SQL.executeMany conn "DELETE FROM `inputs` WHERE `id` = ?" inputIDs
    SQL.executeMany conn "DELETE FROM `outputs` WHERE `id` = ?" outputIDs

    SQL.executeMany conn "DELETE FROM `transactionToInput` WHERE `inputID` = ?" inputIDs
    SQL.executeMany conn "DELETE FROM `transactionToOutput` WHERE `outputID` = ?" outputIDs

    SQL.execute conn "DELETE FROM `transactions` WHERE `id` = ?" $ SQL.Only tid

transactionInsert :: MonadIO m => SQL.Connection -> Transaction -> m ()
transactionInsert conn tx = sqlInsert conn (transactionID tx) tx
