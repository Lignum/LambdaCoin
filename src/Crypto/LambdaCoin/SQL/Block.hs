{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Crypto.LambdaCoin.SQL.Block where

import Crypto.LambdaCoin.Block
import Crypto.LambdaCoin.Transaction
import Crypto.LambdaCoin.SQL
import Crypto.LambdaCoin.SQL.Transaction
import Crypto.LambdaCoin.SQL.Utils
import Crypto.LambdaCoin.Utils

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as B
import Data.Time.Clock
import Data.Time.Format
import Data.Maybe

import qualified Database.SQLite.Simple as SQL

instance SQLObject Block B.ByteString where
  sqlRetrieve conn hash' = liftIO $ do
    let hash = hexEncode' hash'
    bh <- listToMaybe <$> SQL.query conn "SELECT `previous`, `merkleRoot`, `target`, `timestamp` FROM `blocks` WHERE `hash` = ?" (SQL.Only hash) :: IO (Maybe (String, String, String, String))
    case bh of
      Just (prev, mkrt, tgt, ts) ->
        case parseTimeM True defaultTimeLocale rfc822DateFormat ts :: Maybe UTCTime of
          Just ts -> do let header = BlockHeader (hexDecode' prev) (hexDecode' mkrt) (hexDecode' tgt) ts
                        tids <- SQL.query conn "SELECT `id` FROM `blockTransactions` WHERE `hash` = ?" $ SQL.Only hash :: IO [SQL.Only String]
                        txs  <- catMaybes <$> traverse (sqlRetrieve conn) (hexDecode' . SQL.fromOnly <$> tids)
                        pure . Just $ Block header txs
          Nothing -> pure Nothing
      Nothing -> pure Nothing

  sqlInsert conn hash' bl = liftIO $ do
    let hash                           = hexEncode' hash'
        (BlockHeader prev mkrt tgt ts) = blockHeader bl
        ts'                            = formatTime defaultTimeLocale rfc822DateFormat ts
        txs                            = blockTransactions bl
    SQL.execute conn "INSERT INTO `blocks` (`hash`, `previous`, `merkleRoot`, `target`, `timestamp`) VALUES (?, ?, ?, ?, ?)" (hash, hexEncode' prev, hexEncode' mkrt, hexEncode' tgt, ts')
    forM_ txs $ \tx -> do
      let txid = transactionID tx
      tx' <- sqlRetrieve conn txid :: IO (Maybe Transaction)
      case tx' of
        Just _   -> pure ()
        Nothing  -> transactionInsert conn tx
      SQL.execute conn "INSERT INTO `blockTransactions` (`hash`, `id`) VALUES (?, ?)" (hash, hexEncode' txid)

  sqlDelete _ conn hash = pure ()

blockInsert :: MonadIO m => SQL.Connection -> Block -> m ()
blockInsert conn bl = sqlInsert conn (blockHash bl) bl
