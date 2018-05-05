module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Crypto.LambdaCoin
import Crypto.SQL

import qualified Database.HDBC.Sqlite3 as SQL

main :: IO ()
main = do
  lambdaCoinInit
  conn <- SQL.connectSqlite3 "coin.db"
  e <- runExceptT $ initDB conn
  case e of
    Left e  -> putStrLn $ "Failed to initialise database. SQLite error code: " ++ show e
    Right _ -> pure ()
