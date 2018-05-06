module Crypto.LambdaCoin.SQL.Utils where

import Data.String

import qualified Database.SQLite.Simple as SQL

query :: [String] -> SQL.Query
query = fromString . unlines
