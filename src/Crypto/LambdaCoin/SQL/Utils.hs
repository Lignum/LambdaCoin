module Crypto.LambdaCoin.SQL.Utils where

import Data.Int
import Data.String
import Data.Time.Clock.POSIX
import Data.Ratio

import qualified Database.SQLite.Simple as SQL

query :: [String] -> SQL.Query
query = fromString . unlines
