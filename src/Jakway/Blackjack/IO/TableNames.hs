module Jakway.Blackjack.IO.TableNames where

import Jakway.Blackjack.Util
import Database.HDBC

--only need 3 table names because the cards table is reused
type TableNames = (String, String, String)

baseTableNames :: TableNames
baseTableNames = ("players_", "hands_", "matches_")

getTableNames :: String -> TableNames
getTableNames suffix = innerMapTuple3 (++ suffix) baseTableNames

getPlayerTableName :: TableNames -> String
getPlayerTableName (name,_,_) = name

getHandTableName :: TableNames -> String
getHandTableName (_,name,_) = name

getMatchTableName :: TableNames -> String
getMatchTableName (_,_,name) = name

tableNamesToSql :: TableNames -> [SqlValue]
tableNamesToSql (a,b,c) = map toSql [a,b,c]
