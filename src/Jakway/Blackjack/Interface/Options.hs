{-# LANGUAGE ScopedTypeVariables #-}
module Jakway.Blackjack.Interface.Options
(
getConfig
) where

import Jakway.Blackjack.AI
import Jakway.Blackjack.Util
import System.Console.GetOpt
import Data.Maybe (fromJust, catMaybes)
import System.Exit (die)
import Jakway.Blackjack.IO.TableNames
import qualified Jakway.Blackjack.Interface.Config as Conf

data Flag = Verbose |
            WhichDealer AI |
            NumBasicPlayer Int |
            NumGames Integer |
            TableNameSuffix String |
            PostgresqlConnectString String
            deriving (Show, Eq)


--extract the number of BasicPlayer or return Nothing
getNumBasicPlayer :: Flag -> Maybe Int
getNumBasicPlayer (NumBasicPlayer num) = Just num
getNumBasicPlayer _ = Nothing

getWhichDealer :: Flag -> Maybe AI
getWhichDealer (WhichDealer whichAI) = Just whichAI
getWhichDealer _ = Nothing

getNumGames :: Flag -> Maybe Integer
getNumGames (NumGames num) = Just num
getNumGames _ = Nothing

getPostgresqlConnectString :: Flag -> Maybe String
getPostgresqlConnectString (PostgresqlConnectString str) = Just str
getPostgresqlConnectString _ = Nothing


flagsToConfig :: [Flag] -> Either String Conf.Config
flagsToConfig [] = Left "No flags passed."
flagsToConfig flags
                --No dealer!
                | (whichDealer == Nothing) = Left "No dealer AI found, perhaps you passed a player AI by mistake?"
                | (numPlayerAIs == 0) = Left "Must have >0 players!"
                | numGames == Nothing = Left "Specify how many games to run."
                | suffixes == [] = Left "You must specify a table name suffix."
                | otherwise = Right $ Conf.Config hasVerbose (fromJust whichDealer) playerAIs (fromJust numGames) (getTableNames tableNameSuffix) pConnStr
            --Make sure a player AI hasn't been passed for a dealer AI
            -- TODO: rewrite using filter?
            where whichDealer = case (catMaybes $ map getWhichDealer flags) of [] -> Nothing
                                                                               [x] -> Just x
                  getNum f = length . catMaybes $ map f flags
                  --need at least 1 player besides the dealer
                  numBasicPlayerAIs = let n = catMaybes $ map getNumBasicPlayer flags
                                          in case n of [] -> 0
                                                       _ -> head n
                  numPlayerAIs = numBasicPlayerAIs -- TODO: modify as we add more AI types
                  suffixes = catMaybes $ map (\a -> case a of (TableNameSuffix suff) -> Just suff
                                                              _ -> Nothing) flags
                  numGames = case catMaybes (map getNumGames flags) of [] -> Nothing
                                                                       [x] -> Just x
                  hasVerbose = True `elem` (map (== Verbose) flags)
                  --build the list of player AIs from the passed flags
                  -- TODO: concat other ais as we add more types
                  playerAIs = replicate numBasicPlayerAIs BasicPlayer
                  tableNameSuffix = head suffixes

                  pConnStr :: Maybe String
                  pConnStr = extractFlags (getPostgresqlConnectString) flags


extractFlags :: (Flag -> Maybe b) -> [Flag] -> Maybe b
extractFlags f flags = case (catMaybes $ map f flags) of [] -> Nothing
                                                         [x] -> Just x


--flag converters
dealerOpt, ngOpt, nbpOpt :: String -> Flag
dealerOpt = WhichDealer . read
nbpOpt = NumBasicPlayer . read
ngOpt = NumGames . (read :: String -> Integer)

options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"] (NoArg Verbose)       "Be verbose."
    , Option []        ["with-dealer"]  (ReqArg dealerOpt "DealerAI")  "Which dealer AI to use."
    , Option []        ["num-BasicPlayer"]  (ReqArg nbpOpt  "NUM")  "Number of BasicPlayer AI's."
    , Option []        ["num-FiftyFiftyPlayer"]  (ReqArg nbpOpt  "NUM")  "Number of FiftyFiftyPlayer AI's."
    , Option ['g']     ["num-games"]  (ReqArg ngOpt "NUM") "Number of games to run."
    , Option ['s']     ["tablename-suffix"] (ReqArg TableNameSuffix "SUFFIX") "Table name suffix."
    , Option []        ["postgres-string"] (ReqArg PostgresqlConnectString "CONNSTR")  "Postgresql connection string (used instead of config file if both are present)"
    ]


parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: blackjack-simulator [OPTION...]"


--returns a valid configuration or prints an error and exits
getConfig :: [String] -> IO Conf.Config
getConfig argv = parseOptions argv >>= \(flags, _) -> return (flagsToConfig flags) >>=
                                \res -> case res of (Left x) -> die $ "Error processing options: " ++ x
                                                    (Right conf) -> return conf
