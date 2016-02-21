module Jakway.Blackjack.Interface.Options
(
Flag(..),
processOptions
) where

import Jakway.Blackjack.AI
import System.Console.GetOpt
import Data.Maybe (fromJust, catMaybes)

data Flag = Verbose |
            WhichDealer AI |
            NumBasicPlayer Int |
            NumGames Integer
            deriving Show

type Config = (Bool, AI, [AI], Integer)

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

flagsToConfig :: [Flag] -> Either String Config
flagsToConfig [] = Left "No flags passed."
flagsToConfig flags
                --No dealer!
                | (whichDealer == Nothing) = Left "No dealer AI found, perhaps you passed a player AI by mistake?"
                | (numPlayerAIs == 0) = Left "Must have >0 players!"
                | numGames == Nothing = Left "Specify how many games to run."
                | otherwise = Right (hasVerbose, (fromJust whichDealer), playerAIs, fromJust numGames)
            --Make sure a player AI hasn't been passed for a dealer AI
            -- TODO: rewrite using filter?
            where whichDealer = case (catMaybes $ map getWhichDealer flags) of [] -> Nothing
                                                                               [x] -> Just x
                  getNum f = length . catMaybes $ map f flags
                  --need at least 1 player besides the dealer
                  numBasicPlayerAIs = getNum getNumBasicPlayer
                  numPlayerAIs = numBasicPlayerAIs -- TODO: modify as we add more AI types
                  numGames = case catMaybes (map getNumGames flags) of [] -> Nothing
                                                                       [x] -> Just x
                  hasVerbose = (map (== Verbose) flags) `elem` True
                  --build the list of player AIs from the passed flags
                  -- TODO: concat other ais as we add more types
                  playerAIs = replicate numBasicPlayerAIs BasicPlayer
                  

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
    , Option ['g']     ["num-games"]  (ReqArg ngOpt "NUM") "Number of games to run."
    ]


processOptions :: [String] -> IO ([Flag], [String])
processOptions argv = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: blackjack-simulator [OPTION...]"
