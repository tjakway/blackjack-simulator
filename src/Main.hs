module Main where

import System.Random
import Data.Maybe (fromJust)
import Cards

-- |no need to create the empty list of starting records--it'll be
-- generated automatically
foldOverResults :: [ScoreRecord] -> [Result] -> [ScoreRecord]
foldOverResults [] results = foldOverResults (flip replicate mempty . length $ results) results 
foldOverResults startingRecords [] = startingRecords
foldOverResults startingRecords results = map (\(thisRecord, thisScore) -> thisRecord `addResult` thisScore) zippedArgs
                    where zippedArgs = zip startingRecords results
                          -- ^ TODO: come up with a better name...

sumScores :: (ScoreRecord, [ScoreRecord]) -> [Maybe (ScoreRecord, [Result])] -> (ScoreRecord, [ScoreRecord])
sumScores startingValues allGameResults = case summedRecords of Nothing -> error "Should never reach here!" 
                                                                _ -> fromJust summedRecords
        where summedRecords = foldr (\res (tallyDealerScore, tallyPlayerScores) -> res >>= (\(prevDealerScore, prevPlayerScores) -> return (tallyDealerScore `mconcat` prevDealerScore, foldOverResults tallyPlayerScores prevPlayerScores ))) startingValues allGameResults


playNGamesNPlayers :: RandomGen a => Int -> Int -> a -> Maybe (ScoreRecord, [ScoreRecord])
playNGamesNPlayers numGames numPlayers gen = let players = replicate numPlayers BasicPlayer 
                                                 dealer = BasicDealer
                                                 -- |XXX: There should be
                                                 -- a better way to do
                                                 -- this--mapping over
                                                 -- a list of ints that are
                                                 -- ignored is basically
                                                 -- just a loop
                                                 gameResults = map (\_ -> playGame dealer players (infiniteShuffledDeck gen)) [1..numGames]
                                                 startingValues = (mempty, replicate numPlayers mempty) :: (ScoreRecord, [ScoreRecord]) 
                                                    in sumScores startingValues gameResults



main :: IO ()
main = putStrLn "Hello, Haskell!"
