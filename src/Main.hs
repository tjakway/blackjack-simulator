module Main where

import System.Random
import Jakway.Blackjack.AI
import Jakway.Blackjack.Game
import Jakway.Blackjack.Cards
import Jakway.Blackjack.Result
import Jakway.Blackjack.Visibility

-- |no need to create the empty list of starting records--it'll be
-- generated automatically
foldOverResults :: [ScoreRecord] -> [Result] -> [ScoreRecord]
foldOverResults [] results = foldOverResults (flip . replicate mempty . length $ results) results 
foldOverResults startingRecords results = map (\(thisRecord, thisScore) -> thisRecord `addResult` thisScore) zippedArgs
                    where zippedArgs = zip startingRecords results
                          -- ^ TODO: come up with a better name...

runNGamesNPlayers :: RandomGen a => Int -> Int -> a -> Maybe (ScoreRecord, [ScoreRecord])
runNGamesNPlayers numGames numPlayers gen = let players = replicate numPlayers BasicPlayer 
                                                dealer = BasicDealer
                                                gameResults = map (playGame dealer players (infiniteShuffledDeck gen)) [1..numGames]
                                                -- |tally the scores
                                                --http://stackoverflow.com/questions/9448570/using-map-with-two-lists-rather-than-one-can-you-nest
                                                scores = foldr (\(tallyDealerScore, tallyPlayerScores) res -> res >>= \(prevDealerScore, prevPlayerScores) -> return (tallyDealerScore `mconcat` prevDealerScore, foldOverResults prevPlayerScores))
                                                startingValues = (mempty, replicate numPlayers mempty)
                                                    in scores gameResults startingValues



main :: IO ()
main = putStrLn "Hello, Haskell!"
