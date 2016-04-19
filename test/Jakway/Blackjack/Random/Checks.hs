{-# LANGUAGE ScopedTypeVariables #-}

module Jakway.Blackjack.Random.Checks
(
testDeckEvenDistribution,
testRNGDistribution
)
where

import qualified Statistics.Test.Types as Stats
import qualified Statistics.Test.ChiSquared as Stats
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random
import Jakway.Blackjack.AI
import Jakway.Blackjack.CardOps 
import Jakway.Blackjack.Game
import Data.Hashable
import Control.Monad (foldM)
import Data.List (genericReplicate, genericTake)

deck_value_range :: Int
deck_value_range = 104 -- ^ 52 cards plus the visibility flag

-- |record a new observation, updating the U.Vector in place
deckToObservation :: U.Vector Int -> Deck -> U.Vector Int
deckToObservation vec deck = U.modify (\v -> UM.write v index newCount) vec
        where vecRange = U.length vec
              index = (`mod` vecRange) . hash $ (deck !! 13) -- ^ we've arbitrarily chosen the position to take a card from
              newCount = (vec U.! index) + 1

vecIncrement :: U.Vector Int -> Int -> U.Vector Int
vecIncrement v pos = U.modify (\mv -> UM.write mv pos newCount) v
        where newCount = (v U.! pos) + 1

                     
testDeckEvenDistribution :: Double -> Integer -> AI -> [AI] -> IO Stats.TestResult
testDeckEvenDistribution pvalue numSamples dealerAI playerAIs = do
        
        let samplesVec = (U.fromList (replicate deck_value_range 0)) :: U.Vector Int

        observations <- foldM (\vec _ -> newDeckIO >>= (\d -> (return $ evalGameKeepDeck dealerAI playerAIs d) >>= (\maybeDeck -> 
                                case maybeDeck of Nothing -> return vec
                                                  -- | modify the list of observations and return it
                                                  (Just (resDeck,_)) -> return $ deckToObservation vec resDeck))) samplesVec [1..numSamples] 

        let additionalDF = 0

        
        return $ Stats.chi2test pvalue additionalDF (evenDistribution observations)

        where newDeckIO = getStdGen >>= return . infiniteShuffledDeck

testRNGDistribution :: Double -> Integer -> AI -> [AI] -> IO Stats.TestResult
testRNGDistribution = undefined

--instead of testing against an even distribution, test against the
--distribution of the default StdGen

rngDistribution :: (RandomGen g) => g -> Integer -> U.Vector Int -> U.Vector (Int, Double)
rngDistribution gen numRngObservations observed = U.zip observed percents
        where numBins = snd $ genRange gen
              -- how many observations to get the standard RNG
              -- distribution?
              startingVec = (U.fromList (genericReplicate numRngObservations 0)) :: U.Vector Int
              rngObservations = genericTake numRngObservations $ randoms gen
              -- |increment the count for this observation
              rngObservationCounts = foldr (\thisObservation vec -> vecIncrement vec thisObservation) startingVec rngObservations
              -- |the distribution as a percent for each bin (i.e. a list
              -- of doubles)
              percents :: U.Vector Double
              percents = U.map (\o -> (fromIntegral o) / (fromIntegral numRngObservations)) rngObservationCounts


-- |Takes as input a set of bins with the number of observations in each
-- returns the set of bins you would expect given an even distribution
-- across all bins zipped with the observed values
evenDistribution :: U.Vector Int -> U.Vector (Int, Double)
evenDistribution observed = U.zip observed expectedObservations
        where numBins = U.length observed
              totalObservations = fromIntegral . U.sum $ observed
              expectedObservations = U.fromList $ replicate numBins (totalObservations / (fromIntegral numBins))
