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


newDeckIO :: IO Deck
newDeckIO = getStdGen >>= return . infiniteShuffledDeck

observeDeck :: Integer -> AI -> [AI] -> IO (U.Vector Int)
observeDeck numSamples dealerAI playerAIs =
        foldM (\vec _ -> newDeckIO >>= (\d -> (return $ evalGameKeepDeck dealerAI playerAIs d) >>= (\maybeDeck -> 
                                case maybeDeck of Nothing -> return vec
                                                  -- | modify the list of observations and return it
                                                  (Just (resDeck,_)) -> return $ deckToObservation vec resDeck))) samplesVec [1..numSamples] 

        where samplesVec = (U.fromList (replicate deck_value_range 0)) :: U.Vector Int

                     
testDeckEvenDistribution :: Double -> Integer -> AI -> [AI] -> IO Stats.TestResult
testDeckEvenDistribution pvalue numSamples dealerAI playerAIs = do
        observations <- observeDeck numSamples dealerAI playerAIs
        return $ Stats.chi2test pvalue additionalDF (evenDistribution observations)
        where additionalDF = 0

testRNGDistribution :: Double -> Integer -> Integer -> Integer -> AI -> [AI] -> IO Stats.TestResult
testRNGDistribution pvalue numSamples numRNGSamples rngMaxRange dealerAI playerAIs = getStdGen >>= (\gen -> observeDeck numSamples dealerAI playerAIs >>= \samples -> return $ Stats.chi2test pvalue 0 $ rngDistribution numRNGSamples rngMaxRange gen samples)

--instead of testing against an even distribution, test against the
--distribution of the default StdGen

rngDistribution :: (RandomGen g) => Integer -> Integer -> g -> U.Vector Int -> U.Vector (Int, Double)
rngDistribution numRngObservations maxr gen observed = U.zip observed percents
        where -- how many observations to get the standard RNG
              -- distribution?
              startingVec = (U.fromList (genericReplicate maxr 0)) :: U.Vector Int
              rngObservations = genericTake numRngObservations $ randomRs (0, maxr) gen -- ^ lower bound on random numbers is 0
              -- |increment the count for this observation
              rngObservationCounts = foldr (\thisObservation vec -> vecIncrement vec thisObservation) startingVec $ map fromIntegral rngObservations
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
