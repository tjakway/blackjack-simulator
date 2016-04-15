module Jakway.Blackjack.Fuzzy.Checks where

import qualified Statistics.Test.Types as Stats
import qualified Statistics.Test.ChiSquared as Stats
import qualified Data.Vector.Unboxed as U
import System.Random
import Jakway.Blackjack.CardOps (infiniteShuffledDeck)

-- |get stdgen -> newshuffled deck -> stdgen -> integer 
rngOutputWithRange :: (RandomGen g) => g -> (Int, (Int, Int))
rngOutputWithRange gen = let deck = infiniteShuffledDeck gen
                    rngFromDeck = deckToRNG deck
                    in (fst . next $ rngFromDeck, genRange rngFromDeck)

-- |same as above but ignore the range
rngOutput :: (RandomGen g) => g -> Int
rngOutput = fst rngOutputWithRange
                     

testSourceUnbiased pvalue numSamples = undefined
        additionalDF = (snd genRange gen) - (fst genRange gen)
        -- ^ DOUBLE CHECK THIS
        


-- |Takes as input a set of bins with the number of observations in each
-- returns the set of bins you would expect given an even distribution
-- across all bins zipped with the observed values
evenDistribution :: [Int] -> [(Int, Double)]
evenDistribution observed = zip observed expectedObservations
        where numBins = length observed
              totalObservations = fromIntegral . sum $ observed
              expectedObservations = replicate numBins (totalObservations / (fromIntegral numBins))
