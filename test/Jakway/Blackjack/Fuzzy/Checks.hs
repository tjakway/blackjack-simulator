module Jakway.Blackjack.Fuzzy.Checks where

import qualified Statistics.Test.Types as Stats
import qualified Statistics.Test.ChiSquared as Stats
import qualified Data.Vector.Unboxed as U
import System.Random

--get stdgen -> newshuffled deck -> stdgen -> integer 

-- |returns the first random int (given by next) and the range of possible
-- values
rngOutput :: (RandomGen g) => g -> (Int, (Int, Int))
rngOutput gen = (fst . next $ gen, genRange gen)
 
-- |
--testSourceUnbiased :: 


-- |Takes as input a set of bins with the number of observations in each
-- returns the set of bins you would expect given an even distribution
-- across all bins zipped with the observed values
evenDistribution :: [Int] -> [(Int, Double)]
evenDistribution observed = zip observed expectedObservations
        where numBins = length observed
              totalObservations = fromIntegral . sum $ observed
              expectedObservations = replicate numBins (totalObservations / (fromIntegral numBins))
