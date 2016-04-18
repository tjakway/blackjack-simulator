module Jakway.Blackjack.Fuzzy.Checks where

import qualified Statistics.Test.Types as Stats
import qualified Statistics.Test.ChiSquared as Stats
import qualified Data.Vector.Unboxed as V
import System.Random
import Jakway.Blackjack.AI (deckToRNG)
import Jakway.Blackjack.CardOps 
import Data.Hashable

deck_value_range = 104 -- ^ 52 cards plus the visibility flag

-- |get stdgen -> newshuffled deck -> stdgen -> integer 
rngOutputWithRange :: (RandomGen g) => g -> (Int, (Int, Int))
rngOutputWithRange gen = let deck = infiniteShuffledDeck gen
                             rngFromDeck = deckToRNG deck
                         in (fst . next $ rngFromDeck, genRange rngFromDeck)


-- |same as above but ignore the range
rngOutput :: (RandomGen g) => g -> Int
rngOutput gen = fst $ rngOutputWithRange gen



-- |record a new observation, updating the vector in place
deckToObservation :: Vector Int -> Deck -> Vector Int
deckToObservation vec deck = modify (\v -> write v index newCount)
        where index = (`mod` deck_value_range) . hash $ deck
              newCount = (vec ! index) + 1

                     
testDeckRandomness :: Double -> Integer -> AI -> [AI] -> IO TestResult
testDeckRandomness pvalue numSamples dealerAI playerAIs = do
        
        let samplesVec = (fromList (replicate deck_value_range 0)) :: Vector Int

        observations <- foldM (\vec _ -> newDeckIO >>= (\d -> return $ evalGameKeepDeck dealerAI playerAIs d >>= \maybeDeck -> 
                                case maybeDeck of Nothing -> return vec
                                                (Just resDeck) -> return $ deckToObservation vec resDeck)) samplesVec [1..numSamples] 

        where newDeckIO = (liftM infiniteShuffledDeck) . getStdGen


testSourceUnbiased pvalue numSamples = undefined --getStdGen >>= getAdditionalDF
        where getAdditionalDF gen = (snd $ genRange gen) - (fst $ genRange gen)
        -- ^ DOUBLE CHECK THIS
        


-- |Takes as input a set of bins with the number of observations in each
-- returns the set of bins you would expect given an even distribution
-- across all bins zipped with the observed values
evenDistribution :: [Int] -> [(Int, Double)]
evenDistribution observed = zip observed expectedObservations
        where numBins = length observed
              totalObservations = fromIntegral . sum $ observed
              expectedObservations = replicate numBins (totalObservations / (fromIntegral numBins))
