module Jakway.Blackjack.Fuzzy.Checks
(
testDeckRandomness
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
import Control.Monad (liftM, foldM)
import Data.List (foldl')

deck_value_range = 104 -- ^ 52 cards plus the visibility flag

-- |record a new observation, updating the U.Vector in place
deckToObservation :: U.Vector Int -> Deck -> U.Vector Int
deckToObservation vec deck = U.modify (\v -> UM.write v index newCount) vec
        where vecRange = U.length vec
              index = (`mod` vecRange) . hash $ deck
              newCount = (vec U.! index) + 1

                     
testDeckRandomness :: Double -> Integer -> AI -> [AI] -> IO Stats.TestResult
testDeckRandomness pvalue numSamples dealerAI playerAIs = do
        
        let samplesVec = (U.fromList (replicate deck_value_range 0)) :: U.Vector Int

        observations <- sequence $ foldl' (\vec _ -> newDeckIO >>= (\d -> (return $ evalGameKeepDeck dealerAI playerAIs d) >>= (\maybeDeck -> 
                                case maybeDeck of Nothing -> return vec
                                                  -- | modify the list of observations and return it
                                                  (Just (resDeck,_)) -> return $ deckToObservation vec resDeck))) samplesVec [1..numSamples] 

        let additionalDF = (U.length observations) - 1

        
        return $ Stats.chi2test pvalue additionalDF (evenDistribution observations)

        where newDeckIO = getStdGen >>= return . infiniteShuffledDeck


-- |Takes as input a set of bins with the number of observations in each
-- returns the set of bins you would expect given an even distribution
-- across all bins zipped with the observed values
evenDistribution :: U.Vector Int -> U.Vector (Int, Double)
evenDistribution observed = U.zip observed expectedObservations
        where numBins = U.length observed
              totalObservations = fromIntegral . U.sum $ observed
              expectedObservations = U.fromList $ replicate numBins (totalObservations / (fromIntegral numBins))
