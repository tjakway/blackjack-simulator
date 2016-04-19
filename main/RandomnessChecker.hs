module Main where

import System.Environment (getArgs)
import qualified Statistics.Test.Types as Stats
import Jakway.Blackjack.Random.Checks
import Jakway.Blackjack.Random.Options
import Jakway.Blackjack.Util (die)     -- ^ die is implemented here for compatibility with older GHC versions
import Jakway.Blackjack.AI

main :: IO ()
main = do
    args <- getArgs
    conf <- getConfig args

    let distrib = distribution conf
        pval = pvalue conf
        n = sampleSize conf
        r = rngSampleSize conf
        dealerAI = BasicDealer
        playerAIs = [BasicPlayer]
    
    if (n <= 104) then die "Samples must be >104." else return ()

    case distrib of EvenDistribution -> testDeckEvenDistribution pval n dealerAI playerAIs >>= printResult
                    RNGDistribution -> testRNGDistribution pval n r dealerAI playerAIs >>= printResult
                    _ -> die "Fatal error, unknown test!"


printResult :: Stats.TestResult -> IO ()
printResult (Stats.Significant) = putStrLn $ "The test result is: Significant.  There is evidence to conclude that the deck IS NOT an unbiased source of randomness."
printResult (Stats.NotSignificant) = putStrLn $ "The test result is: Not Significant.  There is insufficient evidence to conclude that the deck is not an unbiased source of randomness."

