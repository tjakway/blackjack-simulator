module Main where

import System.Environment (getArgs)
import qualified Statistics.Test.Types as Stats
import Jakway.Blackjack.Fuzzy.Checks
import Jakway.Blackjack.Util (die)     -- ^ die is implemented here for compatibility with older GHC versions
import Jakway.Blackjack.AI

main :: IO ()
main = do
    args <- getArgs

    if (length args) /= numArgs then die usage 
                                else return ()

    
    let pvalue = read (args !! 0)
        samples = read (args !! 1)
        dealerAI = BasicDealer
        playerAIs = [BasicPlayer]
    
    --for now just run it with a very basic setup
    testDeckRandomness pvalue samples dealerAI playerAIs >>= printResult

    where usage = "Usage: [pvalue] [samples]"
          numArgs = 2       -- ^ doesn't include the program's name

printResult :: Stats.TestResult -> IO ()
printResult (Stats.Significant) = putStrLn $ "The test result is: Significant.  There is evidence to conclude that the deck IS NOT an unbiased source of randomness."
printResult (Stats.NotSignificant) = putStrLn $ "The test result is: Not Significant.  There is insufficient evidence to conclude that the deck is not an unbiased source of randomness."

