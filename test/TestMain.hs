module Main where

import qualified Jakway.Blackjack.Tests.DatabaseTests as DatabaseTests
import Test.HUnit (runTestTT)

main :: IO ()
main = do 
          runTestTT DatabaseTests.tests
          return ()
