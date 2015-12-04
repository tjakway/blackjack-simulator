module Main where

import qualified Jakway.Blackjack.Tests.DatabaseTests as DatabaseTests
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMainWithOpts 
            DatabaseTests.tests 
            mempty
