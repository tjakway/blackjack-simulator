module Jakway.Blackjack.Tests.IntegrationTests.MatchTests (tests) where

import Jakway.Blackjack.Tests.GameTests (test_1v1_game)
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseReads
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Match
import Jakway.Blackjack.Game
import Jakway.Blackjack.AI
import Jakway.Blackjack.Tests.DatabaseTests.Common
import Data.Maybe (fromJust, isJust)
import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit
import Database.HDBC
import System.Random
import Control.Monad (liftM)
import Control.Monad.State
import Control.Monad.Random

--run a very simple match, write it to the database, and read it back
testReadWrite1v1 :: Assertion
testReadWrite1v1 = withSingleTableTestDatabase $ \conn -> do
        let origDealersHand = dealersHand test_1v1_game
            origPlayersHand = playersHands test_1v1_game
            origRes = playerResults test_1v1_game
            playerID = playerIds test_1v1_game
        --there's only one player and player 0 is the dealer

        insertPlayers conn basicTestTableNames 2
        insMatchStatement <- insertMatchStatement conn basicTestTableNames
        insHandStatement <- insertHandStatement conn basicTestTableNames
        gameID <- insertMatch insMatchStatement insHandStatement conn basicTestTableNames (Match origDealersHand playerID origPlayersHand origRes)
        commit conn

        rMatchStatement <- readMatchStatement conn basicTestTableNames
        rHandStatement <- readHandStatement conn basicTestTableNames
        commit conn
        rMatch <- readMatch rMatchStatement rHandStatement gameID
        commit conn
        assertBool "readMatch failed" (isJust rMatch)
        assertBool "Match database error" (fromJust rMatch == test_1v1_game)


testReadWriteRandomMatches :: Assertion
testReadWriteRandomMatches = withSingleTableTestDatabase $ \conn -> do
    (numPlayers, numMatches) <- (evalRandIO genRandVariables) :: IO (Integer, Integer)
    insertPlayers conn basicTestTableNames (fromInteger numPlayers)

    let playerAIs = replicate (fromInteger numPlayers) BasicPlayer
    matchesToTest <- mapM (\_ -> genMatch BasicDealer playerAIs) [1..numMatches]

    mapM_ (testReadWriteMatch conn basicTestTableNames) matchesToTest

    where maxRandPlayers = 10 :: Integer
          minRandMatches = 10 :: Integer
          maxRandMatches = 100 :: Integer
          --use MonadRandom to generate parameters
          --MonadRandom is basically the state monad specialized to StdGen
          genRandVariables = do
                np <- getRandomR (1, maxRandPlayers) --minimum 1 other player
                nm <- getRandomR (minRandMatches, maxRandMatches)
                return (np, nm)
                


-- |for simplicity this function creates all the needed statements from the
-- passed connection
testReadWriteMatch :: (IConnection a) => a -> TableNames -> Match -> Assertion
testReadWriteMatch conn tableNames match = do
        --create all the statements
        insMatchStatement <- insertMatchStatement conn tableNames
        insHandStatement <- insertHandStatement conn tableNames
        rMatchStatement <- readMatchStatement conn tableNames
        rHandStatement <- readHandStatement conn tableNames

        --insert the match
        gameID <- insertMatch insMatchStatement insHandStatement conn basicTestTableNames match
        commit conn

        --read it back
        rMatch <- readMatch rMatchStatement rHandStatement gameID
        commit conn

        --check that it matches
        assertBool "readMatch failed (return Nothing)" (isJust rMatch)
        let testPassed = fromJust rMatch == match
        when (testPassed == False) (putStrLn "Error in testReadWriteMatch." >> putStrLn ("READ MATCH: " ++ (show . fromJust $ rMatch)) >> putStrLn ("EXPECTED MATCH: " ++ (show match)))
        let dbErrorMessage = "Match database error"
        assertBool dbErrorMessage (fromJust rMatch == match)

tests = testGroup "IntegrationTests" [testCase "testReadWrite1v1" testReadWrite1v1, testCase "testReadWriteRandomMatches" testReadWriteRandomMatches]
