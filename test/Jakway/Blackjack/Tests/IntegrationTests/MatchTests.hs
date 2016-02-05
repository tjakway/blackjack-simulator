module Jakway.Blackjack.Tests.IntegrationTests.MatchTests (tests) where

import Jakway.Blackjack.Tests.GameTests (test_1v1_game)
import Jakway.Blackjack.IO.DatabaseCommon
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.IO.DatabaseReads
import Jakway.Blackjack.Match
import Jakway.Blackjack.Tests.DatabaseTests.Common
import Data.Maybe (fromJust)
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Database.HDBC

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
        rMatch <- readMatch rMatchStatement rHandStatement gameID
        assertBool "readMatch failed" (rMatch /= Nothing)
        assertBool "Match database error" ((fromJust rMatch) == test_1v1_game)


tests = testGroup "IntegrationTests" [testCase "testReadWrite1v1" testReadWrite1v1]
