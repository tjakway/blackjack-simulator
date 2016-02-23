module Jakway.Blackjack.IO.Action where

import Jakway.Blackjack.Match
import Control.Either
import Database.HDBC (Statement)

--TODO: write a function that wraps recursivePerformMatch by:
--1. calling it
--2. catching any exceptions
--3. writing the exception message to Left String


-- |Returns an IO action that yields either a string describing an error or
-- the number of games written
-- the generator passed by RandomGen is reused so that this function ought
-- to be deterministic (i.e. it does not get a new generator from IO)
-- TODO: any way to split this function up so we can be certain (from the
-- type signature) it doesn't get a new random generator but still have it
-- return an IO type?
recursivePerformMatch :: (IConnection a, RandomGen g) =>
            Config ->
            --state parameters
            Either String Integer -> 
            Integer ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a
            IO (Either String Integer)
recursivePerformMatch (beVerbose, dealerAI, playerAIs, numGames, suffix) e maxGames gen insHandStatement insMatchStatement conn =
        --if e == Left it short circuits and we stop updating the total
        --number of games
        e >>= (\totalGames ->
              let deck = infiniteShuffledDeck gen
                  maybeMatch = evalGame
                  in case maybeMatch of Nothing -> return $ Left (errorMessage thisMatch totalGames)
                                        Just (justMatch) -> do
                                            insRes <- insertMatch insHandStatement insMatchStatement conn tableNames justMatch


              )
    where errorMessage match num = "Match number " ++ (show num) ++ "failed!" ++ "\nMatch is:\n" ++ (show match)
          tableNames = getTableNames suffix
