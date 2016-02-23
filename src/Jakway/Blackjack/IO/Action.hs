module Jakway.Blackjack.IO.Action where

import Jakway.Blackjack.Match
import Control.Either
import Database.HDBC (Statement)


-- ****************************************
-- if recursivePerformMatch doesn't work well as is,
-- instead of having it be recursive just have it try to perform ONE match
-- insert (returning IO (Either String Integer) but NOT taking a (Either String Integer) parameter)
-- write a helper function to call recursivePerformMatch repeatedly based
-- on whether or not it returns Left or Right
-- ****************************************

--TODO: write a function that wraps recursivePerformMatch by:
--1. calling it
--2. catching any exceptions
--3. writing the exception message to Left String


-- |Returns an IO action that yields either a string describing an error or
-- the number of games written
-- the generator passed by RandomGen is reused so that this function ought
-- to be deterministic (i.e. it does not get a new generator from IO)
--
--  + pass 0 for numGames when calling this function externally
-- ************************************************************************
-- TODO: any way to split this function up so we can be certain (from the
-- type signature) it doesn't get a new random generator but still have it
-- return an IO type?
recursivePerformMatch :: (IConnection a, RandomGen g) =>
            --state parameters
            Either String Integer -> 
            Config ->
            Integer ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a
            IO (Either String Integer)
recursivePerformMatch e conf numGames gen insHandStatement insMatchStatement conn =
        --numGames is the number of games we've done so far
        --if it's greater than or equal to maxGames, we're done
        case numGames of (>= maxGames) -> return e
                         _ -> 
        --if e == Left it short circuits and we stop updating the total
        --number of games
        e >>= (\totalGames ->
              let deck = infiniteShuffledDeck gen
                  maybeMatch = evalGame
                  in case maybeMatch of Nothing -> return $ Left (errorMessage thisMatch totalGames)
                                        Just (justMatch) -> do
                                            insRes <- insertMatch insHandStatement insMatchStatement conn tableNames justMatch
                                            --recurse with left to short
                                            --circuit
                                            let failedRecurse = recursivePerformMatch (Left (insertFailedMessage insRes)) (beVerbose, dealerAI, playerAIs, maxGames, suffix) numGames gen insHandStatement insMatchStatement conn
                                                successRecurse = recursivePerformMatch (Right (numGames + 1)) 
                                            if (insRes < 1) then failedRecurse
                                                            else successRecurse

              )
    where matchFailedMessage match num = "Match number " ++ (show num) ++ "failed!" ++ "\nMatch is:\n" ++ (show match)
          insertFailedMessage res = "Error inserting match, database returned: " ++ (show res)
          tableNames = getTableNames suffix
          nextRNG = snd . split
          (beVerbose, dealerAI, playerAIs, maxGames, suffix) = conf
