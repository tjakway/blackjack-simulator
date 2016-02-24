module Jakway.Blackjack.IO.Action
(performMatchIO)
where

import Jakway.Blackjack.Match
import Data.Either
import Database.HDBC
import Jakway.Blackjack.Interface.Options
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Game
import System.Random



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


performMatchIO :: (IConnection a, RandomGen g) =>
            --state parameters
            Config ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (Either String Integer)
performMatchIO = recursivePerformMatch 0


recursivePerformMatch :: (IConnection a, RandomGen g) =>
            --state parameters
            Integer -> 
            Config ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (Either String Integer)
recursivePerformMatch numGames conf gen insHandStatement insMatchStatement conn = 
        let (beVerbose, dealerAI, playerAIs, maxGames, suffix) = conf
         in performMatch numGames conf gen insHandStatement insMatchStatement conn >>= (\res ->
            case res of (Right newNumGames) -> if (newNumGames < maxGames) then recursivePerformMatch (newNumGames) conf nextRNG insHandStatement insMatchStatement conn else return (Right newNumGames)
                        Left _ -> return res)
        where nextRNG = snd . split $ gen

-- |Returns an IO action that yields either a string describing an error or
-- the number of games written
-- the generator passed by RandomGen is reused so that this function ought
-- to be deterministic (i.e. it does not get a new generator from IO)
--
--  + pass 0 for numGames when calling this function externally
performMatch :: (IConnection a, RandomGen g) =>
            --state parameters
            Integer -> 
            Config ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (Either String Integer)
performMatch numGames conf gen insHandStatement insMatchStatement conn =
        let (beVerbose, dealerAI, playerAIs, maxGames, suffix) = conf
            tableNames = getTableNames suffix
        --if e == Left it short circuits and we stop updating the total
        --number of games
            deck = infiniteShuffledDeck gen
            maybeMatch = evalGame dealerAI playerAIs deck
        in case maybeMatch of Nothing -> return $ Left (matchFailedMessage numGames)
                              Just (justMatch) -> do
                                insRes <- insertMatch insHandStatement insMatchStatement conn tableNames justMatch
                                --recurse with left to short
                                --circuit
                                if (insRes < 1) then return (Left $ insertFailedMessage numGames)
                                                else return (Right $ numGames + 1)
                                                            
    where matchFailedMessage num = "Match number " ++ (show num) ++ "failed!"          
          insertFailedMessage res = "Error inserting match, database returned: " ++ (show res)
          
