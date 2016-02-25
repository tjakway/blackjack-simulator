{-# LANGUAGE ScopedTypeVariables #-}
module Jakway.Blackjack.IO.Action
(
transacPerformMatchIO,
performMatchIO
)
where

import Database.HDBC
import Jakway.Blackjack.Interface.Options
import Jakway.Blackjack.IO.TableNames
import Jakway.Blackjack.IO.DatabaseWrites
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Game
import System.Random

--collapse all of the individual transacPerformMatchIO calls into one huge IO (Either String Integer)
collapseMatches :: 
            Integer ->
            --state parameters
            Config ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (Either String Integer)
collapseMatches matches_per_transaction conf gen insHandStatement insMatchStatement conn = do
              let matchesPerTransaction = (numGames `div` matches_per_transaction) + (ceiling $ (numGames `mod` matches_per_transaction) `div` numGames)
                  (beVerbose, dealerAI, playerAIs, numGames, suffix) = conf
                  perTransactionConf = (beVerbose, dealerAI, playerAIs, matchesPerTransaction, suffix)
                  tableNames = getTableNames suffix

              --get the statements and the RNG
              (insHandStatement, insMatchStatement) <- getStatements conn tableNames
              initialGen <- getStdGen

              --discard the RNG
              (_, matchesRes) <- foldr (\_ ioRes -> ioRes >>= (\(mutatedGen, res) -> 
                                case res of (Left _) -> (mutatedGen, res)
                                            (Right ngames) -> transacPerformMatchIO perTransactionConf initialGen insHandStatement insMatchStatement conn)) (initialGen, Right 0) [1..(numGames `div` matches_per_transaction)]
             
              return matchesRes
        --get the hand and match insert statements
    where getStatements :: (IConnection a) => a -> TableNames -> IO (Statement, Statement)
          getStatements conn names = insertHandStatement conn names >>= 
                                        (\ihs -> insertMatchStatement conn names >>= 
                                            (\ims -> return (ihs, ims)))

transacPerformMatchIO :: (IConnection a, RandomGen g) =>
            --state parameters
            Config ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (g, Either String Integer)
--commit before starting the transaction so we don't rollback
--farther than we wanted
transacPerformMatchIO conf gen insHandStatement insMatchStatement conn = commit conn >> trans
                where trans =  withTransaction conn (\transacConn -> performMatchIO conf gen insHandStatement insMatchStatement transacConn)

-- |The public interface to recursivePerformMatch
performMatchIO :: (IConnection a, RandomGen g) =>
            --state parameters
            Config ->
            g ->
            --IO parameters
            Statement ->
            Statement -> 
            a ->
            IO (g, Either String Integer)
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
            IO (g, Either String Integer)
recursivePerformMatch numGames conf gen insHandStatement insMatchStatement conn = 
        let (_, _, _, maxGames, _) = conf
            nextRNG = snd . split $ gen
         in performMatch numGames conf gen insHandStatement insMatchStatement conn >>= (\res ->
            case res of (Right newNumGames) -> if (newNumGames < maxGames) then recursivePerformMatch (newNumGames) conf nextRNG insHandStatement insMatchStatement conn else return (nextRNG, Right newNumGames)
                        Left _ -> return (nextRNG, res))

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
        let (_, dealerAI, playerAIs, _, suffix) = conf
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
          
