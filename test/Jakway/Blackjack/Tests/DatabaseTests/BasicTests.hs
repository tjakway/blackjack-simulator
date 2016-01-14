module Jakway.Blackjack.Tests.DatabaseTests.BasicTests (tests) where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import Database.HDBC
import Database.HDBC.Sqlite3
import Database.HDBC.Session (withConnectionIO')
import Test.HUnit
import Control.Monad (liftM, unless)
-- |overlapping for convenience--change if necessary
import qualified Jakway.Blackjack.IO.DatabaseWrites as DB
import qualified Jakway.Blackjack.IO.DatabaseReads as DB
import qualified Jakway.Blackjack.IO.DatabaseCommon as DB
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Control.Monad (when)
import Data.Maybe (fromJust)
import Jakway.Blackjack.Visibility
import Data.List (sort, delete)
import Test.Framework
import Test.Framework.Providers.HUnit
import System.Random
import Control.Monad.State
import Jakway.Blackjack.Game
import Data.Monoid (mempty)

test_db_name = "tmp_test.db"

testTableNames = DB.getTableNames "test1"

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase name = withConnectionIO' (connectSqlite3 name) 

-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase transaction dbName = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName

-- |initialize the database then run the transaction
-- don't forget to commit!
withTestDatabase transaction = withTempDatabase (\conn -> DB.enableForeignKeys conn >> DB.initializeDatabase conn [testTableNames] >> DB.insertAllCards conn >> commit conn >> transaction conn) test_db_name


testOpenDatabase :: Assertion
testOpenDatabase = withTestDatabase $ (\_ -> do
                                exists <- doesFileExist test_db_name
                                if exists
                                    then return ()
                                    else assertFailure message)
                    where message = "Database "++test_db_name++" does not exist!"

testTableList :: Assertion
testTableList =  withTestDatabase $ \conn -> getTables conn >>= (\tables -> unless (tablesEqual tables) (assertFailure $ message tables))
                where tables = ["cards", DB.getPlayerTableName testTableNames, DB.getHandTableName testTableNames, DB.getMatchTableName testTableNames]
                      -- | in case sqlite adds an extra schema table
                      tablesEqual readTables = (sort tables) == (sort . (delete "sqlite_sequence") $ readTables)
                      message readTables = "Database tables don't match!  Read tables: " ++ (show readTables)



testInsertPlayers :: Assertion
testInsertPlayers = withTestDatabase $ \conn -> do
                        let numPlayers = 10
                        DB.insertPlayers conn testTableNames numPlayers
                        commit conn
                        numPlayerRows <- DB.getNumPlayers conn testTableNames
                        let message = "numPlayerRows is "++(show numPlayerRows)++" (should be"++(show numPlayers)++")"
                        assertBool message (numPlayers == numPlayerRows) 

testInsertOneHand :: Assertion
testInsertOneHand = withTestDatabase $ \conn -> do
       let whichPlayer = 1
       DB.insertPlayers conn testTableNames 2
       let hand = [Hidden (Card Spade Ace), Shown (Card Diamond King)] 
       insertStatement <- DB.insertHandStatement conn testTableNames
       whichHand <- DB.insertHand insertStatement conn testTableNames whichPlayer hand
       commit conn

       readStatement <- DB.readHandStatement conn testTableNames
       res <- DB.readHand readStatement whichHand
       case res of Nothing -> assertFailure "Could not read hand id!"
                   Just resHand -> assertEqual "" hand resHand

testInsertRandStartingHands :: Assertion
testInsertRandStartingHands = withTestDatabase $ \conn -> do
       let whichPlayer = 0
       DB.insertPlayers conn testTableNames 2
       commit conn

       --insert between 10 and 100 hands
       numHands <- randomRIO (10, 100)
       gen <- getStdGen
       let startingDeck = infiniteShuffledDeck gen
       
        --don't need the state monad here--use a fold to keep track of state
       let hands= fst $ foldr (\_ (accumulatedHands, thisDeck) -> let (drawnHand, resDeck) = runState startingHand thisDeck
               in (drawnHand : accumulatedHands, resDeck)) (mempty, startingDeck) ([1..numHands] :: [Int])
               -- ^ start with an empty list of hands

       testInsertHands conn whichPlayer hands
       return ()

testInsertThreeCardHand :: Assertion
testInsertThreeCardHand = withTestDatabase $ \conn -> do
       let whichPlayer = 0
       DB.insertPlayers conn testTableNames 2
       commit conn

       gen <- getStdGen
       let deck = infiniteShuffledDeck gen
       let hand = return $ flip evalState deck $ do
           firstCard <- drawCard
           secondCard <- drawCard
           thirdCard <- drawCard
           return [Hidden firstCard, Shown secondCard, Hidden thirdCard]

       testInsertHands conn 1 hand
    

testInsertHands :: (IConnection a) => a -> Int -> [Hand] -> Assertion
testInsertHands conn whichPlayer hands
                            | whichPlayer < 0 = assertFailure ("Invalid player id: "++ (show whichPlayer))
                            | hands == [] = assertFailure "Attempted to insert empty list of hands"
                            | otherwise = do
                                -- |insert the ids then read them back from
                                -- the DB
                                insertStatement <- DB.insertHandStatement conn testTableNames
                                --player ids don't really matter here
                                let playerIds = replicate (length hands) whichPlayer
                                handIds <- DB.insertHands insertStatement conn testTableNames (playerIds, hands)
                                commit conn

                                readStatement <- DB.readHandStatement conn testTableNames
                                commit conn
                                res <- sequence $ map (DB.readHand readStatement) handIds
                                --make sure there weren't any problems
                                when (res `elem` Nothing) $ assertFailure "failed to insert a hand!"

                                --unwrap the maybes
                                let readHands = map fromJust res
                                assertBool "Ought to have read the same hands we inserted into the database" ((sort hands) == (sort readHands))


                                

tests =  testGroup "BasicTests" [testCase "testOpenDatabase" testOpenDatabase, testCase "testTableList" testTableList, testCase "testInsertPlayers" testInsertPlayers, testCase "testInsertOneHand" testInsertOneHand, testCase "testInsertRandStartingHands" testInsertRandStartingHands, testCase "testInsertThreeCardHand" testInsertThreeCardHand]
