module Jakway.Blackjack.Tests.DatabaseTests.Common where

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
--for why hiding catch is a good idea
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error hiding (catch)
import System.Directory
import Database.HDBC.Session (withConnectionIO')
import Database.HDBC.Sqlite3

--see http://stackoverflow.com/questions/8502201/remove-file-if-it-exists
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

withDatabase name = withConnectionIO' (connectSqlite3 name) 

-- |run a transaction on a database that will be deleted before and after running it
withTempDatabase transaction dbName = removeIfExists dbName >> withDatabase dbName transaction >> removeIfExists dbName
