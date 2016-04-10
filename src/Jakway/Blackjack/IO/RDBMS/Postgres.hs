--This module should only be accessed by Jakway.Blackjack.IO.DatabaseCommon
--Jakway.Blackjack.IO.DatabaseCommon exports the correct createTables
--function based on flags to `cabal configure`
module Jakway.Blackjack.IO.RDBMS.Postgres
(
initialize
)
where

import Jakway.Blackjack.Util
import Jakway.Blackjack.IO.TableNames
import Database.HDBC

createTables :: IConnection a => a -> IO ()
createTables conn = initializeSqlQuery >>= flipInner2 run conn [] >> return ()

loadFunctions :: IConnection a => a -> IO ()
loadFunctions conn = functionsSqlQuery >>= flipInner2 run conn [] >> return ()

-- |set up the DB
initialize :: IConnection a => a -> IO ()
initialize = createTables >> loadFunctions
