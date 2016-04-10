{-# LANGUAGE ExistentialQuantification #-}
module Jakway.Blackjack.Util
(
innerMapTuple4,
innerMapTuple3,
flipInner2,
replaceElem,
default_replacement_character,
ssub_wrep_char,
ssub,
die,
initializeSqlQuery,
functionsSqlQuery
)
where

import Data.List (elemIndex)
import System.IO
import System.Exit (exitFailure)
import qualified Paths_blackjack_simulator as Paths

innerMapTuple4 :: forall t t1. (t -> t1) -> (t, t, t, t) -> (t1, t1, t1, t1)
innerMapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

innerMapTuple3 :: forall t t1. (t -> t1) -> (t, t, t) -> (t1, t1, t1)
innerMapTuple3 f (a,b,c) = (f a, f b, f c)

flipInner2 :: (a -> b -> c -> d) -> a -> c -> b -> d
flipInner2 f x y z = f x z y

--insert the 2nd string into the first, replacing the character at the
--passed position
replaceElem :: Int -> String -> String -> String
replaceElem pos orig ins = let (front, back) = splitAt pos orig
                               in front ++ ins ++ (tail back)

default_replacement_character :: Char
default_replacement_character = '?'

-- |"static substitution with replacement character"
-- it's an error to have `length orig` be less than the number of
-- replacement strings
ssub_wrep_char :: Char -> String -> [String] -> String
ssub_wrep_char rep_char orig [] = if (rep_char `elem` orig) == False then orig else error $ "Not enough replacement strings in string " ++ orig
ssub_wrep_char rep_char orig (x:xs) = case xIndex of Nothing -> error "Could not find string to replace!"
                                                     Just pos -> ssub_wrep_char rep_char (replaceElem pos orig x) xs
               where xIndex = elemIndex rep_char orig

--use the default replacement character, ?
ssub :: String -> [String] -> String
ssub = ssub_wrep_char default_replacement_character

-- |simple implementation of die
-- only exists in base >= 4.8
die :: String -> IO a
die errMsg = hPutStrLn stderr errMsg >> exitFailure

-- |resource access utils
initializeSqlQuery :: IO String
initializeSqlQuery = initializeSqlResource >>= readFile 

functionsSqlQuery :: IO String
functionsSqlQuery = functionsSqlResource >>= readFile

-- |internal resource functions (not exported)
initializeSqlResource :: IO FilePath
initializeSqlResource = Paths.getDataFileName "res/initialize.sql"

functionsSqlResource :: IO FilePath
functionsSqlResource = Paths.getDataFileName "res/functions.sql"
