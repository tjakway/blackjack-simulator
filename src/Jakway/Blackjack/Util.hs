{-# LANGUAGE ExistentialQuantification #-}
module Jakway.Blackjack.Util where

import Data.List (elemIndex)
import System.IO
import System.Exit hiding (die)
import System.Console.GetOpt
import Data.Maybe (catMaybes)

innerMapTuple4 :: forall t t1. (t -> t1) -> (t, t, t, t) -> (t1, t1, t1, t1)
innerMapTuple4 f (a,b,c,d) = (f a, f b, f c, f d)

innerMapTuple3 :: forall t t1. (t -> t1) -> (t, t, t) -> (t1, t1, t1)
innerMapTuple3 f (a,b,c) = (f a, f b, f c)

innerMapTuple2 :: forall t t1. (t -> t1) -> (t, t) -> (t1, t1)
innerMapTuple2 f (a,b) = (f a, f b)

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

parseOptions :: [String] -> [OptDescr a] -> String -> IO ([a], [String])
parseOptions argv options usage = 
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))

-- |returns the underlying value of the underlying flag if exactly one
-- exists
getSingleFlag :: (a -> Maybe b) -> [a] -> Maybe b
getSingleFlag f allFlags = case catMaybes $ map f allFlags of [x] -> Just x
                                                              [] -> Nothing
