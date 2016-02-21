{-# LANGUAGE ExistentialQuantification #-}
module Jakway.Blackjack.Util where

import Data.List (elemIndex)

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
                               in front ++ ins ++ back

default_replacement_character :: Char
default_replacement_character = '?'

-- |"static substitution with replacement character"
ssub_wrep_char :: Char -> String -> [String] -> String
ssub_wrep_char _ orig [] = orig
ssub_wrep_char rep_char orig (x:xs) = case xIndex of Nothing -> error "Could not find string to replace!"
                                                     Just pos -> ssub_wrep_char rep_char (replaceElem pos orig x) xs
    where xIndex = elemIndex rep_char orig

--use the default replacement character, ?
ssub = ssub_wrep_char default_replacement_character
