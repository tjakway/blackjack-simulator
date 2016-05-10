{-# LANGUAGE FlexibleInstances #-}
module Jakway.Blackjack.Visibility where

import Jakway.Blackjack.Cards
import Control.Applicative
import Control.Monad (join)

data Visibility a = Hidden a | Shown a
                  deriving (Eq, Show)

unwrapVisibility :: Visibility a -> a
unwrapVisibility (Hidden a) = a
unwrapVisibility (Shown a) = a

instance Functor Visibility where
  fmap f (Hidden a) = Hidden (f a)
  fmap f (Shown a) = Shown (f a)

instance Monad Visibility where
  --cards are shown by default
  return = Shown

  (>>=) (Shown a) f  = f a
  (>>=) (Hidden a) f = f a

instance Applicative Visibility where
  pure = return
  (Hidden f) <*> b = fmap f b
  (Shown f) <*> b = fmap f b

-- |Need this instance so we can use Visibility Card in a hashmap
instance Ord (Visibility Card) where
        compare (Hidden _) (Shown _) = LT
        compare (Shown _) (Hidden _) = GT
        compare (Hidden a) (Hidden b) = compare a b
        compare (Shown a) (Shown b) = compare a b

-- | analogous to catMaybes
-- returns a list of all Shown a in the input list or the empty list
catShown :: [Visibility a] -> [a]
catShown = join . (fmap (\k -> case k of Shown l -> [l]
                                         Hidden m -> []))
