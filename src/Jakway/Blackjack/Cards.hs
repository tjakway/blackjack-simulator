{-# LANGUAGE FlexibleInstances #-}
module Jakway.Blackjack.Cards where

import Control.Monad.State
import System.Random
import System.Random.Shuffle

data Suit
  = Spade 
  | Club 
  | Heart
  | Diamond
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data CardValue
  = Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Ten 
  | Jack  
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card
  { cardSuit :: Suit
  , cardValue :: CardValue
  } deriving (Show, Eq)


instance Ord Card where
        -- | suit comes first, then the card value
        compare (Card fstSuit fstValue) (Card sndSuit sndValue) = case (compare fstSuit sndSuit) of LT -> LT
                                                                                                    GT -> GT
                                                                                                    EQ -> compare fstValue sndValue
