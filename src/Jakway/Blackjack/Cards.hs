{-# LANGUAGE DeriveGeneric #-}
module Jakway.Blackjack.Cards where

import GHC.Generics (Generic)
import Data.Hashable

data Suit
  = Spade 
  | Club 
  | Heart
  | Diamond
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

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
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data Card = Card
  { cardSuit :: Suit
  , cardValue :: CardValue
  } deriving (Show, Eq, Generic)

instance Hashable Suit
instance Hashable CardValue
instance Hashable Card

instance Ord Card where
        -- | suit comes first, then the card value
        compare (Card fstSuit fstValue) (Card sndSuit sndValue) = case (compare fstSuit sndSuit) of LT -> LT
                                                                                                    GT -> GT
                                                                                                    EQ -> compare fstValue sndValue
