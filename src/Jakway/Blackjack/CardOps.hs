-- |This module exists to break the dependency cycle between Visibility and
-- Cards.  It has logic that acts on cards, but Jakway.Blackjack.Cards only
-- has basic type definitions and an instance or two
module Jakway.Blackjack.CardOps where



import Jakway.Blackjack.Cards
import Jakway.Blackjack.Visibility

--disambiguate between a player's hand and the deck--both are lists of cards
type Deck = [Card]
type Hand = [Visibility Card]

type Blackjack a = State Deck a

allSuits :: [Suit]
allSuits = [minBound..maxBound] :: [Suit]

allCardValues :: [CardValue]
allCardValues = [minBound..maxBound] :: [CardValue]

isFaceCard :: Card -> Bool
isFaceCard (Card _ Jack)  = True
isFaceCard (Card _ Queen) = True
isFaceCard (Card _ King)  = True
isFaceCard (Card _ _)     = False

newDeck :: Deck
newDeck = Card <$> allSuits <*> allCardValues

-- |(strictly) shuffles an entire deck of cards
shuffleDeck :: (RandomGen a) => a -> Deck -> Deck
shuffleDeck gen cards = shuffle' cards (length cards) gen

infiniteShuffledDeck :: (RandomGen a) => a -> Deck
infiniteShuffledDeck gen = shuffledDeck ++ infiniteShuffledDeck gen
  where shuffledDeck = shuffleDeck gen newDeck

-- |draws 1 card and returns a tuple of that card and the resulting deck
-- this function intentionally DOES NOT pattern match on []--the deck is
-- supposed to be infinite so if we got an empty list it's a bug
drawCard :: Blackjack Card
drawCard = do
  card <- gets head
  modify tail
  return card

drawCard' :: Deck -> (Card, Deck)
drawCard' (x:xs) = (x, xs)

hasCard :: [Card] -> CardValue -> Bool
hasCard cards whichCard = any ((whichCard ==) . cardValue) cards
