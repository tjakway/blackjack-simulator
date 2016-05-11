module Jakway.Blackjack.AI.CardCounting where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps

-- |a score assigned to a card by the AI used to make playing decisions
type Score = Int

-- |determines what move to make based on the count (actually the index
-- table tells you when to diverge from basic strategy)
-- takes as input a score, the hit function and the stand function and
-- returns which move to make
newtype IndexTable a b c = IndexTable { lookupIndex :: Hand -> Hand -> Score -> a -> b -> c }

hiLo :: Hand -> Hand -> [Hand] -> (a -> b) -> (c -> d) -> f
hiLo myHand 
     dealersHand
     otherHands 
     hitF 
     standF = undefined
--    where cardCountScore =  fmap (cardValue . unwrapVisibility) (join $ [myHand] ++ [dealersHand] ++ otherHands) 
--    ^ WRONG--need to count my own hand separately since I can see my face
--    down card

hiLoCount :: CardValue -> Score
hiLoCount val
            | isFaceCard val = -1 -- ^ face cards and aces are -1
            | val == Ace = -1
            | val `elem` [Seven .. Nine] = 0
            | otherwise = 1 -- ^ 2 through 6 = +1

illustrious18 :: IndexTable a b c
illustrious18 myHand dealersHand score hitF standF = 
        where myHandPoints = handPoints . (fmap unwrapVisibility) $ myHand
              dealerHandPoints = 
