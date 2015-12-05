module Jakway.Blackjack.AI where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Cards
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Points
import Control.Monad.State

class AI a where
  play :: a -> Hand -> Deck -> (Hand, Deck)

data BasicDealer = BasicDealer
data BasicPlayer = BasicPlayer

instance AI BasicDealer where
  play BasicDealer myHand deck = flip runState deck $ do
      let points = handPoints (map unwrapVisibility myHand)
      if points < 17
         then do
           drawnCard <- drawCard
           deck' <- get
           return . fst $ play BasicDealer (Shown drawnCard : myHand) deck'
         else 
           return myHand


instance AI BasicPlayer where
  play BasicPlayer = play BasicDealer
