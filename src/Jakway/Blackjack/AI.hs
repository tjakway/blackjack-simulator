module Jakway.Blackjack.AI where

import Jakway.Blackjack.Visibility
import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Points
import Control.Monad.State

data AI = BasicDealer | BasicPlayer

play :: AI -> Hand -> Deck -> (Hand, Deck)
play BasicDealer myHand deck = flip runState deck $ do
    let points = handPoints (map unwrapVisibility myHand)
    if points < 17
        then do
        drawnCard <- drawCard
        deck' <- get
        return . fst $ play BasicDealer (Shown drawnCard : myHand) deck'
        else 
        return myHand
-- |currently all players play the same
play BasicPlayer myHand deck = play BasicDealer myHand deck
