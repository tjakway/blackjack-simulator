module Jakway.Blackjack.Tests.Constants (testDeck, test_1v1_game, test_db_name) where

import Jakway.Blackjack.Cards
import Jakway.Blackjack.AI
import Jakway.Blackjack.Game
import Data.Maybe (fromJust)


test_db_name = "test_tmp.db"

testDeck = [Card {cardSuit = Spade, cardValue = Three},Card {cardSuit = Club, cardValue = Nine},Card {cardSuit = Heart, cardValue = Ten},Card {cardSuit = Spade, cardValue = Four},Card {cardSuit = Spade, cardValue = King},Card {cardSuit = Heart, cardValue = Eight},Card {cardSuit = Diamond, cardValue = Ten},Card {cardSuit = Heart, cardValue = Three},Card {cardSuit = Diamond, cardValue = Three},Card {cardSuit = Spade, cardValue = Seven},Card {cardSuit = Heart, cardValue = Nine},Card {cardSuit = Spade, cardValue = Ten},Card {cardSuit = Club, cardValue = King},Card {cardSuit = Club, cardValue = Ten},Card {cardSuit = Club, cardValue = Six},Card {cardSuit = Heart, cardValue = Five},Card {cardSuit = Heart, cardValue = Queen},Card {cardSuit = Heart, cardValue = Six},Card {cardSuit = Club, cardValue = Four},Card {cardSuit = Spade, cardValue = Jack},Card {cardSuit = Spade, cardValue = Six},Card {cardSuit = Diamond, cardValue = Six},Card {cardSuit = Heart, cardValue = Seven},Card {cardSuit = Spade, cardValue = Nine},Card {cardSuit = Spade, cardValue = Queen},Card {cardSuit = Club, cardValue = Five},Card {cardSuit = Diamond, cardValue = Five},Card {cardSuit = Diamond, cardValue = Eight},Card {cardSuit = Spade, cardValue = Five},Card {cardSuit = Diamond, cardValue = Seven},Card {cardSuit = Heart, cardValue = Four},Card {cardSuit = Diamond, cardValue = Nine},Card {cardSuit = Diamond, cardValue = Four},Card {cardSuit = Club, cardValue = Three},Card {cardSuit = Club, cardValue = Queen},Card {cardSuit = Club, cardValue = Seven},Card {cardSuit = Spade, cardValue = Ace},Card {cardSuit = Diamond, cardValue = Jack},Card {cardSuit = Heart, cardValue = Ace},Card {cardSuit = Club, cardValue = Two},Card {cardSuit = Diamond, cardValue = Ace},Card {cardSuit = Heart, cardValue = Jack},Card {cardSuit = Spade, cardValue = Two},Card {cardSuit = Diamond, cardValue = Queen},Card {cardSuit = Diamond, cardValue = Two},Card {cardSuit = Heart, cardValue = Two},Card {cardSuit = Diamond, cardValue = King},Card {cardSuit = Club, cardValue = Jack},Card {cardSuit = Club, cardValue = Ace},Card {cardSuit = Club, cardValue = Eight},Card {cardSuit = Spade, cardValue = Eight},Card {cardSuit = Heart, cardValue = King}]


test_1v1_game = fromJust $ evalGame BasicDealer [BasicPlayer] testDeck
