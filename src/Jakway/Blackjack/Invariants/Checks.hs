module Jakway.Blackjack.Invariants.Checks where

import Jakway.Blackjack.CardOps
import Jakway.Blackjack.Visibility
import Jakway.Blackjack.Points

checkHand :: Hand -> Either String Hand
checkHand hand = hiddenCardsCheck hand >>= handCeilingCheck

hiddenCardsCheck :: Hand -> Either String Hand
hiddenCardsCheck hand = numHiddenCheck

        where numHidden = foldr (\thisCard hiddenSum -> case thisCard
                                of (Hidden _) -> hiddenSum + 1
                                   (Shown _) -> hiddenSum) 0 hand
              numHiddenCheck = if numHidden /= 1 then Left "Should only have 1 face down card" else Right hand

--invariants to implement:
-- * if both players bust it should be a tie
-- * no player should be over 30 (because it would mean they busted and
-- then hit again, which should never happen)

handCeilingCheck :: Hand -> Either String Hand
handCeilingCheck hand = if points > maxPoints then Left "Too many points in hand--must have hit after a bust."
                                              else Right hand
        where points = handPoints . (map unwrapVisibility) $ hand
              maxPoints = 30 --ceiling
