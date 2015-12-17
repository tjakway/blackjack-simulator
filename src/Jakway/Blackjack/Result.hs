module Jakway.Blackjack.Result where 

import Data.Monoid

data Result = Lose | Tie | Win deriving (Eq, Ord, Bounded, Show)


-- | add the passed result to the total and return the new total
addResult :: ScoreRecord -> Result -> ScoreRecord
-- could have used guards here, but I wanted practice using case
addResult (ScoreRecord prevWins prevTies prevLosses) res = 
  case res of
       Win -> ScoreRecord (prevWins+1) prevTies prevLosses
       Tie -> ScoreRecord prevWins (prevTies+1) prevLosses
       Lose -> ScoreRecord prevWins prevTies (prevLosses+1)

instance Monoid ScoreRecord where
  mempty = ScoreRecord 0 0 0
  --XXX
  --there has got to be a better way of doing this
  --maybe redefine ScoreRecord as a tuple (Integer, Integer, Integer)?
  mappend (ScoreRecord firstWins firstTies firstLosses) (ScoreRecord secondWins secondTies secondLosses) = ScoreRecord (firstWins + secondWins) (firstTies + secondTies) (firstLosses + secondLosses)

data ScoreRecord
  = ScoreRecord 
  { wins :: Integer
  , ties :: Integer
  , losses :: Integer
  } deriving Show

ordToResult :: Ordering -> Result
ordToResult GT = Win
ordToResult LT = Lose
ordToResult EQ = Tie

-- |There's probably a more concise way to define this
oppositeResult :: Result -> Result
oppositeResult Win = Lose
oppositeResult Tie = Tie
oppositeResult Lose = Win

-- |no need to create the empty list of starting records--it'll be
-- generated automatically
foldOverResults :: [ScoreRecord] -> [Result] -> [ScoreRecord]
foldOverResults [] results = foldOverResults (flip replicate mempty . length $ results) results 
foldOverResults startingRecords [] = startingRecords
foldOverResults startingRecords results = map (\(thisRecord, thisScore) -> thisRecord `addResult` thisScore) zippedArgs
                    where zippedArgs = zip startingRecords results
                          -- ^ TODO: come up with a better name...
