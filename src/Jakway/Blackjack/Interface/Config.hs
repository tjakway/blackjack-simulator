module Jakway.Blackjack.Interface.Config where

import Jakway.Blackjack.AI
import Jakway.Blackjack.IO.TableNames


data Config = Config
            { verbose :: Bool,
              whichDealer :: AI,
              playerAIs :: [AI],
              numGames :: Integer,
              tableNames :: TableNames,
              psqlConnString :: Maybe String
            }
