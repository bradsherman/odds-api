module Lib
    ( getSports,
      getUpcoming,
      someFunc,
      Config(..),
      Sport,
      SportingEvent,
      H2H(..),
      Spreads(..),
      sportKey,
      sportName
    ) where

import Client (getSports, getUpcoming)
import Config (Config(..))
import Data.Sport (Sport, sportKey)
import Data.SportingEvent (SportingEvent, sportName)
import Data.Text (Text)
import Data.Odds (H2H(..), Spreads(..))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
