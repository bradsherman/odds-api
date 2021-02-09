module Lib
    ( getSports,
      getUpcoming,
      Config(..),
      defConfig,
      Market(..),
      Sport,
      SportingEvent,
      ApiResponse(..),
      H2HResponse(..),
      SpreadsResponse(..),
      sportKey,
      sportName
    ) where

import Client (getSports, getUpcoming)
import Config (Config(..), defConfig)
import Data.Market (Market(..))
import Data.Response (ApiResponse(..))
import Data.Sport (Sport, sportKey)
import Data.SportingEvent (SportingEvent, sportName)
import Data.Text (Text)
import Data.Odds (H2HResponse(..), SpreadsResponse(..))
