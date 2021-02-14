module Lib
  ( getSports,
    getOdds,
    Config (..),
    defConfig,
    Market (..),
    Sport (sportKey),
    SportingEvent,
    ApiResponse (..),
    OddsResponse,
    SportKey (..),
    Region (..),
    sportName,
  )
where

import API.API (Market (..), Region (..), SportKey (..), getOdds, getSports)
import Config (Config (..), defConfig)
import Data.Odds (OddsResponse)
import Data.Response (ApiResponse (..))
import Data.Sport (Sport (sportKey))
import Data.SportingEvent (SportingEvent, sportName)
import Data.Text (Text)
