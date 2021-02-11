module Lib
  ( getMoneylines,
    getSports,
    getSpreads,
    Config (..),
    defConfig,
    Market (..),
    Sport,
    SportingEvent,
    ApiResponse (..),
    H2HResponse (..),
    SpreadsResponse (..),
    SportKey (..),
    Region (..),
    sportName,
  )
where

import API.API (Market (..), Region (..), SportKey (..), getMoneylines, getSports, getSpreads)
import Config (Config (..), defConfig)
import Data.Odds (H2HResponse (..), SpreadsResponse (..))
import Data.Response (ApiResponse (..))
import Data.Sport (Sport)
import Data.SportingEvent (SportingEvent, sportName)
import Data.Text (Text)
