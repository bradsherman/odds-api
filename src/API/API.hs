{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API.API (getMoneylines, getSports, getSpreads, Region (..), Market (..), SportKey (..)) where

import Control.Monad (MonadPlus (mzero))
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Odds (H2HResponse (..), SpreadsResponse (..))
import Data.Proxy (Proxy (..))
import Data.Response (ApiResponse)
import Data.Sport (Sport)
import Data.SportingEvent (SportingEvent)
import Data.Text (Text, pack, toLower)
import GHC.Generics (Generic)
import Servant.API
-- for some reason adding this makes :<|> cause an error
-- (Capture, Get, JSON, (:<|>), type (:>))
import Servant.Client (ClientM, client)

data Region = US | UK
  deriving (Eq, Show, Generic)

instance ToHttpApiData Region where
  toUrlPiece = toLower . pack . show
  toQueryParam = toLower . pack . show

data Market = Spreads | H2H | Totals
  deriving (Eq, Show)

instance ToHttpApiData Market where
  toUrlPiece = toLower . pack . show
  toQueryParam = toLower . pack . show

-- there are more, just doing these for now
data SportKey = UPCOMING | BASKETBALL_NBA | BASKETBALL_NCAAB | MMA_MIXED_MARTIAL_ARTS | AMERICANFOOTBALL_NCAAF | AMERICANFOOTBALL_NFL | BASEBALL_MLB
  deriving (Eq, Show)

instance ToHttpApiData SportKey where
  toUrlPiece = toLower . pack . show
  toQueryParam = toLower . pack . show

type OddsAPI =
  "sports" :> QueryParam "apiKey" String :> QueryParam "all" Bool :> Get '[JSON] (ApiResponse Sport)
    -- I feel like I shouldn't have to duplicate these endpoints like this..
    -- probably have to put Odds into Site as ADT and parse
    :<|> "odds"
    :> QueryParam "apiKey" String
    :> QueryParam "sport" SportKey
    :> QueryParam "region" Region
    :> QueryParam "mkt" Market
    :> Get '[JSON] (ApiResponse (SportingEvent SpreadsResponse))
    :<|> "odds"
    :> QueryParam "apiKey" String
    :> QueryParam "sport" SportKey
    :> QueryParam "region" Region
    :> QueryParam "mkt" Market
    :> Get '[JSON] (ApiResponse (SportingEvent H2HResponse))

oddsAPI :: Proxy OddsAPI
oddsAPI = Proxy

getSports :: Maybe String -> Maybe Bool -> ClientM (ApiResponse Sport)
getSpreads :: Maybe String -> Maybe SportKey -> Maybe Region -> Maybe Market -> ClientM (ApiResponse (SportingEvent SpreadsResponse))
getMoneylines :: Maybe String -> Maybe SportKey -> Maybe Region -> Maybe Market -> ClientM (ApiResponse (SportingEvent H2HResponse))
(getSports :<|> getSpreads :<|> getMoneylines) = client oddsAPI
