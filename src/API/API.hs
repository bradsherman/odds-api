{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API.API (getSports, getOdds, Region (..), Market (..), SportKey (..)) where

import Control.Monad (MonadPlus (mzero))
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Odds (OddsResponse) --, H2HResponse (..), SpreadsResponse (..))
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
    :<|> "odds"
    :> QueryParam "apiKey" String
    :> QueryParam "sport" SportKey
    :> QueryParam "region" Region
    :> QueryParam "mkt" Market
    :> Get '[JSON] (ApiResponse SportingEvent)

oddsAPI :: Proxy OddsAPI
oddsAPI = Proxy

getSports :: Maybe String -> Maybe Bool -> ClientM (ApiResponse Sport)
getOdds :: Maybe String -> Maybe SportKey -> Maybe Region -> Maybe Market -> ClientM (ApiResponse SportingEvent)
(getSports :<|> getOdds) = client oddsAPI
