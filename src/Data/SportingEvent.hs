module Data.SportingEvent where

import Data.Sport (Sport(..), SportKey)
import Data.Site (Site(..), Timestamp)

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Text (Text, pack)
import Data.Aeson.Types (withObject)

type TeamName = Text

data SportingEvent o = SportingEvent
  { sportKey :: SportKey,
    sportName :: Text,
    teams :: [TeamName],
    commenceTime :: Timestamp,
    homeTeam :: TeamName,
    sites :: [Site o],
    sitesCount :: Integer
  } deriving (Show, Eq)

instance (FromJSON o) => FromJSON (SportingEvent o) where
  parseJSON = withObject "SportingEvent" $ \v ->
    SportingEvent
      <$> v .: pack "sport_key"
      <*> v .: pack "sport_nice"
      <*> v .: pack "teams"
      <*> v .: pack "commence_time"
      <*> v .: pack "home_team"
      <*> v .: pack "sites"
      <*> v .: pack "sites_count"
