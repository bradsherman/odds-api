module Data.SportingEvent where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Site (Site (..))
import Data.Text (Text, pack)

type SportKey = Text

type TeamName = Text

type Timestamp = Integer

data SportingEvent = SportingEvent
  { sportKey :: SportKey,
    sportName :: Text,
    teams :: [TeamName],
    commenceTime :: Timestamp,
    homeTeam :: TeamName,
    sites :: [Site],
    sitesCount :: Integer
  }
  deriving (Show, Eq)

instance FromJSON SportingEvent where
  parseJSON = withObject "SportingEvent" $ \v ->
    SportingEvent
      <$> v .: pack "sport_key"
      <*> v .: pack "sport_nice"
      <*> v .: pack "teams"
      <*> v .: pack "commence_time"
      <*> v .: pack "home_team"
      <*> v .: pack "sites"
      <*> v .: pack "sites_count"
