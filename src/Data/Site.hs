module Data.Site where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (Text, pack)

type SiteKey = Text

-- TODO move this somewhere else?
type Timestamp = Integer

data Site o = Site
  { siteKey :: SiteKey,
    siteName :: Text,
    lastUpdate :: Timestamp,
    odds :: o
  }
  deriving (Show, Eq)

instance (FromJSON o) => FromJSON (Site o) where
  parseJSON = withObject "Site" $ \v ->
    Site
      <$> v .: pack "site_key"
      <*> v .: pack "site_nice"
      <*> v .: pack "last_update"
      <*> v .: pack "odds"
