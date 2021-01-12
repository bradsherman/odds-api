module Data.Response where

import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:))
import Data.Aeson.Types (withArray, withObject)
import Data.Text (Text, pack)

import Data.Sport (Sport(..))
import Data.SportingEvent (SportingEvent(..))
import Data.Foldable (asum)

data ApiResponse b = ApiResponse
  { success :: Bool,
    body :: [b]
  } deriving (Show, Eq)

instance (FromJSON b) => FromJSON (ApiResponse b) where
  parseJSON = withObject "ApiResponse" $ \v ->
    ApiResponse
      <$> v .: pack "success"
      <*> v .: pack "data"
