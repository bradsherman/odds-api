module Data.Response where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (pack)

data ApiResponse b = ApiResponse
  { success :: Bool,
    body :: [b]
  }
  deriving (Show, Eq)

instance (FromJSON b) => FromJSON (ApiResponse b) where
  parseJSON = withObject "ApiResponse" $ \v ->
    ApiResponse
      <$> v .: pack "success"
      <*> v .: pack "data"
