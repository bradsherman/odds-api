{-# LANGUAGE OverloadedStrings #-}

module Data.Sport (Sport (..), SportKey, SportTitle) where

import Data.Aeson (FromJSON, parseJSON, (.:))
import Data.Aeson.Types (withObject)
import Data.Text (Text, pack)

type SportKey = Text

type SportTitle = Text

data Sport = Sport
  { sportKey :: SportKey,
    active :: Bool,
    group :: Text,
    details :: Text,
    title :: SportTitle,
    hasOutrights :: Bool
  }
  deriving (Show, Eq)

instance FromJSON Sport where
  parseJSON = withObject "Sport" $ \v ->
    Sport
      <$> v .: "key"
      <*> v .: "active"
      <*> v .: "group"
      <*> v .: "details"
      <*> v .: "title"
      <*> v .: "has_outrights"
