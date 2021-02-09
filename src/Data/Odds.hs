{-# LANGUAGE DeriveGeneric #-}

module Data.Odds where

import Data.Aeson (FromJSON, parseJSON, (.:), (.:?))
import Data.Aeson.Types (withArray, withObject)
import Data.Text (pack)
import GHC.Generics (Generic)

type OddsValue = Float

type OddsList = [OddsValue]

data SpreadOdds = SpreadOdds
  { odds :: [OddsValue],
    points :: [String]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpreadOdds

newtype SpreadsResponse = SpreadsResponse
  { spreads :: SpreadOdds
  }
  deriving (Show, Eq, Generic)

instance FromJSON SpreadsResponse

data MoneylineOdds = MoneylineOdds
  { team1Odds :: OddsValue,
    team2Odds :: OddsValue,
    drawOdds :: Maybe OddsValue
  }
  deriving (Show, Eq)

instance FromJSON MoneylineOdds where
  parseJSON j = do
    oddsList <- parseJSON j
    return $
      MoneylineOdds
        { team1Odds = head oddsList,
          team2Odds = head $ tail oddsList,
          drawOdds = extractDrawOdds oddsList
        }
    where
      extractDrawOdds l = case length l of
        3 -> Just (head $ tail $ tail l)
        _ -> Nothing

data H2HResponse = H2HResponse
  { h2h :: MoneylineOdds,
    h2hLay :: Maybe MoneylineOdds
  }
  deriving (Show, Eq)

instance FromJSON H2HResponse where
  parseJSON = withObject "H2HResponse" $ \v ->
    H2HResponse
      <$> v .: pack "h2h"
      <*> v .:? pack "h2h_lay"
