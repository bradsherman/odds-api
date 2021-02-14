{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Odds where

import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Aeson.Types (parseMaybe, withArray, withObject)
import Data.Foldable (asum)
import Data.Maybe (catMaybes, fromMaybe)
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

-- Have to figure out how to parse this
data OddsResponse
  = SpreadsResponse
      { spreads :: SpreadOdds
      }
  | H2HResponse
      { h2h :: MoneylineOdds,
        h2hLay :: Maybe MoneylineOdds
      }
  deriving (Show, Eq)

instance FromJSON OddsResponse where
  parseJSON = withObject "SpreadsResponse or H2HResponse" $ \o ->
    asum
      [ SpreadsResponse <$> o .: pack "spreads",
        H2HResponse <$> o .: pack "h2h" <*> o .:? pack "h2h_lay"
      ]
