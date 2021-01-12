module Data.Odds where

import Data.Aeson (FromJSON, parseJSON, (.:), (.:?))
import Data.Aeson.Types (withObject, withArray)
import Data.Text (pack)

type OddsValue = Float
type OddsList = [OddsValue]

-- TODO make spreads a bit more structured
-- data SpreadValue = SpreadValue
--   { spreadValue :: Float,
--     odds :: Float
--   } deriving (Show, Eq)

data SpreadOdds = SpreadOdds
  { odds :: [OddsValue],
    points :: [String]
  } deriving (Show, Eq)
instance FromJSON SpreadOdds where
--   parseJSON j = do
--     spreads <- parseJSON j
  parseJSON = withObject "SpreadOdds" $ \v ->
    SpreadOdds
      <$> v .: pack "odds"
      <*> v .: pack "points"

newtype Spreads = Spreads
  { spreads :: SpreadOdds
  } deriving (Show, Eq)
instance FromJSON Spreads where
  parseJSON = withObject "Spreads" $ \v ->
    Spreads
      <$> v .: pack "spreads"

data MoneylineOdds = MoneylineOdds
  { team1Odds :: OddsValue,
    team2Odds :: OddsValue,
    drawOdds :: Maybe OddsValue
  } deriving (Show, Eq)
instance FromJSON MoneylineOdds where
  parseJSON j = do
    oddsList <- parseJSON j
    return $ MoneylineOdds {team1Odds = head oddsList, team2Odds = head $ tail oddsList, drawOdds = extractDrawOdds oddsList}
    where extractDrawOdds l = case length l of
                                3 -> Just (head $ tail $ tail l)
                                _ -> Nothing

data H2H = H2H
  { h2h :: MoneylineOdds,
    h2hLay :: Maybe MoneylineOdds
  } deriving (Show, Eq)
instance FromJSON H2H where
  parseJSON = withObject "H2H" $ \v ->
    H2H
      <$> v .: pack "h2h"
      <*> v .:? pack "h2h_lay"
