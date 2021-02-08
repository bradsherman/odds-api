module Main where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Lib (Config (..), H2HResponse (..), Market (..), Sport, SportingEvent, SpreadsResponse (..), defConfig, getSports, getUpcoming, sportKey, sportName)
import LoadEnv (loadEnv)
import System.Envy (decodeWithDefaults)

main :: IO ()
main = do
  loadEnv
  cfg <- decodeWithDefaults defConfig :: IO Config
  h2h <- getUpcoming cfg H2H :: IO [SportingEvent H2HResponse]
  print h2h
  spreads <- getUpcoming cfg Spreads :: IO [SportingEvent SpreadsResponse]
  print spreads

  -- Test with json files so we don't use up the api request limit
  -- testSports <- B.readFile "test_sports.json"
  -- testSpreads <- B.readFile "test_spreads.json"
  -- testH2H <- B.readFile "test_h2h.json"
  -- print $ maybe [] (map sportKey) (decode testSports :: Maybe [Sport])
  -- print $ fromMaybe [] (decode testSpreads :: Maybe [SportingEvent SpreadsResponse])
  -- print $ fromMaybe [] (decode testH2H :: Maybe [SportingEvent H2HResponse])
  return ()
