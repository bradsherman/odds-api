module Main where

import Lib ( getSports, getUpcoming, Config(..), H2H(..), Spreads(..), Sport, SportingEvent, someFunc, sportKey, sportName )
import Data.Aeson ( decode )
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import LoadEnv (loadEnv)
import System.Envy (decodeEnv)

main :: IO ()
main = do
  loadEnv
  print =<< do decodeEnv :: IO (Either String Config)
  -- testSports <- B.readFile "test_sports.json"
  -- testSpreads <- B.readFile "test_spreads.json"
  -- testH2H <- B.readFile "test_h2h.json"
  -- print $ maybe [] (map sportKey) (decode testSports :: Maybe [Sport])
  -- print $ fromMaybe [] (decode testSpreads :: Maybe [SportingEvent Spreads])
  -- print $ fromMaybe [] (decode testH2H :: Maybe [SportingEvent H2H])
  -- print upcoming
  return ()
