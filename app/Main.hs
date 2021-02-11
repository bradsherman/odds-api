{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Lib
  ( ApiResponse (body),
    Config (..),
    H2HResponse (..),
    Market (..),
    Region (..),
    Sport,
    SportKey (..),
    SportingEvent,
    SpreadsResponse (..),
    defConfig,
    getMoneylines,
    getSports,
    getSpreads,
    sportName,
  )
import LoadEnv (loadEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Envy (decodeWithDefaults)

queries :: Config -> ClientM (ApiResponse Sport, ApiResponse (SportingEvent SpreadsResponse), ApiResponse (SportingEvent H2HResponse))
queries cfg = do
  sports <- getSports token (Just True)
  spreads <- getSpreads token sport region (Just Spreads)
  moneylines <- getMoneylines token sport region (Just H2H)
  return (sports, spreads, moneylines)
  where
    token = Just $ oddsApiKey cfg
    sport = Just BASKETBALL_NCAAB
    region = Just US

main :: IO ()
main = do
  loadEnv
  cfg <- decodeWithDefaults defConfig :: IO Config
  manager' <- newManager tlsManagerSettings

  res <- runClientM (queries cfg) (mkClientEnv manager' (BaseUrl Https "api.the-odds-api.com" 443 "/v3"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (sports, spreads, moneylines) -> do
      print sports
      print spreads
      print moneylines

  -- Test with json files so we don't use up the api request limit
  -- testSports <- B.readFile "test_sports.json"
  -- testSpreads <- B.readFile "test_spreads.json"
  -- testH2H <- B.readFile "test_h2h.json"
  -- print $ maybe [] (map sportKey) (decode testSports :: Maybe [Sport])
  -- print $ fromMaybe [] (decode testSpreads :: Maybe [SportingEvent SpreadsResponse])
  -- print $ fromMaybe [] (decode testH2H :: Maybe [SportingEvent H2HResponse])
  return ()
