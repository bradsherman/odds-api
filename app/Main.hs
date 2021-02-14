{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import Lib
  ( ApiResponse (body),
    Config (..),
    Market (..),
    Region (..),
    Sport (sportKey),
    SportKey (..),
    SportingEvent,
    defConfig,
    getOdds,
    getSports,
    sportName,
  )
import LoadEnv (loadEnv)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import System.Envy (decodeWithDefaults)
import Text.Read (readMaybe)

queries :: Config -> ClientM (ApiResponse Sport, ApiResponse SportingEvent, ApiResponse SportingEvent)
queries cfg = do
  sports <- getSports token (Just True)
  spreads <- getOdds token sport region (Just Spreads)
  moneylines <- getOdds token sport region (Just H2H)
  return (sports, spreads, moneylines)
  where
    token = Just $ oddsApiKey cfg
    sport = Just BASKETBALL_NCAAB
    region = Just US

data InputSource = File | Api
  deriving (Show, Eq, Read)

printFileContents :: IO ()
printFileContents = do
  -- Test with json files so we don't use up the api request limit
  testSports <- B.readFile "test_sports.json"
  testSpreads <- B.readFile "test_spreads.json"
  testH2H <- B.readFile "test_h2h.json"
  print $ maybe [] (map sportKey) (decode testSports :: Maybe [Sport])
  print $ fromMaybe [] (decode testSpreads :: Maybe [SportingEvent])
  print $ fromMaybe [] (decode testH2H :: Maybe [SportingEvent])
  return ()

printApiContents :: IO ()
printApiContents = do
  loadEnv
  cfg <- decodeWithDefaults defConfig :: IO Config
  manager' <- newManager tlsManagerSettings

  res <- runClientM (queries cfg) (mkClientEnv manager' (BaseUrl Https "api.the-odds-api.com" 443 "/v3"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (sports, spreads, moneylines) -> do
      print sports
      print ""
      print spreads
      print ""
      print moneylines

main :: IO ()
main = do
  putStrLn "Enter your source ('Api' or 'File', case matters!)"
  input <- getLine
  case readMaybe input of
    Just source -> case source of
      File -> printFileContents
      Api -> printApiContents
    Nothing -> fail "Invalid source entered"

  return ()
