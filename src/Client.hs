{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Config (Config(..))
import Data.Text (Text, pack, append, unpack)
import Data.Odds (H2H(H2H), Spreads(Spreads))
import Data.Response (ApiResponse(..))
import Data.Sport (Sport)
import Data.SportingEvent (SportingEvent)
import Network.HTTP.Simple (getResponseStatusCode, getResponseBody, httpJSON)
import Network.HTTP.Conduit (simpleHttp)
import GHC.IO (liftIO)
import Network.HTTP.Client.Conduit (Request, parseRequest)
import Data.Aeson (FromJSON, decode)

baseUrl :: Text
baseUrl = pack "https://api.the-odds-api.com/"

version :: Text
version = pack "v3/"

callApi :: FromJSON b => Request -> IO (ApiResponse b)
callApi r = do
  response <- httpJSON r
  pure $ getResponseBody response

getSports :: Config -> IO [Sport]
getSports cfg =
  let url = append baseUrl $ append version $ append (pack "sports/?apiKey=") (pack $ oddsApiKey cfg)
    in do
      request <- parseRequest (unpack url)
      b :: ApiResponse Sport <- callApi request
      pure $ body b

-- TODO: add this constraint (OddsType o) =>
getUpcoming :: (FromJSON o, Show o) => Config -> String -> IO [SportingEvent o]
getUpcoming cfg mkt =
  let path = append (pack "odds/?sport=UPCOMING&region=us&mkt=") $ append (pack mkt) (pack "&apiKey=")
      url = append baseUrl $ append version $ append path (pack $ oddsApiKey cfg)
      -- oddsType = if mkt == "spreads" then Spreads else H2H
    in do
      request <- parseRequest (unpack url)
      print request
      b :: ApiResponse (SportingEvent o) <- callApi request
      pure $ body b
