{-# LANGUAGE ScopedTypeVariables #-}

module Client where

import Config (Config (..))
import Data.Aeson (FromJSON, decode)
import Data.Market (Market (..))
import Data.Odds (H2HResponse (..), SpreadsResponse (..))
import Data.Response (ApiResponse (..))
import Data.Sport (Sport)
import Data.SportingEvent (SportingEvent)
import Data.Text (Text, append, pack, toLower, unpack)
import GHC.IO (liftIO)
import Network.HTTP.Client.Conduit (Request, Response, parseRequest)
import Network.HTTP.Simple (getResponseBody, httpJSON)

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
  let url = append baseUrl $ append version $ append (pack "sports?apiKey=") (pack $ oddsApiKey cfg)
   in do
        request <- parseRequest (unpack url)
        b :: ApiResponse Sport <- callApi request
        pure $ body b

getUpcoming :: (FromJSON o) => Config -> Market -> IO [SportingEvent o]
getUpcoming cfg mkt =
  let path = append (pack "odds?sport=UPCOMING&region=us&mkt=") $ append (toLower $ pack $ show mkt) (pack "&apiKey=")
      url = append baseUrl $ append version $ append path (pack $ oddsApiKey cfg)
   in do
        request <- parseRequest (unpack url)
        body <$> callApi request
