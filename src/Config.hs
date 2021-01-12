{-# LANGUAGE DeriveGeneric #-}
module Config (Config(..)) where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import LoadEnv (loadEnv)
import System.Envy ( decodeEnv, DefConfig(..), FromEnv )

newtype Config
  = Config
      { oddsApiKey :: String
      }
  deriving (Generic, Show)

instance DefConfig Config where
  defConfig =
    Config
      { oddsApiKey = "Invalid"
      }

instance FromEnv Config
