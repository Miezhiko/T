{-# LANGUAGE
    DeriveGeneric
  , UnicodeSyntax
  #-}

module Track
  ( FromJSON
  , ToJSON
  , Track (..)
  , yDecode
  , yEncode
  ) where

import           GHC.Generics

import           Data.Aeson.Types      (defaultOptions, genericToJSON)
import           Data.Yaml

import           Control.Monad         ((<=<))

import qualified Data.ByteString.Char8 as BS

data Track
  = Track
      { tracked :: Maybe String
      , start   :: String
      , pause   :: Maybe String
      }
  deriving (Generic, Show)

instance FromJSON Track
instance ToJSON Track where
  toJSON = genericToJSON defaultOptions

yDecode ∷ FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode = decodeThrow <=< BS.readFile

yEncode ∷ ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode = (. encode) . BS.writeFile
