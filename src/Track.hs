{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Track
  ( Track(..)
  , FromJSON
  , ToJSON
  , yDecode
  , yEncode
  ) where

import           GHC.Generics

import           Data.Yaml
import           Data.Aeson.Types            (defaultOptions, genericToJSON)

import           Control.Applicative
import           Control.Applicative.Unicode

import qualified Data.ByteString.Char8       as BS

data Track = Track { tracked ∷ Maybe String
                   , start   ∷ String
                   , pause   ∷ Maybe String
                   } deriving (Show, Generic)

instance FromJSON Track
instance ToJSON Track where
  toJSON = genericToJSON defaultOptions

yDecode ∷ FromJSON iFromJSONable ⇒ FilePath → IO iFromJSONable
yDecode fName = do
  ymlData ← BS.readFile fName
  decodeThrow ymlData

yEncode ∷ ToJSON iToJSONable ⇒ FilePath → iToJSONable → IO()
yEncode fName δ = BS.writeFile fName $ encode δ
