module Data.Json (
  Json (..)

) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Scientific (Scientific)

import qualified Data.ByteString.Lazy as BS
import Data.Char (chr)
import {-# SOURCE #-} Data.Json.Encode (jsonEncode, defaultFormat)

data Json
  = Object (HashMap Text Json)
  | Array (Seq Json)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq)

instance Show Json where
  show = map (chr . fromEnum) . BS.unpack . jsonEncode defaultFormat
