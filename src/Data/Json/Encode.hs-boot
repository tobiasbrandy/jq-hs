module Data.Json.Encode (jsonEncode, defaultFormat) where

import {-# SOURCE #-} Data.Json (Json)
import Data.ByteString.Lazy (ByteString)

data Format

defaultFormat :: Format

jsonEncode :: Format -> Json -> ByteString
