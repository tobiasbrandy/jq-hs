module Data.Json.Encode (jsonEncode, compactFormat) where

import {-# SOURCE #-} Data.Json (Json)
import Data.ByteString.Lazy (ByteString)

data Format

compactFormat :: Format

jsonEncode :: Format -> Json -> ByteString
