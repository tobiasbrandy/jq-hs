module Json (
  Json (..)

) where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Scientific (Scientific)

data Json
  = Object (Map Text Json)
  | Array (Seq Json)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq, Show)
