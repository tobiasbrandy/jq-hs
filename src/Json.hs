module Json (
  Json (..)

) where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Scientific (Scientific)

data Json
  = Object (HashMap Text Json)
  | Array (Seq Json)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq, Show)
