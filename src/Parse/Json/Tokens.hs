module Parse.Json.Tokens (
  Token (..)
) where

import Data.Text (Text)
import Data.Scientific (Scientific)

data Token
  -- Literals
  = Str  Text
  | Num  Scientific
  | True
  | False
  | Null

  -- Array
  | LBrack      -- [
  | RBrack      -- ]
  | Comma       -- ,

  -- Object
  | LBrace      -- {
  | RBrace      -- }
  | KVDelim     -- :

  -- EOF
  | EOF
  deriving (Eq, Show)
