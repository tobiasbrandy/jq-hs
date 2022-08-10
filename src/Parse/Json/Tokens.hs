module Parse.Json.Tokens (
  JsonToken (..)
, untokStrBuilder
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Text.Lazy.Builder (Builder)

data JsonToken
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

  -- String
  | StrBuilder Builder
  | LQuote       -- "
  | RQuote       -- "

  -- EOF
  | EOF
  deriving (Eq, Show)

untokStrBuilder :: JsonToken -> Builder
untokStrBuilder (StrBuilder s) = s
untokStrBuilder _ = error "JsonToken not StrBuilder"
