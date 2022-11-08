module Data.Json.Parsing.Tokens (
  JsonToken (..)
, strBuilderAppend
, strBuilderToStr
) where

import Data.Parser.Build.Parser (LexInput)

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy (toStrict)

data JsonToken
  -- Literals
  = Str  Text
  | Num  Scientific
  | True
  | False
  | Null

  -- Special nums
  | NaN         -- nan
  | InfP        -- +inf
  | InfM        -- -inf

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

  -- Parser Control
  | LexError LexInput
  | EOF
  deriving (Eq, Show)

strBuilderAppend :: JsonToken -> Builder -> JsonToken
strBuilderAppend (StrBuilder s1) s2 = StrBuilder (s1 <> s2)
strBuilderAppend _ _ = error "JsonToken not StrBuilder"

strBuilderToStr :: JsonToken -> JsonToken
strBuilderToStr (StrBuilder s) = Str $ toStrict $ toLazyText s
strBuilderToStr _ = error "JsonToken not StrBuilder"
