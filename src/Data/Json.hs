module Data.Json
( Json (..)
, toNum
, jsonShowType
) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Scientific (Scientific)

import qualified Data.ByteString.Lazy as BS
import Data.Char (chr)
import {-# SOURCE #-} Data.Json.Encode (jsonEncode, compactFormat)
import Data.List (sort)
import TextShow (TextShow, showb, fromLazyText)
import Data.Text.Lazy.Encoding (decodeUtf8)

data Json
  = Object (HashMap Text Json)
  | Array (Seq Json)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq)

instance Show Json where
  show = map (chr . fromEnum) . BS.unpack . jsonEncode compactFormat

instance TextShow Json where
  showb = fromLazyText . decodeUtf8 . jsonEncode compactFormat

instance Ord Json where
  -- null
  compare Null          Null          = EQ
  compare Null          _             = LT
  compare _             Null          = GT
  -- false
  compare (Bool False)  (Bool False)  = EQ
  compare (Bool False)  _             = LT
  compare _             (Bool False)  = GT
  -- true
  compare (Bool True)   (Bool True)   = EQ
  compare (Bool True)   _             = LT
  compare _             (Bool True)   = GT
  -- numbers
  compare (Number l)    (Number r)    = compare l r
  compare (Number _)    _             = LT
  compare _             (Number _)    = GT
  -- strings
  compare (String l)    (String r)    = compare l r
  compare (String _)    _             = LT
  compare _             (String _)    = GT
  -- arrays
  compare (Array l)     (Array r)     = compare l r
  compare (Array _)     _             = LT
  compare _             (Array _)     = GT
  -- objects
  compare (Object l)    (Object r)    = let keyOrd = compare (sort $ Map.keys l) (sort $ Map.keys r) in
    if keyOrd == EQ
    then 
      compare (sort $ Map.elems l) (sort $ Map.elems r)
    else
      keyOrd

toNum :: Integral a => a -> Json
toNum = Number . fromInteger . toInteger

jsonShowType :: Json -> Text
jsonShowType (Number  _)  = "number"
jsonShowType (String  _)  = "string"
jsonShowType (Bool    _)  = "boolean"
jsonShowType (Object  _)  = "object"
jsonShowType (Array   _)  = "array"
jsonShowType Null         = "null"
