{-# LANGUAGE DeriveGeneric #-}
module Data.Json
( Json (..)
, jsonShowType
) where

import {-# SOURCE #-} Data.Json.Encode (jsonEncode, compactFormat)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.Scientific (Scientific)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Data.List (sort, sortBy)
import Data.Ord (comparing)

import TextShow (TextShow, showb, fromLazyText)
import qualified Data.Text.Encoding as T (decodeUtf8With)
import qualified Data.Text.Lazy.Encoding as TL (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

data Json
  = Object (HashMap Text Json)
  | Array (Seq Json)
  | String Text
  | Number Scientific
  | Bool Bool
  | Null
  deriving (Eq, Generic)

instance Hashable Json

instance Show Json where
  show = T.unpack . T.decodeUtf8With lenientDecode . BS.toStrict . jsonEncode compactFormat

instance TextShow Json where
  showb = fromLazyText . TL.decodeUtf8With lenientDecode . jsonEncode compactFormat

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
      compare (map snd $ sortBy (comparing fst) $ Map.toList l) (map snd $ sortBy (comparing fst) $ Map.toList r)
    else
      keyOrd

jsonShowType :: Json -> Text
jsonShowType (Number  _)  = "number"
jsonShowType (String  _)  = "string"
jsonShowType (Bool    _)  = "boolean"
jsonShowType (Object  _)  = "object"
jsonShowType (Array   _)  = "array"
jsonShowType Null         = "null"
