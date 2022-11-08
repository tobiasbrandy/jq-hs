{-# LANGUAGE DeriveGeneric #-}
module Data.Json
( Json (..)
, JsonNum (..)
, jsonNumMaybe
, fromJsonNum
, jsonBool
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

import qualified Data.Text.Encoding as T (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Control.Monad (ap, liftM, liftM2)

data JsonNum a
  = NaN
  | Num a
  deriving (Generic)

instance Hashable a => Hashable (JsonNum a)

instance Eq a => Eq (JsonNum a) where
  Num l == Num r  = l == r
  _ == _          = False

instance Ord a => Ord (JsonNum a) where
  compare (Num l) (Num r) = compare l r
  compare NaN     _       = LT
  compare _       NaN     = GT

instance Functor JsonNum where
  fmap = liftM

instance Applicative JsonNum where
  pure = Num
  (<*>) = ap

instance Monad JsonNum where
  m >>= k = case m of
    NaN   -> NaN
    Num a -> k a

instance Num a => Num (JsonNum a) where
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  (*) = liftM2 (*)
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

data Json
  = Object (HashMap Text Json)
  | Array (Seq Json)
  | String Text
  | Number (JsonNum Scientific)
  | Bool Bool
  | Null
  deriving (Eq, Generic)

instance Hashable Json

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

instance Show Json where
  show = T.unpack . T.decodeUtf8With lenientDecode . BS.toStrict . jsonEncode compactFormat

jsonNumMaybe :: JsonNum a -> Maybe a
jsonNumMaybe NaN      = Nothing
jsonNumMaybe (Num a)  = Just a

fromJsonNum :: JsonNum a -> a
fromJsonNum NaN = error "fromJsonNum: JsonNum is NaN"
fromJsonNum (Num a) = a

jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

jsonShowType :: Json -> Text
jsonShowType (Number  _)  = "number"
jsonShowType (String  _)  = "string"
jsonShowType (Bool    _)  = "boolean"
jsonShowType (Object  _)  = "object"
jsonShowType (Array   _)  = "array"
jsonShowType Null         = "null"
