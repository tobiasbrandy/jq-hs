{-# LANGUAGE BangPatterns, RecordWildCards #-}

-- Simple Json (Pretty) Encoding
module Json.Encode (
-- Pretty printing inspired by https://hackage.haskell.org/package/aeson-2.1.0.0/docs/src/Data.Aeson.Text.html
  encode
, encodeToTextBuilder

-- Pretty printing inspired by https://hackage.haskell.org/package/aeson-pretty-0.8.9/docs/src/Data.Aeson.Encode.Pretty.html
, encodePretty
, encodePrettyToTextBuilder

, Config (..)
, Indent (..)
, NumberFormat (..)
, defConfig
) where

import Json (Json (..))

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map

import Data.Foldable (toList)
import Data.List (intersperse, sortBy)
import Data.Function (on)

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder (Builder, toLazyText)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Data.Text.Lazy.Encoding (encodeUtf8)

import qualified Data.Scientific as S (Scientific, FPFormat(..), base10Exponent)
import Numeric (showHex)

------------------------------------- Compact Encoding ------------------------------------------

-- | Encode json to ByteString
encode :: Json -> ByteString
encode = encodeUtf8 . toLazyText . encodeToTextBuilder

-- | Encode a Json to a "Data.Text" 'Builder', which can be
-- embedded efficiently in a text-based protocol.
encodeToTextBuilder :: Json -> Builder
encodeToTextBuilder = go
  where
    go Null       = "null"
    go (Bool b)   = if b then "true" else "false"
    go (Number s) = fromScientific s
    go (String s) = string s
    go (Array v)
      | Seq.null v = "[]"
      | otherwise = 
        TB.singleton '[' <>
        go (unsafeHead v) <>
        foldr f (TB.singleton ']') (unsafeTail v)
      where f a z = TB.singleton ',' <> go a <> z
    go (Object m) = 
      case Map.toList m of
        (x:xs) -> TB.singleton '{' <> one x <> foldr f (TB.singleton '}') xs
        _      -> "{}"
      where f a z     = TB.singleton ',' <> one a <> z
            one (k,v) = string k <> TB.singleton ':' <> go v

string :: T.Text -> Builder
string s = TB.singleton '"' <> quote s <> TB.singleton '"'
  where
    quote q = case T.uncons t of
      Nothing      -> TB.fromText h
      Just (!c,t') -> TB.fromText h <> escape c <> quote t'
      where (h,t) = T.break isEscape q
    isEscape c =
      c == '\"' ||
      c == '\\' ||
      c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    escape c
      | c < '\x20' = TB.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
      | otherwise  = TB.singleton c
      where h = showHex (fromEnum c) ""

fromScientific :: S.Scientific -> Builder
fromScientific s = formatScientificBuilder format prec s
  where
    (format, prec)
      | S.base10Exponent s < 0  = (S.Generic, Nothing)
      | otherwise               = (S.Fixed,   Just 0)

unsafeHead :: Seq a -> a
unsafeHead s = let (h :<| _) = s in h

unsafeTail :: Seq a -> Seq a
unsafeTail s = let (_ :<| t) = s in t

------------------------------------- Pretty Encoding ------------------------------------------

-- Terminal Colors
cReset  :: Builder
cReset  = "\ESC[0m"
cBlack  :: Builder
cBlack  = "\ESC[;30m"
cBBlue  :: Builder
cBBlue  = "\ESC[1;34m"
cGreen  :: Builder
cGreen  = "\ESC[;32m"

data PState = PState { 
  pLevel      :: Int,
  pIndent     :: Builder,
  pNewline    :: Builder,
  pItemSep    :: Builder,
  pKeyValSep  :: Builder,
  pNumFormat  :: NumberFormat,
  pSort       :: [(Text, Json)] -> [(Text, Json)],
  pColorize   :: Bool
}

-- | Indentation per level of nesting. @'Spaces' 0@ removes __all__ whitespace
--   from the output.
data Indent = Spaces Int | Tab

data NumberFormat
  -- | Uses integer literals for integers (1, 2, 3...), simple decimals
  --   for fractional values between 0.1 and 9,999,999, and scientific
  --   notation otherwise.
  = Generic
  -- | Scientific notation (e.g. 2.3e123).
  | Scientific
  -- | Standard decimal notation
  | Decimal
  -- | Custom formatting function
  | Custom (S.Scientific -> Builder)

data Config = Config {
  -- Indentation per level of nesting
  confIndent  :: Indent,
  -- Function used to sort keys in objects
  confCompare :: Text -> Text -> Ordering,
  -- Number format
  confNumFormat :: NumberFormat,
  -- Whether to add a trailing newline to the output
  confTrailingNewline :: Bool,
  -- Wether to add color for terminal output
  confColorizeTerminal :: Bool
}

-- | The default configuration: indent by four spaces per level of nesting, do
--  not sort objects by key, do not add trailing newline.
--
--  > defConfig = Config { confIndent = Spaces 4, confCompare = mempty, confNumFormat = Generic, confTrailingNewline = False }
defConfig :: Config
defConfig = Config {confIndent = Spaces 4, confCompare = mempty, confNumFormat = Generic, confTrailingNewline = False, confColorizeTerminal = False }

-- | Pretty encode json
encodePretty :: Config -> Json -> ByteString
encodePretty conf = encodeUtf8 . toLazyText . encodePrettyToTextBuilder conf

-- | Pretty encode json
encodePrettyToTextBuilder :: Config -> Json -> Builder
encodePrettyToTextBuilder Config {..} x = fromValue st x <> trail
  where
    indent  = case confIndent of
      Spaces n -> mconcat (replicate n " ")
      Tab      -> "\t"
    newline = case confIndent of
      Spaces 0 -> ""
      _        -> "\n"
    itemSep = ","
    kvSep   = case confIndent of
      Spaces 0 -> ":"
      _        -> ": "
    sortFn  = sortBy (confCompare `on` fst)
    trail   = if confTrailingNewline then "\n" else ""
    st = PState {
      pLevel      = 0,
      pIndent     = indent,
      pNewline    = newline,
      pItemSep    = itemSep,
      pKeyValSep  = kvSep,
      pNumFormat  = confNumFormat,
      pSort       = sortFn,
      pColorize   = confColorizeTerminal
    }

fromValue :: PState -> Json -> Builder
fromValue st@PState { pSort } = go
  where
    go (Array v)  = fromCompound st ("[","]") fromValue (toList v)
    go (Object m) = fromCompound st ("{","}") fromPair (pSort (Map.toList m))
    go (Number x) = fromNumber st x
    go (String s) = fromString st s
    go Null       = fromNull st
    go v          = encodeToTextBuilder v

fromCompound :: PState
  -> (Builder, Builder)
  -> (PState -> a -> Builder)
  -> [a]
  -> Builder
fromCompound st@PState { pLevel, pNewline, pItemSep } (delimL,delimR) fromItem items = mconcat
  [ delimL
  , if null items then mempty
      else pNewline <> items' <> pNewline <> fromIndent st
  , delimR
  ]
  where
    items' = mconcat . intersperse (pItemSep <> pNewline) $
      map (\item -> fromIndent st' <> fromItem st' item) items
    st' = st { pLevel = pLevel + 1}

fromPair :: PState -> (Text, Json) -> Builder
fromPair st@PState { pColorize } (k,v) =
  let keyEnc = if pColorize then cBBlue <> string k <> cReset else string k in
    keyEnc <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState { pLevel, pIndent } = mconcat (replicate pLevel pIndent)

fromNumber :: PState -> S.Scientific -> Builder
fromNumber st x = case pNumFormat st of
  Generic
    | x > 1.0e19 || x < -1.0e19 -> formatScientificBuilder S.Exponent Nothing x
    | otherwise -> encodeToTextBuilder $ Number x
  Scientific -> formatScientificBuilder S.Exponent Nothing x
  Decimal    -> formatScientificBuilder S.Fixed Nothing x
  Custom f   -> f x

fromString :: PState -> Text -> Builder
fromString PState { pColorize } s =
  let enc = string s in
    if pColorize then cGreen <> enc <> cReset else enc

fromNull :: PState -> Builder
fromNull PState { pColorize } =
  let enc = encodeToTextBuilder Null in
    if pColorize then cBlack <> enc <> cReset else enc
