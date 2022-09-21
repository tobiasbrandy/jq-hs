-- Simple Json (Pretty) Encoding
-- Inspired by:
-- - https://hackage.haskell.org/package/aeson-2.1.0.0/docs/src/Data.Aeson.Text.html
-- - https://hackage.haskell.org/package/aeson-pretty-0.8.9/docs/src/Data.Aeson.Encode.Pretty.html
module Data.Json.Encode (
  jsonEncode
, jsonEncodeToByteStringBuilder

, Indent (..)

, Format (..)
, compactFormat
) where

import Data.Json (Json (..))

import qualified Data.HashMap.Strict as Map

import Data.Foldable (toList)
import Data.List (intersperse, sortBy)
import Data.Function (on)

import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BSS
import Data.ByteString.Builder (Builder, toLazyByteString, charUtf8, byteString, stringUtf8, intDec, char8, string8, string7)
import Data.ByteString.Builder.Scientific (formatScientificBuilder)

import Data.Scientific (Scientific, FPFormat(..), isInteger, toBoundedRealFloat, toDecimalDigits, formatScientific)
import Numeric (showHex)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder.Extra (byteStringCopy)
import qualified Data.ByteString.Lazy as BS

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
  pSort       :: [(Text, Json)] -> [(Text, Json)],
  pColorize   :: Bool
}

-- | Indentation per level of nesting. @'Spaces' 0@ removes __all__ whitespace
--   from the output.
data Indent = Spaces Int | Tab

data Format = Format {
  -- Indentation per level of nesting
  fmtIndent           :: Indent,
  -- Function used to sort keys in objects
  fmtCompare          :: Text -> Text -> Ordering,
  -- Wether to add color for terminal output
  fmtColorize         :: Bool,
  -- If the output is a string, don't include the quotes
  fmtRawStr           :: Bool,
  -- Whether to add a trailing newline to the output
  fmtTrailingNewline  :: Bool
}

-- Configuration for compact encoding
compactFormat :: Format
compactFormat         = Format {
  fmtIndent           = Spaces 0,
  fmtCompare          = mempty,
  fmtColorize         = False,
  fmtRawStr           = False,
  fmtTrailingNewline  = False
}

-- | Encode json using format
jsonEncode :: Format -> Json -> ByteString
jsonEncode fmt = toLazyByteString . jsonEncodeToByteStringBuilder fmt

-- | Encode json to builder using format
jsonEncodeToByteStringBuilder :: Format -> Json -> Builder
jsonEncodeToByteStringBuilder Format {..} x
  | fmtRawStr && isStr x  = let String s = x in quote s <> trail
  | otherwise             = fromValue st x <> trail
  where
    isStr (String _)  = True
    isStr _           = False

    indent  = case fmtIndent of
      Spaces n -> mconcat (replicate n " ")
      Tab      -> "\t"
    newline = case fmtIndent of
      Spaces 0 -> ""
      _        -> "\n"
    itemSep = ","
    kvSep   = case fmtIndent of
      Spaces 0 -> ":"
      _        -> ": "
    sortFn  = sortBy (fmtCompare `on` fst)
    trail   = if fmtTrailingNewline then "\n" else ""
    st = PState {
      pLevel      = 0,
      pIndent     = indent,
      pNewline    = newline,
      pItemSep    = itemSep,
      pKeyValSep  = kvSep,
      pSort       = sortFn,
      pColorize   = fmtColorize
    }

fromValue :: PState -> Json -> Builder
fromValue st@PState { pSort } = go
  where
    go Null       = fromNull st
    go (Bool b)   = fromBool st b
    go (Number x) = fromNumber st x
    go (String s) = fromString st s
    go (Array v)  = fromCompound st ("[","]") fromValue (toList v)
    go (Object m) = fromCompound st ("{","}") fromPair (pSort (Map.toList m))

fromNull :: PState -> Builder
fromNull PState { pColorize } =
  let enc = "null" in
    if pColorize then cBlack <> enc <> cReset else enc

fromBool :: PState -> Bool -> Builder
fromBool _ b = if b then "true" else "false"

fromNumber :: PState -> Scientific -> Builder
fromNumber _ x
  | isInteger x =
    if x < 1e23 && x > -1e23
    then formatScientificBuilder Fixed (Just 0) x
    else formatSciExponentBuilder x
  | otherwise = case toBoundedRealFloat x :: Either Double Double of
    Left e    ->
      if e == 0
      then "0"
      else if e > 0 then "Infinity" else "-Infinity"
    Right n  -> string7 $
      if x < 1e23 && x > -1e23
      then reverse $ dropWhile (== '0') $ reverse $ formatScientific Fixed (Just 15) x
      else reformatDoubleShow $ show n
    where
      -- Removes unnecessary decimal part (ex. '1.0e-1' to '1e-1')
      -- Adds explicit '+' to exponent (ex. '1.1e2' to '1.1e+2')
      reformatDoubleShow :: String -> String
      reformatDoubleShow ('.':'0':'e':s)  = reformatDoubleShow ('e':s)
      reformatDoubleShow ('e':'-':s)      = 'e' : '-' : reformatDoubleShow s
      reformatDoubleShow ('e':s)          = 'e' : '+' : reformatDoubleShow s
      reformatDoubleShow (c:s)            = c : reformatDoubleShow s
      reformatDoubleShow []               = []

formatSciExponentBuilder :: Scientific -> Builder
formatSciExponentBuilder n = let
  neg = n < 0
  (is, e) = toDecimalDigits $ if neg then -n else n
  ds = map (head . show) is
  show_e' = intDec (e-1)
  eNeg = fromEnum '-' == fromEnum (BS.head $ toLazyByteString show_e')
  showE = if eNeg then show_e' else char8 '+' <> show_e'
  ret = case ds of
    "0"     -> byteStringCopy "0"
    [d]     -> char8 d <> byteStringCopy "e" <> showE
    [d,'0'] -> char8 d <> char8 'e' <> showE
    (d:ds') -> char8 d <> char8 '.' <> string8 ds' <> char8 'e' <> showE
    []      -> error "formatSciExponentBuilder"
  in if neg then char8 '-' <> ret else ret

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
  let
    quotedK = "\"" <> quote k <> "\""
    keyEnc = if pColorize then cBBlue <> quotedK <> cReset else quotedK
  in keyEnc <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState { pLevel, pIndent } = mconcat (replicate pLevel pIndent)

fromString :: PState -> Text -> Builder
fromString PState { pColorize } s =
  let enc = charUtf8 '"' <> quote s <> charUtf8 '"' in
    if pColorize then cGreen <> enc <> cReset else enc

quote :: Text -> Builder
quote s = case T.uncons t of
  Nothing     -> encodeUtf8Builder q
  Just (c,t') -> encodeUtf8Builder q <> (c `seq` escape c) <> quote t'
  where
    (q,t) = T.break isEscape s
    isEscape c =
      c == '\"' ||
      c == '\\' ||
      c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\b' = "\\b"
    escape '\f' = "\\f"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | c < '\x20' = "\\u" <> pad <> stringUtf8 h
      | otherwise  = charUtf8 c
      where
        h   = showHex (fromEnum c) ""
        pad = byteString $ BSS.replicate (4 - length h) (toEnum $ fromEnum '0')
