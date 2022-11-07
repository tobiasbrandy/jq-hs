-- Simple Json (Pretty) Encoding
-- Inspired by:
-- - https://hackage.haskell.org/package/aeson-2.1.0.0/docs/src/Data.Aeson.Text.html
-- - https://hackage.haskell.org/package/aeson-pretty-0.8.9/docs/src/Data.Aeson.Encode.Pretty.html
module Data.Json.Encode (
  jsonEncode
, jsonEncodeToByteStringBuilder

, Colors (..)
, defaultColors
, parseColors

, Indent (..)

, Format (..)
, compactFormat
) where

import Data.Json (Json (..), JsonNum (..))

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
import Data.Char (intToDigit, isDigit)

-- Terminal Colors
cReset  :: Builder
cReset  = "\ESC[0m"

data Colors = Colors
  { cNull     :: Builder
  , cFalse    :: Builder
  , cTrue     :: Builder
  , cNumber   :: Builder
  , cString   :: Builder
  , cArray    :: Builder
  , cObject   :: Builder
  , cObjKey   :: Builder
}

defaultColors :: Colors
defaultColors = Colors
  { cNull     = "\ESC[1;30m"
  , cFalse    = "\ESC[0;39m"
  , cTrue     = "\ESC[0;39m"
  , cNumber   = "\ESC[0;39m"
  , cString   = "\ESC[0;32m"
  , cArray    = "\ESC[1;39m"
  , cObject   = "\ESC[1;39m"
  , cObjKey   = "\ESC[1;34m"
}

parseColors :: String -> Colors
parseColors s =
  if validColorStr s
  then 
    case map toEscSeq $ splitColors s of
      [null', false, true, numbres, strings, arrays, objects] -> Colors
        { cNull     = null'
        , cFalse    = false
        , cTrue     = true
        , cNumber   = numbres
        , cString   = strings
        , cArray    = arrays
        , cObject   = objects
        , cObjKey   = cObjKey defaultColors
      }
      _ -> defaultColors
  else defaultColors
  where
    validColorStr = all (\c -> isDigit c || c == ':' || c == ';')

    -- Stolen lines function from prelude
    splitColors "" =  []
    splitColors str = cons $ case break (== ':') str of
      (l, s') -> (l,) $ case s' of
        []    -> []
        _:s'' -> splitColors s''
      where
        cons ~(h, t) = h : t

    toEscSeq str = "\ESC[" <> string7 str <> "m"

data PState = PState
  { pLevel      :: Int
  , pIndent     :: Builder
  , pNewline    :: Builder
  , pItemSep    :: Builder
  , pKeyValSep  :: Builder
  , pSort       :: [(Text, Json)] -> [(Text, Json)]
  , pColors     :: Maybe Colors
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
  fmtColors           :: Maybe Colors,
  -- If the output is a string, don't quote it
  fmtRawStr           :: Bool,
  -- Whether to add a trailing newline to the output
  fmtTrailingNewline  :: Bool
}

-- Configuration for compact encoding
compactFormat :: Format
compactFormat = Format
  { fmtIndent           = Spaces 0
  , fmtCompare          = mempty
  , fmtColors           = Nothing
  , fmtRawStr           = False
  , fmtTrailingNewline  = False
}

-- | Encode json using format
jsonEncode :: Format -> Json -> ByteString
jsonEncode fmt = toLazyByteString . jsonEncodeToByteStringBuilder fmt

-- | Encode json to builder using format
jsonEncodeToByteStringBuilder :: Format -> Json -> Builder
jsonEncodeToByteStringBuilder Format {..} x
  | fmtRawStr && isStr x  = let String s = x in encodeUtf8Builder s <> trail
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
    st = PState
      { pLevel      = 0
      , pIndent     = indent
      , pNewline    = newline
      , pItemSep    = itemSep
      , pKeyValSep  = kvSep
      , pSort       = sortFn
      , pColors     = fmtColors
    }

fromValue :: PState -> Json -> Builder
fromValue st@PState { pSort, pColors } = go
  where
    go Null       = fromNull st
    go (Bool b)   = fromBool st b
    go (Number x) = fromNumber st x
    go (String s) = fromString st s
    go (Array v)  = fromCompound st (cArray  <$> pColors) ("[","]") fromValue (toList v)
    go (Object m) = fromCompound st (cObject <$> pColors) ("{","}") fromPair  (pSort (Map.toList m))

fromNull :: PState -> Builder
fromNull PState { pColors } =
  let enc = "null" in case pColors of
    Nothing -> enc
    Just Colors { cNull } -> cNull <> enc <> cReset

fromBool :: PState -> Bool -> Builder
fromBool PState { pColors } b = let enc = if b then "true" else "false" in case pColors of
  Nothing -> enc
  Just Colors { cTrue, cFalse } -> (if b then cTrue else cFalse) <> enc <> cReset

fromNumber :: PState -> JsonNum Scientific -> Builder
fromNumber PState { pColors } NaN = let enc = "null" in case pColors of
  Nothing -> enc
  Just Colors { cNumber } -> cNumber <> enc <> cReset
fromNumber st (Num x)
  | isInteger x = colorize st $
    if x < 1e23 && x > -1e23
    then formatScientificBuilder Fixed (Just 0) x
    else formatSciExponentBuilder x
  | otherwise = colorize st $ case toBoundedRealFloat x :: Either Double Double of
    Left e ->
      if e == 0
      then "0"
      else if e > 0 then "Infinity" else "-Infinity"
    Right n -> string7 $
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

      colorize PState { pColors } enc = case pColors of
        Nothing -> enc
        Just Colors { cNumber } -> cNumber <> enc <> cReset

formatSciExponentBuilder :: Scientific -> Builder
formatSciExponentBuilder n = let
  neg = n < 0
  (is, e) = toDecimalDigits $ if neg then -n else n
  ds = map intToDigit is
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

fromCompound :: PState -> Maybe Builder -> (Builder, Builder) -> (PState -> a -> Builder) -> [a] -> Builder
fromCompound st@PState { pLevel, pNewline, pItemSep } mColor (delimL, delimR) fromItem items = mconcat
  [ colorize delimL
  , if null items then mempty
      else pNewline <> items' <> pNewline <> fromIndent st
  , colorize delimR
  ]
  where
    colorize enc = case mColor of
      Nothing -> enc
      Just color -> color <> enc <> cReset

    items' = mconcat . intersperse (colorize pItemSep <> pNewline) $
      map (\item -> fromIndent st' <> fromItem st' item) items
    st' = st { pLevel = pLevel + 1 }

fromPair :: PState -> (Text, Json) -> Builder
fromPair st@PState { pColors } (k,v) =
  let
    keyEnc  = "\"" <> quote k <> "\""
    key     = case pColors of
      Nothing -> keyEnc
      Just Colors { cObjKey } -> cObjKey <> keyEnc <> cReset
  in key <> pKeyValSep st <> fromValue st v

fromIndent :: PState -> Builder
fromIndent PState { pLevel, pIndent } = mconcat (replicate pLevel pIndent)

fromString :: PState -> Text -> Builder
fromString PState { pColors } s =
  let enc = charUtf8 '"' <> quote s <> charUtf8 '"' in case pColors of
    Nothing -> enc
    Just Colors { cString } -> cString <> enc <> cReset

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
