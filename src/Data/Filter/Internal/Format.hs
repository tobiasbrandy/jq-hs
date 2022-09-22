module Data.Filter.Internal.Format (
  jsonFormat
, textFormat
, csvFormat
, tsvFormat
, htmlFormat
, uriFormat
, shFormat
, base64EncodeFormat
, base64DecodeFormat
) where

import Prelude hiding (any)

import Data.Json (Json (..), jsonShowType)
import Data.Json.Encode (Format(..), compactFormat, jsonEncode)

import Data.Filter.Internal.Result (FilterRet (..))
import Data.Filter.Internal.Sci (toFloatNum)

import Data.Text (Text)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8, decodeUtf8')
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import Data.Sequence (Seq)
import Data.Char (isAscii, isAlphaNum, chr, toUpper)
import Data.Word (Word8)
import Numeric (showHex)

import Data.Text.Encoding.Base64 (encodeBase64, decodeBase64With)

jsonFormat :: Json -> Text
jsonFormat = decodeUtf8With lenientDecode . BS.toStrict . jsonEncode compactFormat

textFormat :: Json -> Text
textFormat = decodeUtf8With lenientDecode . BS.toStrict . jsonEncode compactFormat { fmtRawStr = True }

csvFormat :: Seq Json -> FilterRet Text
csvFormat = fmap (T.intercalate ",") . sequence . foldr ((:) . run) []
  where
    run :: Json -> FilterRet Text
    run Null          = Ok ""
    run j@(Bool _)    = Ok $ jsonFormat j
    run j@(Number n)  = Ok $ if isNaN $ toFloatNum n then "" else jsonFormat j
    run (String s)    = Ok $ "\"" <> T.concatMap escape s <> "\""
    run any           = Err $ jsonShowType any <> " is not valid in a csv row"

    escape :: Char -> Text
    escape '\"' = "\"\""
    escape c    = T.singleton c

tsvFormat :: Seq Json -> FilterRet Text
tsvFormat = fmap (T.intercalate "\t") . sequence . foldr ((:) . run) []
  where
    run :: Json -> FilterRet Text
    run Null          = Ok ""
    run j@(Bool _)    = Ok $ jsonFormat j
    run j@(Number n)  = Ok $ if isNaN $ toFloatNum n then "" else jsonFormat j
    run (String s)    = Ok $ T.concatMap escape s
    run any           = Err $ jsonShowType any <> " is not valid in a tsv row"

    escape :: Char -> Text
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape '\n' = "\\n"
    escape '\\' = "\\\\"
    escape c    = T.singleton c

htmlFormat :: Json -> Text
htmlFormat = T.concatMap escape . textFormat
  where
    escape :: Char -> Text
    escape '&'  = "&amp;"
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '\'' = "&apos;"
    escape '\"' = "&quot;"
    escape c    = T.singleton c

uriFormat :: Json -> Text
uriFormat = T.concat . map escape . BSS.unpack . encodeUtf8 . textFormat
  where
    nonAlphaNumReserved :: BSS.ByteString
    nonAlphaNumReserved = "-_.!~*'()"

    escape :: Word8 -> Text
    escape w = let n = fromEnum w in
      if w < 128 && (
        BSS.any (== w) nonAlphaNumReserved
        || let c = chr n in isAscii c && isAlphaNum c
      )
      then T.singleton $ chr n
      -- Hexadecimal with capital letters and length 2
      else let hex = showHex n "" in T.pack $ '%' : map toUpper (if length hex == 1 then '0' : hex else hex)
        

shFormat :: Seq Json -> FilterRet Text
shFormat = fmap T.concat . sequence . foldr ((:) . run) []
  where
    run Null          = Ok $ jsonFormat Null
    run j@(Bool _)    = Ok $ jsonFormat j
    run j@(Number _)  = Ok $ jsonFormat j
    run (String s)    = Ok $ "\'" <> T.concatMap escape s <> "\'"
      where
        escape :: Char -> Text
        escape '\'' = "\'\\\'\'"
        escape c    = T.singleton c
    run any = Err $ jsonShowType any <> " can not be escaped for shell"

base64EncodeFormat :: Json -> Text
base64EncodeFormat = encodeBase64 . textFormat

base64DecodeFormat :: Json -> FilterRet Text
base64DecodeFormat json = let s = textFormat json in
  case decodeBase64With decodeUtf8' $ encodeUtf8 s of
    Left _    -> Err $ s <> " is not valid base64 data"
    Right ret -> Ok ret
