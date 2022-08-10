-- Usefull functions when defining lexing rules in lexer engine --
module Parse.Internal.Lexing (
  LexAction

, lexError

, tok
, strTok
, strBuilderTok
, escapedStrBuilderTok
, numTok

, andBegin
, lexPushedToksThen
, lexFinishTokAndThen

, builderToText
) where

import Parse.Defs (Parser, LexInput, ParserPos (..), ParserSize, parserFail, parserPopTok,
 StartCode, parserSetStartCode, parserPopUnfinishedTok, parserSetUnfinishedTok, parserPushTok, parserShowState
 )

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified TextShow as TS
import Data.Char (chr)
import Data.Scientific (Scientific, scientificP)

import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.Builder as BT
import Data.Text.Lazy (toStrict)
import Numeric (readHex)

type LexAction token = LexInput -> ParserSize -> Parser token token

-- Lex error handle
lexError :: Show token => LexInput -> Parser token a
lexError (ParserPos line column, _size, inp) = do
  state <- parserShowState
  parserFail
    $  "lexical error at line "
    <> TS.showt line
    <> ", column "
    <> TS.showt column
    <> ". Next: "
    <> decodeUtf8 (BS.toStrict $ BS.take 20 inp)
    <> ". State: "
    <> state

tok :: token -> LexAction token
tok t _ _ = return t

strTok :: (Text -> token) -> LexAction token
strTok f (_, _, str) len = return $ f $ decodeUtf8 $ BS.toStrict $ BS.take len str

strBuilderTok :: Parser token token -> (Builder -> token) -> (token -> Builder) -> LexAction token
strBuilderTok lexer returnTok untok = lexApUnfinishedTok lexer returnTok ((<>) . untok) (fromText . decodeUtf8 . BS.toStrict)

-- Precondition: Escaped json character
escapedStrBuilderTok :: Parser token token -> (Builder -> token) -> (token -> Builder) -> LexAction token
escapedStrBuilderTok lexer returnTok untok inp len = lexApUnfinishedTok lexer returnTok ((<>) . untok) unescape inp len
  where
    unescape "\\\"" = "\""
    unescape "\\\\" = "\\"
    unescape "\\/"  = "\\"
    unescape "\\b"  = "\b"
    unescape "\\f"  = "\f"
    unescape "\\n"  = "\n"
    unescape "\\r"  = "\r"
    unescape "\\t"  = "\t"
    unescape s
      | len == 6 && BS.take 2 s == "\\u"  = BT.singleton $ chr $ fst $ last $ readHex $ map (chr . fromEnum) $ BS.unpack $ BS.drop 2 s
      | otherwise = error $ map (chr . fromEnum) $ BS.unpack $ "Not a valid JSON escaped character: " <> s

numTok :: (Scientific -> token) -> LexAction token
numTok f (_, _, str) len = return $ f $ fst $ last $ readP_to_S scientificP $ map (chr . fromEnum) $ BS.unpack $ BS.take len str

andBegin :: token -> StartCode -> LexAction token
andBegin token code _ _ = do
  parserSetStartCode code
  return token

lexPushedToksThen :: Parser token token -> Parser token token
lexPushedToksThen after = do
  mt <- parserPopTok
  maybe after return mt

lexApUnfinishedTok :: Parser token token -> (builder -> token) -> (token -> builder -> builder) -> (ByteString -> builder) -> LexAction token
lexApUnfinishedTok lexer returnTok merge parse (_, _, str) len = do
  current <- parserPopUnfinishedTok
  case current of
    Nothing -> do
      parserSetUnfinishedTok $ returnTok $ parse $ BS.take len str
      lexer
    Just token -> do
      parserSetUnfinishedTok $ returnTok $ merge token $ parse $ BS.take len str
      lexer

lexFinishTokAndThen :: (token -> token) -> LexAction token -> LexAction token
lexFinishTokAndThen f nextTokAction inp len = do
  current <- parserPopUnfinishedTok
  case current of
    Nothing -> nextTokAction inp len
    Just token -> do
      nextTok <- nextTokAction inp len
      parserPushTok nextTok
      return $ f token

builderToText :: Builder -> Text
builderToText = toStrict . toLazyText
