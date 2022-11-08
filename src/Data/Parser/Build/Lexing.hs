-- Usefull functions when defining lexing rules in lexer engine --
module Data.Parser.Build.Lexing (
  LexAction

, lexError

, tok
, strTok
, strTokBuilder
, escapedStrTokBuilder
, numTok

, andBeginCode
, andPopCode
, dropAndThen
, lexPushedToksThen
, lexStartTokBuilderAndThen
, lexFinishTokBuilderAndThen
) where

import Data.Parser.Build.Parser
  ( Parser
  , LexInput
  , ParserPos (..)
  , ParserSize
  , parserFail
  , parserPopTok
  , StartCode
  , parserPushStartCode
  , parserPopStartCode
  , parserPopTokBuilder
  , parserPushTokBuilder
  , parserPushTok
 )

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Char (chr)
import Data.Scientific (Scientific, scientificP)

import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Text.Lazy.Builder (Builder, fromText)
import qualified Data.Text.Lazy.Builder as BT
import Numeric (readHex)
import Data.Int (Int64)

type LexAction token = LexInput -> ParserSize -> Parser token token

-- Lex error handle
lexError :: LexInput -> Parser token a
lexError (ParserPos line column, _size, inp) = do
  parserFail
    $  "lexical error at line "
    <> T.pack (show line)
    <> ", column "
    <> T.pack (show column)
    <> ". Next: "
    <> decodeUtf8With lenientDecode (BS.toStrict $ BS.take 20 inp)

tok :: token -> LexAction token
tok t _ _ = return t

strTok :: (Text -> token) -> LexAction token
strTok f (_, _, str) len = return $ f $ decodeUtf8With lenientDecode $ BS.toStrict $ BS.take len str

strTokBuilder :: Parser token token -> (token -> Builder -> token) -> LexAction token
strTokBuilder lexer append = lexApTokBuilder lexer append (fromText . decodeUtf8With lenientDecode . BS.toStrict)

-- Precondition: Escaped json character
escapedStrTokBuilder :: Parser token token -> (token -> Builder -> token) -> LexAction token
escapedStrTokBuilder lexer append inp len = lexApTokBuilder lexer append unescape inp len
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
numTok f (_, _, str) len = return $ f $ fst $ last $ readP_to_S scientificP $ coerceDotDecimal $ map (chr . fromEnum) $ BS.unpack $ BS.take len str

coerceDotDecimal :: String -> String
coerceDotDecimal s@('.':_) = '0' : s
coerceDotDecimal s = s

andBeginCode :: token -> StartCode -> LexAction token
andBeginCode token code _ _ = do
  parserPushStartCode code
  return token

andPopCode :: token -> Text -> LexAction token
andPopCode token errMsg _ _ = do
  parserPopStartCode errMsg
  return token

dropAndThen :: Int64 -> LexAction token -> LexAction token
dropAndThen n after (a, b, str) len = do
  after (a, b, BS.drop n str) (len-1)

lexPushedToksThen :: Parser token token -> Parser token token
lexPushedToksThen after = do
  mt <- parserPopTok
  maybe after return mt

lexApTokBuilder :: Parser token token -> (token -> builder -> token) -> (ByteString -> builder) -> LexAction token
lexApTokBuilder lexer append parse (_, _, str) len = do
  current <- parserPopTokBuilder
  case current of
    Nothing -> error "lexApTokBuilder: No token to append available"
    Just token -> do
      parserPushTokBuilder $ append token $ parse $ BS.take len str
      lexer

lexStartTokBuilderAndThen :: token -> LexAction token -> LexAction token
lexStartTokBuilderAndThen token nextTokAction inp len = do
  parserPushTokBuilder token
  nextTokAction inp len

lexFinishTokBuilderAndThen :: (token -> token) -> LexAction token -> LexAction token
lexFinishTokBuilderAndThen f nextTokAction inp len = do
  current <- parserPopTokBuilder
  case current of
    Nothing -> nextTokAction inp len
    Just token -> do
      nextTok <- nextTokAction inp len
      parserPushTok nextTok
      return $ f token
