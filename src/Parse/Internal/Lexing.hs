-- Usefull functions when defining lexing rules in lexer engine --
module Parse.Internal.Lexing (
  LexAction
, lexError
, tok
, textTok
, strTok
, numTok
, lexPushedToksThen
) where

import Parse.Defs (Parser, LexInput, ParserPos (..), ParserSize, parserFail, parserPopTok)

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified TextShow as TS
import Data.Char (chr)
import Data.Scientific (Scientific)

type LexAction token = LexInput -> ParserSize -> Parser token token

-- Lex error handle
lexError :: LexInput -> Parser token a
lexError (ParserPos line column, _size, inp) = parserFail
  $  "lexical error at line "
  <> TS.showt line
  <> ", column "
  <> TS.showt column
  <> ". Next: "
  <> decodeUtf8 (BS.toStrict $ BS.take 20 inp)

tok :: token -> LexAction token
tok t _ _ = return t

textTok :: (Text -> token) -> LexAction token
textTok f (_, _, str) len = return $ f $ decodeUtf8 $ BS.toStrict $ BS.take len str

-- Same as textTok but for json strings. Drops quotes and parses escaped chars. TODO(tobi): parse escaped chars
strTok :: (Text -> token) -> LexAction token
strTok f (_, _, str) len = return $ f $ decodeUtf8 $ BS.toStrict $ BS.take (len-2) $ BS.drop 1 str

numTok :: (Scientific -> token) -> LexAction token
numTok f (_, _, str) len = return $ f $ read $ map (chr . fromEnum) . BS.unpack $ BS.take len str

lexPushedToksThen :: Parser token token -> Parser token token
lexPushedToksThen after = do
  mt <- parserPopTok
  maybe after return mt
