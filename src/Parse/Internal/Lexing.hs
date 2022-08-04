-- Usefull functions when defining lexing rules in lexer
module Parse.Internal.Lexing (
  lexError
, tok
, textTok
, strTok
, numTok
) where

import Parse.Defs (Lex, LexInput, LexAction, LexPos (..), lexFail)

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified TextShow as TS
import Data.Char (chr)
import Data.Scientific (Scientific)

-- Lex error handle
lexError :: LexInput -> Lex token a
lexError (LexPos line column, _size, inp) = lexFail
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
