-- Usefull functions when defining parsing rules in parser engine --
module Data.Parser.Build.Parsing (
  parseError
) where

import Data.Parser.Build.Parser (Parser, parserFail, ParserPos (..), parserGetLexInput)

import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

parseError :: Show token => token -> Parser token a
parseError _ = do
  (ParserPos line column, _, inp) <- parserGetLexInput
  parserFail
    $  "parser error at line "
    <> T.pack (show line)
    <> ", column "
    <> T.pack (show column)
    <> ". Next: "
    <> decodeUtf8With lenientDecode (BS.toStrict $ BS.take 20 inp)
