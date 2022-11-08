-- Usefull functions when defining parsing rules in parser engine --
module Data.Parser.Build.Parsing (
  parseError
) where

import Data.Parser.Build.Parser (Parser, parserFail, ParserPos (..), parserGetLexInput)

import qualified Data.Text as T

parseError :: Show token => token -> Parser token a
parseError _ = do
  (ParserPos line column, _, _) <- parserGetLexInput
  parserFail
    $  "syntax error at line "
    <> T.pack (show line)
    <> ", column "
    <> T.pack (show column)
