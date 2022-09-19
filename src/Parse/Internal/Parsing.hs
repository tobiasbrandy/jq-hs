-- Usefull functions when defining parsing rules in parser engine --
module Parse.Internal.Parsing (
  parseError
) where

import Parse.Defs (Parser, parserFail, ParserPos (..), parserGetLexInput, parserShowState)

import qualified Data.Text as T

-- TODO(tobi): Better error
parseError :: Show token => token -> Parser token a
parseError token = do
  (ParserPos line column, _, _) <- parserGetLexInput
  state <- parserShowState
  parserFail
    $  "Parse error at line " <> T.pack (show line)
    <> ", column " <> T.pack (show column)
    <> ", token " <> T.pack (show token)
    <> ". State: " <> state
