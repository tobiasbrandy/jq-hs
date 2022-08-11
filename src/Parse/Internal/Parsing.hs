-- Usefull functions when defining parsing rules in parser engine --
module Parse.Internal.Parsing (
  parseError
) where

import Parse.Defs (Parser, parserFail, ParserPos (..), parserGetLexInput, parserShowState)

import qualified Data.Text as T
import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: Show token => token -> Parser token a
parseError token = do
  (ParserPos line column, _, _) <- parserGetLexInput
  state <- parserShowState
  parserFail
    $  "Parse error at line " <> TS.showt line
    <> ", column " <> TS.showt column
    <> ", token " <> T.pack (show token)
    <> ". State: " <> state
