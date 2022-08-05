-- Usefull functions when defining parsing rules in parser engine --
module Parse.Internal.Parsing (
  parseError
, untok
) where

import Parse.Defs (Parser, parserFail, ParserPos (..), parserGetLexInput, parserShowState)

import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: Show token => token -> Parser token a
parseError _ = do
  (ParserPos line column, _, _) <- parserGetLexInput
  state <- parserShowState
  parserFail $ "Parse error at line " <> TS.showt line <> ", column " <> TS.showt column <> ". State: " <> state

untok :: token -> (token -> a) -> a
untok t f = f t