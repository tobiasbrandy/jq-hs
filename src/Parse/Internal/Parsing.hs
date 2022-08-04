module Parse.Internal.Parsing (
  parseError
, untok
) where

import Parse.Defs (Lex, lexFail, LexPos (..), lexGetInput, lexShowState)

import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: Show token => token -> Lex token a
parseError _ = do
  (LexPos line column, _, _) <- lexGetInput
  state <- lexShowState
  lexFail $ "Parse error at line " <> TS.showt line <> ", column " <> TS.showt column <> ". State: " <> state

untok :: token -> (token -> a) -> a
untok t f = f t
