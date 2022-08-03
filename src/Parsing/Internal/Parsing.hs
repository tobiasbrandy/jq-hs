module Parsing.Internal.Parsing (
  parseError
, untok
, andPushTok
) where

import Parsing.Defs (Lex, lexFail, LexPos (..), lexGetInput, lexPushTok, lexShowState)

import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: Show token => token -> Lex token a
parseError _ = do
  (LexPos line column, _, _) <- lexGetInput
  state <- lexShowState
  lexFail $ "Parse error at line " <> TS.showt line <> ", column " <> TS.showt column <> ". State: " <> state

untok :: token -> (token -> a) -> a
untok t f = f t

andPushTok :: result -> token -> Lex token result
result `andPushTok` token = do lexPushTok token; return result
