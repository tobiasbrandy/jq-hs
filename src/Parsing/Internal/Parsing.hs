module Parsing.Internal.Parsing (
  parseError
, unTok
) where

import Parsing.Defs (Lex, lexFail, LexPos (..), lexGetInput)

import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: token -> Lex a
parseError _ = do
  (LexPos _ line column, _, _, _) <- lexGetInput
  lexFail $ "Parse error at line " <> TS.showt line <> ", column " <> TS.showt column

unTok :: token -> (token -> a) -> a
unTok t f = f t
