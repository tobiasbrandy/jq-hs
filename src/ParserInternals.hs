module ParserInternals (
  parseError
, unTok
) where

import Lexer.Defs (Lex, lexFail, LexPos (..), lexGetInput)
import Lexer.Tokens (Token)

import qualified TextShow as TS

-- TODO(tobi): Better error
parseError :: Token -> Lex a
parseError _ = do
  (LexPos _ line column, _, _, _) <- lexGetInput
  lexFail $ "Parse error at line " <> TS.showt line <> ", column " <> TS.showt column

unTok :: Token -> (Token -> a) -> a
unTok t f = f t