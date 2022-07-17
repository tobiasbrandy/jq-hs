module Lexer.Defs (
  StartCode

, LexSize

, LexInput

, LexPos (..)

, Lex
, lexRun
, lexFail
, lexError
, lexGetInput
, lexSetInput
, lexGetStartCode
, lexSetStartCode

, LexAction
) where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import Control.Monad (liftM, ap)
import Data.Text (Text)
import qualified TextShow as TS

-- Type of the start codes used by the engine
type StartCode  = Int

-- Type of the variable holding bytes consumed
type LexSize    = Int64

-- Interface data type
type LexInput = (
  LexPos,       -- current position,
  Char,         -- previous char
  ByteString,   -- current input string
  LexSize       -- bytes consumed so far
  )

-- Lexer position
data LexPos = LexPos {
  l_off     :: !Int,    -- absolute character offset
  l_line    :: !Int,    -- line number
  l_col     :: !Int     -- column number
} deriving (Eq, Show)

lexInitPos :: LexPos
lexInitPos = LexPos { 
  l_off   = 0,
  l_line  = 1,
  l_col   = 1
}

-- Lexer state
data LexState = LexState {
  l_pos   :: !LexPos,     -- position at current input location
  l_bpos  :: !LexSize,    -- bytes consumed so far
  l_input :: ByteString,  -- the current input
  l_prev  :: !Char,       -- the character before the input
  l_code  :: !StartCode   -- the current startcode
}

lexInitState :: ByteString -> LexState
lexInitState input = LexState {
  l_pos   = lexInitPos,
  l_bpos  = 0,
  l_input = input,
  l_prev  = '\n',
  l_code  = 0
}

-- Lexer monad and main interface
newtype Lex a = Lex (LexState -> Either Text (LexState, a))

instance Functor Lex where
  fmap = liftM

instance Applicative Lex where
  pure x = Lex (\s -> Right (s, x))

  (<*>) = ap

instance Monad Lex where
  m >>= k = Lex $ \s -> 
    let Lex f = m in
      case f s of
        Left msg      -> Left msg
        Right (s', a) -> let Lex f' = k a in f' s'

-- Execute lexer
lexRun :: ByteString -> Lex a -> Either Text a
lexRun input (Lex f) = case f (lexInitState input) of
  Left msg      -> Left msg
  Right (_, x)  -> Right x

-- 
lexFail :: Text -> Lex a
lexFail msg = Lex $ const $ Left msg

-- Lex error handle
lexError :: LexInput -> Lex a
lexError (LexPos _ line column, _, _, _) = lexFail $ "lexical error at line " <> TS.showt line <> ", column " <> TS.showt column

-- Retrieves input from state
lexGetInput :: Lex LexInput
lexGetInput = Lex $ \s@LexState { l_pos, l_bpos, l_prev, l_input } -> Right (s, (l_pos, l_prev, l_input, l_bpos))

-- Updates state with input
lexSetInput :: LexInput -> Lex ()
lexSetInput (pos, prev, input, bpos) = Lex $ \s -> let state = s { 
  l_pos   = pos,
  l_bpos  = bpos,
  l_prev  = prev,
  l_input = input
} in Right (state, ())

-- Retrieves current startcode from state
lexGetStartCode :: Lex StartCode
lexGetStartCode = Lex $ \s@LexState{ l_code } -> Right (s, l_code)

-- Updates state with startcode
lexSetStartCode :: StartCode -> Lex ()
lexSetStartCode code = Lex $ \s -> let state = s { l_code = code } in Right (state, ())

type LexAction result = LexInput -> LexSize -> Lex result
