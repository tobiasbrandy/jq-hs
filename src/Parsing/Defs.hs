module Parsing.Defs (
  StartCode

, LexSize

, LexInput

, LexPos (..)
, posForward

, LexState
, lexStateInit

, ParseResult (..)

, Lex
, lexRun
, lexHasNext
, lexFail
, lexError
, lexGetInput
, lexSetInput
, lexGetStartCode
, lexSetStartCode
, lexPopTok
, lexPushTok
, lexShowState

, LexAction
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Control.Monad (liftM, ap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified TextShow as TS
import Data.Maybe (listToMaybe)
import Parsing.Json.Tokens (Token(..))

-- Tab size
tabSize :: Int
tabSize = 8

-- Type of the start codes used by the engine
type StartCode  = Int

-- Type of the variable holding bytes consumed
type LexSize    = Int64

-- Lexer position
data LexPos = LexPos {
  l_line    :: !Int,    -- line number
  l_col     :: !Int     -- column number
} deriving (Eq, Show)

lexInitPos :: LexPos
lexInitPos = LexPos {
  l_line  = 1,
  l_col   = 1
}

-- Move position forward
posForward :: LexPos -> Char -> LexPos
posForward (LexPos l c) '\t' = LexPos  l  (c + tabSize - ((c-1) `mod` tabSize))
posForward (LexPos l _) '\n' = LexPos (l+1)   1
posForward (LexPos l c) _    = LexPos  l     (c+1)

-- Interface data type
type LexInput = (
  LexPos,       -- current position
  LexSize,      -- bytes consumed so far
  ByteString    -- current input string
  )

-- Lexer state
data LexState token = LexState {
  l_pos         :: !LexPos,     -- position at current input location
  l_bpos        :: !LexSize,    -- bytes consumed so far
  l_input       :: ByteString,  -- the current input
  l_pushedToks  :: [token],     -- tokens manually pushed by the user to be processed next
  l_code        :: !StartCode   -- the current startcode
} deriving (Show)

lexStateInit :: ByteString -> LexState Token
lexStateInit input = LexState {
  l_pos         = lexInitPos,
  l_bpos        = 0,
  l_input       = input,
  l_pushedToks  = [],
  l_code        = 0
}

data ParseResult token result
  = Ok (LexState token, result)
  | Error Text

-- Lexer monad and main interface
newtype Lex token result = Lex (LexState token -> ParseResult token result)

instance Functor (Lex token) where
  fmap = liftM

instance Applicative (Lex token) where
  pure x = Lex (\s -> Ok (s, x))

  (<*>) = ap

instance Monad (Lex token) where
  m >>= k = Lex $ \s -> 
    let Lex f = m in
      case f s of
        Error msg  -> Error msg
        Ok (s', a) -> let Lex f' = k a in f' s'

-- Execute lexer
lexRun :: LexState token -> Lex token result -> ParseResult token result
lexRun s (Lex f) = f s { l_code = 0 }

lexHasNext :: LexState token -> Bool
lexHasNext (LexState { l_input, l_pushedToks }) = not (null l_pushedToks && BS.null l_input)

-- 
lexFail :: Text -> Lex token result
lexFail msg = Lex $ const $ Error msg

-- Lex error handle
lexError :: LexInput -> Lex token a
lexError (LexPos line column, _, _) = lexFail $ "lexical error at line " <> TS.showt line <> ", column " <> TS.showt column

-- Retrieves input from state
lexGetInput :: Lex token LexInput
lexGetInput = Lex $ \s@LexState { l_pos, l_bpos, l_input } -> Ok (s, (l_pos, l_bpos, l_input))

-- Updates state with input
lexSetInput :: LexInput -> Lex token ()
lexSetInput (pos, bpos, input) = Lex $ \s -> let state = s { 
  l_pos         = pos,
  l_bpos        = bpos,
  l_input       = input
} in Ok (state, ())

-- Retrieves current startcode from state
lexGetStartCode :: Lex token StartCode
lexGetStartCode = Lex $ \s@LexState{ l_code } -> Ok (s, l_code)

-- Updates state with startcode
lexSetStartCode :: StartCode -> Lex token ()
lexSetStartCode code = Lex $ \s -> Ok (s { l_code = code }, ())

-- Pops manually pushed token by the user
lexPopTok :: Lex token (Maybe token)
lexPopTok = Lex $ \s@LexState{ l_pushedToks = toks } -> Ok (s { l_pushedToks = drop 1 toks }, listToMaybe toks)

-- Push token the user wishes to process next
lexPushTok :: token -> Lex token ()
lexPushTok tok = Lex $ \s@LexState { l_pushedToks } -> Ok (s { l_pushedToks = tok : l_pushedToks }, ())

-- TODO(tobi): Debug purposes
lexShowState :: Show token => Lex token Text
lexShowState = Lex $ \s -> Ok (s, (T.pack . show) s)

type LexAction token = LexInput -> LexSize -> Lex token token
