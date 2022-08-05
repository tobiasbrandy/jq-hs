module Parse.Defs (
  StartCode

, ParserSize

, ParserPos (..)
, posForward

, LexInput

, ParserState
, parserStateInit

, ParserResult (..)

, Parser
, parserRun
, parserHasNext
, parserFail
, parserGetLexInput
, parserSetLexInput
, parserGetStartCode
, parserSetStartCode
, parserPopTok
, parserPushTok
, parserShowState
) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Int (Int64)
import Control.Monad (liftM, ap)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

-- Tab size
tabSize :: Int
tabSize = 8

-- Type of the start codes used by the lexer engine
type StartCode  = Int

-- Type of the variable holding bytes consumed
type ParserSize  = Int64

-- Parser position
data ParserPos = ParserPos {
  p_line    :: !Int,    -- line number
  p_col     :: !Int     -- column number
} deriving (Eq, Show)

parserInitPos :: ParserPos
parserInitPos = ParserPos {
  p_line  = 1,
  p_col   = 1
}

-- Move position forward
posForward :: ParserPos -> Char -> ParserPos
posForward (ParserPos l c) '\t' = ParserPos  l  (c + tabSize - ((c-1) `mod` tabSize))
posForward (ParserPos l _) '\n' = ParserPos (l+1)   1
posForward (ParserPos l c) _    = ParserPos  l     (c+1)

-- Interface data type for lexer
type LexInput = (
  ParserPos,    -- current position
  ParserSize,   -- bytes consumed so far
  ByteString    -- current input string
  )

-- Parser state
data ParserState token = ParserState {
  p_pos         :: !ParserPos,    -- position at current input location
  p_bpos        :: !ParserSize,   -- bytes consumed so far
  p_input       :: ByteString,    -- the current input
  p_pushedToks  :: [token],       -- tokens manually pushed by the user to be processed next
  p_code        :: !StartCode     -- the current startcode
} deriving (Show)

parserStateInit :: ByteString -> ParserState token
parserStateInit input = ParserState {
  p_pos         = parserInitPos,
  p_bpos        = 0,
  p_input       = input,
  p_pushedToks  = [],
  p_code        = 0
}

data ParserResult token result
  = Ok (ParserState token, result)
  | Error Text

-- Parser monad and main interface
newtype Parser token result = Parser (ParserState token -> ParserResult token result)

instance Functor (Parser token) where
  fmap = liftM

instance Applicative (Parser token) where
  pure x = Parser (\s -> Ok (s, x))

  (<*>) = ap

instance Monad (Parser token) where
  m >>= k = Parser $ \s -> 
    let Parser f = m in
      case f s of
        Error msg  -> Error msg
        Ok (s', a) -> let Parser f' = k a in f' s'

-- Execute parser
parserRun :: ParserState token -> Parser token result -> ParserResult token result
parserRun s (Parser f) = f s { p_code = 0 }

-- Parser has more input to parse
parserHasNext :: ParserState token -> Bool
parserHasNext (ParserState { p_input, p_pushedToks }) = not (null p_pushedToks && BS.null p_input)

-- Parser failure
parserFail :: Text -> Parser token result
parserFail msg = Parser $ const $ Error msg

-- Retrieves lexer input from state
parserGetLexInput :: Parser token LexInput
parserGetLexInput = Parser $ \s@ParserState { p_pos, p_bpos, p_input } -> Ok (s, (p_pos, p_bpos, p_input))

-- Updates state with lexer input
parserSetLexInput :: LexInput -> Parser token ()
parserSetLexInput (pos, bpos, input) = Parser $ \s -> let state = s { 
  p_pos         = pos,
  p_bpos        = bpos,
  p_input       = input
} in Ok (state, ())

-- Retrieves current startcode from state
parserGetStartCode :: Parser token StartCode
parserGetStartCode = Parser $ \s@ParserState{ p_code } -> Ok (s, p_code)

-- Updates state with startcode
parserSetStartCode :: StartCode -> Parser token ()
parserSetStartCode code = Parser $ \s -> Ok (s { p_code = code }, ())

-- Pops manually pushed token by the user
parserPopTok :: Parser token (Maybe token)
parserPopTok = Parser $ \s@ParserState{ p_pushedToks = toks } -> Ok (s { p_pushedToks = drop 1 toks }, listToMaybe toks)

-- Push token the user wishes to process next
parserPushTok :: token -> Parser token ()
parserPushTok tok = Parser $ \s@ParserState { p_pushedToks } -> Ok (s { p_pushedToks = tok : p_pushedToks }, ())

-- TODO(tobi): Debug purposes
parserShowState :: Show token => Parser token Text
parserShowState = Parser $ \s -> Ok (s, (T.pack . show) s)
