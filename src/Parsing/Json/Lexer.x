-- Lexer engine
{
module Parsing.Json.Lexer where

import Parsing.Json.Tokens (Token (..))
import qualified Parsing.Json.Tokens as T
import Parsing.Defs (Lex, lexError, lexPopTok, lexGetInput, lexSetInput, lexGetStartCode, LexAction)
import Parsing.Internal.Lexing (tok, strTok, numTok)
import Parsing.Internal.AlexIntegration (AlexInput, alexGetByte)
}

%action "LexAction Token"
%encoding "utf8"

@string = \"(\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})|[^\"\\\0-\x1F\x7F]+)\"
@number = \-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?

tokens :-

-- Ignore whitespace
<0> $white+ ;

-- Literals
<0> @string   { strTok Str  }
<0> @number   { numTok Num  }
<0> "true"    { tok T.True  }
<0> "false"   { tok T.False }
<0> "null"    { tok Null    }

-- Lists
<0> "["       { tok LBrack  }
<0> "]"       { tok RBrack  }
<0> ","       { tok Comma   }

-- Objects
<0> "{"       { tok LBrace  }
<0> "}"       { tok RBrace  }
<0> ":"       { tok KVDelim }

-- Alex provided functions and definitions

-- alexScan :: AlexInput             -- The current input
--          -> Int                   -- The "start code"
--          -> AlexReturn action     -- The return value

-- data AlexReturn action
--   = AlexEOF

--   | AlexError
--       !AlexInput     -- Remaining input

--   | AlexSkip
--       !AlexInput     -- Remaining input
--       !Int           -- Token length

--   | AlexToken
--       !AlexInput     -- Remaining input
--       !Int           -- Token length
--       action         -- action value

{

-- Main driver of lexer engine --
lexer :: Lex Token Token
lexer = do
  mt <- lexPopTok
  case mt of
    Just t -> return t
    Nothing -> do
      inp@(_, n, _) <- lexGetInput
      sc <- lexGetStartCode
      case alexScan inp sc of
        AlexEOF -> return EOF
        AlexError input -> lexError input
        AlexSkip  inp' _ -> do
          lexSetInput inp'
          lexer
        AlexToken inp'@(_, n', _) _ action -> let len = n'-n in do
          lexSetInput inp'
          action inp len

}
