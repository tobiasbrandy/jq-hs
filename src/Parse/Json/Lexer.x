-- Lexer engine --
{
module Parse.Json.Lexer where

import Parse.Json.Tokens (Token (..))
import qualified Parse.Json.Tokens as T
import Parse.Defs (Parser, parserGetLexInput, parserSetLexInput, parserGetStartCode)
import Parse.Internal.Lexing (LexAction, lexError, tok, strTok, numTok, lexPushedToksThen)
import Parse.Internal.AlexIntegration (AlexInput, alexGetByte)
}

%action "LexAction Token"
%encoding "utf8"

-- From https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string
@string = \"(\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})|[^\"\\\0-\x1F\x7F]+)*\"
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
lexer :: Parser Token Token
lexer = lexPushedToksThen $ do
  inp@(_, n, _) <- parserGetLexInput
  sc <- parserGetStartCode
  case alexScan inp sc of
    AlexEOF -> return EOF
    AlexError input -> lexError input
    AlexSkip  inp' _ -> do
      parserSetLexInput inp'
      lexer
    AlexToken inp'@(_, n', _) _ action -> let len = n'-n in do
      parserSetLexInput inp'
      action inp len

}
