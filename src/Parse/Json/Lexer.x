-- Lexer engine --
{
module Parse.Json.Lexer where

import Parse.Json.Tokens (JsonToken (..), strBuilderAppend, strBuilderToStr)
import qualified Parse.Json.Tokens as T
import Parse.Defs (Parser, parserGetLexInput, parserSetLexInput, parserGetStartCode)
import Parse.Internal.Lexing (LexAction, lexError, tok, strTokBuilder, escapedStrTokBuilder, numTok,
 andBegin, lexStartTokBuilderAndThen, lexFinishTokBuilderAndThen, lexPushedToksThen
 )
import Parse.Internal.AlexIntegration (AlexInput, alexGetByte)
}

%action "LexAction JsonToken"
%encoding "utf8"

-- For some reason, you can't directly use it like "\"", because alex gets confused.
@quote = \"

-- Inspired by https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string
@string   = [^\"\\\0-\x1F\x7F]+
@escaped  = \\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})
@number   = \-?([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?

tokens :-

-- Ignore whitespace
<0> $white+ ;

-- String
<0>   @quote    { lexStartTokBuilderAndThen (StrBuilder "") $ LQuote `andBegin` str }
<str> @string   { strTokBuilder lexer strBuilderAppend } 
<str> @escaped  { escapedStrTokBuilder lexer strBuilderAppend } 
<str> @quote    { lexFinishTokBuilderAndThen strBuilderToStr (RQuote `andBegin` 0) }

-- Literals
<0> @number     { numTok Num  }
<0> "true"      { tok T.True  }
<0> "false"     { tok T.False }
<0> "null"      { tok Null    }

-- Lists
<0> "["         { tok LBrack  }
<0> "]"         { tok RBrack  }
<0> ","         { tok Comma   }

-- Objects
<0> "{"         { tok LBrace  }
<0> "}"         { tok RBrace  }
<0> ":"         { tok KVDelim }

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
lexer :: Parser JsonToken JsonToken
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
