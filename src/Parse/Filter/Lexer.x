-- Lexer engine --
{
module Parse.Filter.Lexer (lexer) where

import Parse.Filter.Tokens (FilterToken (..), strBuilderAppend, strBuilderToStr)
import qualified Parse.Filter.Tokens as T

import Parse.Defs (Parser, parserGetLexInput, parserSetLexInput, StartCode, parserGetStartCode, parserPushStartCode)
import Parse.Internal.Lexing
  (LexAction
  , lexError
  , tok
  , strTok
  , strTokBuilder
  , escapedStrTokBuilder
  , numTok
  , lexPushedToksThen
  , andBeginCode
  , andPopCode
  , dropAndThen
  , lexStartTokBuilderAndThen
  , lexFinishTokBuilderAndThen
  )
import Parse.Internal.AlexIntegration (AlexInput, alexGetByte)

import qualified Data.Text as T (tail)
}

%action "LexAction FilterToken"
%encoding "utf8"

-- Special alex characters
@quote    = \"
@l_interp = \\"("
@r_interp = ")"

@id     = ([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*
@field  = \.[a-zA-Z_][a-zA-Z_0-9]*
@format = "@"[a-zA-Z0-9_]+

-- Inspired by https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string
@string   = [^\"\\\0-\x1F\x7F]+
@escaped  = \\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})
@number   = ([0-9]+\.?|[0-9]*\.[0-9]+)([eE][\+\-]?[0-9]+)?

tokens :-

-- Ignore whitespace
<0,i> $white+ ;

-- Line comment
<0,i>       "#" { begin comment     }
<comment> .   ;
<comment> \n  { begin 0           }
<comment> \r  { begin 0           }

-- Literals
<0,i> "true"    { tok T.True        }
<0,i> "false"   { tok T.False       }
<0,i> "null"    { tok T.Null        }
<0,i> @number   { numTok Num        }

-- Keywords
<0,i> "module"  { tok Module        }
<0,i> "import"  { tok Import        }
<0,i> "include" { tok Include       }
<0,i> "def"     { tok Def           }
<0,i> "as"      { tok As            }
<0,i> "if"      { tok If            }
<0,i> "then"    { tok Then          }
<0,i> "else"    { tok Else          }
<0,i> "elif"    { tok Elif          }
<0,i> "end"     { tok End           }
<0,i> "reduce"  { tok Reduce        }
<0,i> "foreach" { tok Foreach       }
<0,i> "try"     { tok Try           }
<0,i> "catch"   { tok Catch         }
<0,i> "label"   { tok Label         }
<0,i> "break"   { tok Break         }
<0,i> "__loc__" { tok Loc           }

-- Arithmetic operators
<0,i> "+"       { tok Plus          }
<0,i> "-"       { tok Minus         }
<0,i> "*"       { tok Times         }
<0,i> "/"       { tok Div           }
<0,i> "%"       { tok Mod           }

-- Flow operators
<0,i> "|"       { tok Pipe          }
<0,i> "//"      { tok Alt           }
<0,i> "?"       { tok Opt           }
<0,i> "?//"     { tok OptAlt        }
<0,i> ","       { tok Comma         }

-- Assignment operators
<0,i> "="       { tok Assign        }
<0,i> "+="      { tok PlusA         }
<0,i> "-="      { tok MinusA        }
<0,i> "*="      { tok TimesA        }
<0,i> "/="      { tok DivA          }
<0,i> "%="      { tok ModA          }
<0,i> "|="      { tok PipeA         }
<0,i> "//="     { tok AltA          }

-- Comparison operators
<0,i> "=="      { tok Eq            }
<0,i> "!="      { tok Neq           }
<0,i> "<"       { tok Lt            }
<0,i> "<="      { tok Le            }
<0,i> ">"       { tok Gt            }
<0,i> ">="      { tok Ge            }
<0,i> "or"      { tok Or            }
<0,i> "and"     { tok And           }

-- Special filters
<0,i> "."       { tok Dot           }
<0,i> ".."      { tok Recr          }

-- Parenthesis
<0,i> "("       { LPar `andBeginCode` 0 }
<0>   ")"       { RPar `andPopCode` "Missing opening parenthesis '('" }

-- Lists
<0,i> "["       { tok LBrack        }
<0,i> "]"       { tok RBrack        }

-- Objects
<0,i> "{"       { tok LBrace        }
<0,i> "}"       { tok RBrace        }
<0,i> ":"       { tok KVDelim       }

-- Params
<0,i> ";"       { tok ArgDelim      }

-- Variables
<0,i> "$"       { tok Var           }

-- String
<0,i> @quote    { lexStartTokBuilderAndThen (StrBuilder "") $ LQuote `andBeginCode` str }
<str> @string   { strTokBuilder lexer strBuilderAppend                                  }
<str> @escaped  { escapedStrTokBuilder lexer strBuilderAppend                           }
<str> @quote    { lexFinishTokBuilderAndThen strBuilderToStr $ RQuote `andPopCode` "Illegal state: close str" }

-- String Interpolation
<str> @l_interp { lexFinishTokBuilderAndThen strBuilderToStr $ LInterp `andBeginCode` i }
<i>   @r_interp { lexStartTokBuilderAndThen (StrBuilder "") $ RInterp `andPopCode` "Illegal state: close string interpolation" }

-- Identifiers
<0,i> @id       { strTok Id                    }
<0,i> @field    { dropAndThen 1 $ strTok Field }
<0,i> @format   { strTok (Format . T.tail)     }

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
lexer :: Parser FilterToken FilterToken
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


-- Auxiliary functions --

-- Ignore this token and set the start code to a new value
begin :: StartCode -> LexAction FilterToken
begin code _input _len = do 
  parserPushStartCode code
  lexer
}
