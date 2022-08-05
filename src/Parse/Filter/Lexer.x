-- Lexer engine --
{
module Parse.Filter.Lexer (lexer) where

import Parse.Filter.Tokens (Token (..))
import Parse.Defs (Parser, parserGetLexInput, parserSetLexInput, StartCode, parserGetStartCode, parserSetStartCode)
import Parse.Internal.Lexing (LexAction, lexError, tok, textTok, strTok, numTok)
import Parse.Internal.AlexIntegration (AlexInput, alexGetByte)
}

%action "LexAction Token"
%encoding "utf8"

@id     = ([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*
@field  = \.[a-zA-Z_][a-zA-Z_0-9]*

-- From https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string
@string = \"(\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})|[^\"\\\0-\x1F\x7F]+)*\"
@number = \-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][\+\-]?[0-9]+)?

tokens :-

-- Ignore whitespace
<0> $white+ ;

-- Block comment
<0>       "/*" { begin comment  }
<comment> "*/" { begin 0        }
<comment> .    ;
<comment> \n   ;

-- Line comment
<0>         "--"  { begin l_comment }
<l_comment> .     ;
<l_comment> \n    { begin 0         }

-- Identifiers
<0> @id       { textTok Id        }
<0> @field    { textTok Field     }

-- Literals
<0> @string   { strTok Str        }
<0> @number   { numTok Num        }

-- Keywords
<0> "module"  { tok Module        }
<0> "import"  { tok Import        }
<0> "include" { tok Include       }
<0> "def"     { tok Def           }
<0> "as"      { tok As            }
<0> "if"      { tok If            }
<0> "then"    { tok Then          }
<0> "else"    { tok Else          }
<0> "elif"    { tok Elif          }
<0> "end"     { tok End           }
<0> "reduce"  { tok Reduce        }
<0> "foreach" { tok Foreach       }
<0> "try"     { tok Try           }
<0> "catch"   { tok Catch         }
<0> "label"   { tok Label         }
<0> "break"   { tok Break         }
<0> "__loc__" { tok Loc           }

-- Arithmetic operators
<0> "+"       { tok Plus          }
<0> "-"       { tok Minus         }
<0> "*"       { tok Times         }
<0> "/"       { tok Div           }
<0> "%"       { tok Mod           }

-- Flow operators
<0> "|"       { tok Pipe          }
<0> "//"      { tok Alt           }
<0> "?"       { tok Opt           }
<0> ","       { tok Comma         }

-- Assignment operators
<0> "="       { tok Assign        }
<0> "+="      { tok PlusA         }
<0> "-="      { tok MinusA        }
<0> "*="      { tok TimesA        }
<0> "/="      { tok DivA          }
<0> "%="      { tok ModA          }
<0> "|="      { tok PipeA         }
<0> "//="     { tok AltA          }

-- Comparison operators
<0> "=="      { tok Eq            }
<0> "!="      { tok Neq           }
<0> "<"       { tok Lt            }
<0> "<="      { tok Le            }
<0> ">"       { tok Gt            }
<0> ">="      { tok Ge            }
<0> "or"      { tok Or            }
<0> "and"     { tok And           }

-- Special filters
<0> "."       { tok Dot           }
<0> ".."      { tok Recr          }

-- Parenthesis
<0> "("       { tok LPar          }
<0> ")"       { tok RPar          }

-- Lists
<0> "["       { tok LBrack        }
<0> "]"       { tok RBrack        }

-- Objects
<0> "{"       { tok LBrace        }
<0> "}"       { tok RBrace        }
<0> ":"       { tok KVDelim       }

-- Params
<0> ";"       { tok KVDelim       }

-- Variables
<0> "$"       { tok Var           }

-- Alex provided functions and definitions

-- alex_tab_size :: Int

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
lexer = do
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
begin :: StartCode -> LexAction Token
begin code _input _len = do 
  parserSetStartCode code
  lexer
}