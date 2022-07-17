{
-- Lexer engine
module Lexer.Engine (lexer) where

import Lexer.Tokens (Token (..))
import Lexer.Defs (Lex, lexError, LexAction, LexPos (..), LexInput, lexGetInput, lexSetInput, StartCode, lexGetStartCode, lexSetStartCode)
import Lexer.Internal (tok, textTok, strTok, numTok)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BS (w2c) -- Should be a nop

import Data.Word (Word8)
}

%action "LexAction Token"
%encoding "utf8"

@id     = ([a-zA-Z_][a-zA-Z_0-9]*::)*[a-zA-Z_][a-zA-Z_0-9]*
@field  = \.[a-zA-Z_][a-zA-Z_0-9]* 
@string = \"(\\([\"\\\/bfnrt]|u[a-fA-F0-9]{4})|[^\"\\\0-\x1F\x7F]+)\"
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
<0>         "//"  { begin l_comment }
<l_comment> .     ;
<l_comment> \n    { begin 0         }

-- Identifiers
<0> @id       { textTok Id        }
<0> @field    { textTok Field     }

-- Literals
<0> @string   { strTok String     }
<0> @number   { numTok Number     }

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
<0> "}"       { tok RBrac         }
<0> ":"       { tok KVDelim       }

-- Params
<0> ";"       { tok KVDelim       }

-- Variables
<0> "$"       { tok Var           }

-- Strings
<0> "\""      { tok Quote         }

{

-- Alex interface definition requirements --

type AlexInput = LexInput

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_, prev, _, _) = prev

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (pos, _, bs, n) =
  case BS.uncons bs of
    Nothing -> Nothing
    Just (b, bs') ->
      let
        c     = BS.w2c b
        pos'  = lexMovePos pos c
        n'    = n+1
      in pos' `seq` bs' `seq` n' `seq` Just (b, (pos', c, bs', n'))

-- Auxiliary functions --

-- Ignore this token and set the start code to a new value
begin :: StartCode -> LexAction Token
begin code _input _len = do 
  lexSetStartCode code
  lexer

-- Get updated lex position with new char
lexMovePos :: LexPos -> Char -> LexPos
lexMovePos (LexPos a l c) '\t' = LexPos (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
lexMovePos (LexPos a l _) '\n' = LexPos (a+1) (l+1)   1
lexMovePos (LexPos a l c) _    = LexPos (a+1)  l     (c+1)

-- Alex provided funtions and definitions

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

-- Main driver of lexer engine --
lexer :: Lex Token
lexer = do
  inp@(_, _, _, n) <- lexGetInput
  sc <- lexGetStartCode
  case alexScan inp sc of
    AlexEOF -> return EOF
    AlexError input -> lexError input
    AlexSkip  inp' _ -> do
      lexSetInput inp'
      lexer
    AlexToken inp'@(_, _, _, n') _ action -> let len = n'-n in do
      lexSetInput inp'
      action (ignorePendingBytes inp) len
}
