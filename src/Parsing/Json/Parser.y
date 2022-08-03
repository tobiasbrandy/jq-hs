{
module Parsing.Json.Parser (parseJson) where

import Parsing.Defs (Lex, lexPushTok)
import Parsing.Json.Lexer (lexer)
import Parsing.Json.Tokens (Token)
import qualified Parsing.Json.Tokens as T (Token (..))
import Parsing.Internal.Parsing (parseError, untok)

import Json (Json (..))

import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import  Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (when)
}

-- Name of parser and first non-terminal. The parser is partial to allow parsing multiple jsons in the same string.
%partial parseJson Json

-- Tokens type
%tokentype { Token }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { Lex Token }

-- Lexer function to use. We need to wrap it to interface with Happy. Also we indicate the EOF token
%lexer { (lexer >>=) } { T.EOF }

-- Don't allow shift/reduce conflicts
%expect 0

%token
  -- Literals
  string    { T.Str _     }
  number    { T.Num _     }
  true      { T.True      }
  false     { T.False     }
  null      { T.Null      }

  -- Lists
  '['       { T.LBrack    }
  ']'       { T.RBrack    }
  ','       { T.Comma     }

  -- Objects
  '{'       { T.LBrace    }
  '}'       { T.RBrace    }
  ':'       { T.KVDelim   }

%%

Json :: { Json }
  : Element                 {%^ \tok -> do when (tk /= T.EOF) $ lexPushTok tok; return $1 }

Object :: { Map Text Json }
  : '{' '}'                 { Map.empty                 }
  | '{' Members '}'         { Map.fromList $2           }

Members :: { [(Text, Json)] }
  : Member                  { [$1]                      }
  | Members ',' Member      { $3 : $1                   }

Member :: { (Text, Json) }
  : string ':' Element      { untok $1 (\(T.Str s) -> (s, $3)) }

Array :: { Seq Json }
  : '[' ']'                 { Seq.empty                 }
  | '[' Elements ']'        { Seq.fromList $ reverse $2 }

Elements :: { [Json] }
  : Element                 { [$1]                      }
  | Elements ',' Element    { $3 : $1                   }

Element :: { Json }
  : Value                   { $1                        }

Value :: { Json }
  : Object                  { Object $1                 }
  | Array                   { Array $1                  }
  | string                  { untok $1 (\(T.Str s) -> String s) }
  | number                  { untok $1 (\(T.Num n) -> Number n) }
  | true                    { Bool True                 }
  | false                   { Bool False                }
  | null                    { Null                      }
