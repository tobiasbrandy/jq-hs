{
module Parse.Json.Parser (parseJson) where

import Parse.Defs (Lex, lexPushTok)
import Parse.Json.Lexer (lexer)
import Parse.Json.Tokens (Token)
import qualified Parse.Json.Tokens as T (Token (..))
import Parse.Internal.Parsing (parseError, untok)

import Json (Json (..))

import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
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

Object :: { HashMap Text Json }
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
