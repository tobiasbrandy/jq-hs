{
{-# LANGUAGE NoStrictData #-}
module Parse.Json.Parser (jsonParser) where

import Parse.Defs (Parser, parserPushTok)
import Parse.Json.Lexer (lexer)
import Parse.Json.Tokens (JsonToken)
import qualified Parse.Json.Tokens as T
import Parse.Internal.Parsing (parseError)

import Data.Json (Json (..))

import Data.Text (Text)
import Data.Scientific (Scientific)

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad (when)
}

-- Name of parser and first non-terminal. The parser is partial to allow parsing multiple jsons in the same string.
%partial jsonParser Json

-- Tokens type
%tokentype { JsonToken }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { Parser JsonToken }

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

  -- Strings
  lq        { T.LQuote     }
  rq        { T.RQuote     }

%%

Json :: { Json }            -- Save last token, as it is the first of the next parsing
  : Element                 {%^ \tok -> do when (tk /= T.EOF) $ parserPushTok tok; return $1 }

Object :: { HashMap Text Json }
  : '{' '}'                 { Map.empty                 }
  | '{' Members '}'         { $2                        }

Members :: { HashMap Text Json }
  : Member                  { Map.singleton (fst $1) (snd $1) }
  | Members ',' Member      { Map.insert (fst $3) (snd $3) $1 }

Member :: { (Text, Json) }
  : String ':' Element      { ($1, $3)                  }

Array :: { Seq Json }
  : '[' ']'                 { Seq.empty                 }
  | '[' Elements ']'        { $2                        }

Elements :: { Seq Json }
  : Element                 { Seq.singleton $1          }
  | Elements ',' Element    { $1 :|> $3                 }

Element :: { Json }
  : Value                   { $1                        }

String :: { Text }
  : lq rq                   { ""                        }
  | lq string rq            { untokStr $2               }

Value :: { Json }
  : Object                  { Object $1                 }
  | Array                   { Array $1                  }
  | String                  { String $1                 }
  | number                  { Number $ untokNum $1      }
  | true                    { Bool True                 }
  | false                   { Bool False                }
  | null                    { Null                      }

 -- Convinience functions --
{
untokStr :: JsonToken -> Text
untokStr (T.Str s)  = s
untokStr x          = error "Not a string token"

untokNum :: JsonToken -> Scientific
untokNum (T.Num n)  = n
untokNum x          = error "Not a number token"
}
