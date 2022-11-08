{
{-# LANGUAGE NoStrictData #-}
module Data.Json.Parsing.Parser (jsonParser) where

import Data.Parser.Build.Parser (Parser, parserPushTok)
import Data.Json.Parsing.Lexer (lexer)
import Data.Json.Parsing.Tokens (JsonToken)
import qualified Data.Json.Parsing.Tokens as T
import Data.Parser.Build.Parsing (parseError)
import Data.Parser.Build.Lexing (lexError)

import Data.Json (Json (..), JsonNum (..))

import Data.Text (Text)
import Data.Scientific (Scientific, fromFloatDigits)

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
%error { parseOrLexError }

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

  -- Special Numbers
  nan       { T.NaN       }
  infP      { T.InfP      }
  infM      { T.InfM      }

  -- Lists
  '['       { T.LBrack    }
  ']'       { T.RBrack    }
  ','       { T.Comma     }

  -- Objects
  '{'       { T.LBrace    }
  '}'       { T.RBrace    }
  ':'       { T.KVDelim   }

  -- Strings
  lq        { T.LQuote    }
  rq        { T.RQuote    }

  -- Parser Control
  lexError  { T.LexError _ }

%%

Json :: { Maybe Json }      -- Save last token, as it is the first of the next parsing
  : Element                 {%^ \tok -> do when (tk /= T.EOF) $ parserPushTok tok; return $ Just $1 }
  | lexError                {% handleLexError $1        }
  | {- empty -}             { Nothing                   }

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
  : lq string rq            { untokStr $2               }

Value :: { Json }
  : Object                  { Object $1                 }
  | Array                   { Array $1                  }
  | String                  { String $1                 }
  | number                  { Number $ Num $ untokNum $1 }
  | true                    { Bool True                 }
  | false                   { Bool False                }
  | null                    { Null                      }
  | nan                     { Number $ NaN              }
  | infP                    { Number $ Num $ fromFloatDigits (1/0)}
  | infM                    { Number $ Num $ fromFloatDigits (-1/0)}

 -- Convinience functions --
{
untokStr :: JsonToken -> Text
untokStr (T.Str s)  = s
untokStr _          = error "Not a string token"

untokNum :: JsonToken -> Scientific
untokNum (T.Num n)  = n
untokNum _          = error "Not a number token"

handleLexError :: JsonToken -> Parser token a
handleLexError (T.LexError input) = lexError input
handleLexError _                  = error "Not LexError"

parseOrLexError t@(T.LexError _)  = handleLexError t
parseOrLexError t                 = parseError t
}
