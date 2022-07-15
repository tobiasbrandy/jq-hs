{
{-# LANGUAGE DeriveFoldable #-}
module Parser
  ( parseMiniML
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Lexer as L
}

-- Name of parser and first non-terminal
%name parseMiniML decs

-- Tokens type
%tokentype { L.Token }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { L.Alex }

-- Lexer function to use. We need to wrap it to interface with Happy. Also we indicate the EOF token
%lexer { (L.lexer >>=) } { L.EOF }

-- Don't allow shift/reduce conflicts
%expect 0

%token
  -- Identifiers
  identifier { L.Identifier _ }

  -- Constants
  string     { L.String _     }
  integer    { L.Integer _    }

  -- Keywords
  let        { L.Let          }
  in         { L.In           }
  if         { L.If           }
  then       { L.Then         }
  else       { L.Else         }

  -- Arithmetic operators
  '+'        { L.Plus         }
  '-'        { L.Minus        }
  '*'        { L.Times        }
  '/'        { L.Divide       }

  -- Comparison operators
  '='        { L.Eq           }
  '<>'       { L.Neq          }
  '<'        { L.Lt           }
  '<='       { L.Le           }
  '>'        { L.Gt           }
  '>='       { L.Ge           }

  -- Logical operators
  '&'        { L.And          }
  '|'        { L.Or           }

  -- Parenthesis
  '('        { L.LPar         }
  ')'        { L.RPar         }

  -- Lists
  '['        { L.LBrack       }
  ']'        { L.RBrack       }
  ','        { L.Comma        }

  -- Types
  ':'        { L.Colon        }
  '->'       { L.Arrow        }

%right else in
%right '->'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'

%%

optional(p)
  :   { Nothing }
  | p { Just $1 }

many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

sepBy_rev(p, sep)
  :                         { [] }
  | sepBy_rev(p, sep) sep p { $3 : $1 }

sepBy(p, sep)
  : sepBy_rev(p, sep) { reverse $1 }

name :: { Name }
  : identifier { unTok $1 (\(L.Identifier name) -> Name name) }

type :: { Type }
  : name           { TVar $1 }
  | '(' ')'        { TUnit }
  | '(' type ')'   { TPar $2 }
  | '[' type ']'   { TList $2 }
  | type '->' type { TArrow $1 $3 }

typeAnnotation :: { Type }
  : ':' type { $2 }

argument :: { Argument }
  : '(' name optional(typeAnnotation) ')' { Argument $2 $3 }
  | name                                  { Argument $1 Nothing }

dec :: { Dec }
  : let name many(argument) optional(typeAnnotation) '=' exp { Dec $2 $3 $4 $6 }

decs :: { [Dec] }
  : many(dec) { $1 }

exp :: { Exp }
  : expapp                   { $1                   }
  | expcond                  { $1                   }
  | '-' exp                  { ENeg $2              }

  -- Arithmetic operators
  | exp '+'  exp             { EBinOp $1 Plus $3    }
  | exp '-'  exp             { EBinOp $1 Minus $3   }
  | exp '*'  exp             { EBinOp $1 Times $3   }
  | exp '/'  exp             { EBinOp $1 Divide $3  }

  -- Comparison operators
  | exp '='  exp             { EBinOp $1 Eq $3      }
  | exp '<>' exp             { EBinOp $1 Neq $3     }
  | exp '<'  exp             { EBinOp $1 Lt $3      }
  | exp '<=' exp             { EBinOp $1 Le $3      }
  | exp '>'  exp             { EBinOp $1 Gt $3      }
  | exp '>=' exp             { EBinOp $1 Ge $3      }

  -- Logical operators
  | exp '&'  exp             { EBinOp $1 And $3     }
  | exp '|'  exp             { EBinOp $1 Or $3      }
  | dec in exp               { ELetIn $1 $3         }

expapp :: { Exp }
  : expapp atom              { EApp $1 $2           }
  | atom                     { $1                   }

expcond :: { Exp }
  : if exp then exp %shift   { EIfThen $2 $4        }
  | if exp then exp else exp { EIfThenElse $2 $4 $6 }

atom :: { Exp }
  : integer                  { unTok $1 (\(L.Integer int) -> EInt int) }
  | name                     { EVar $1              }
  | string                   { unTok $1 (\(L.String string) -> EString string) }
  | '(' ')'                  { EUnit                }
  | '[' sepBy(exp, ',') ']'  { EList $2             }
  | '(' exp ')'              { EPar $2              }

    -- Arithmetic operators
  | '(' '+' ')'              { EOp Plus             }
  | '(' '-' ')'              { EOp Minus            }
  | '(' '*' ')'              { EOp Times            }
  | '(' '/' ')'              { EOp Divide           }

  -- Comparison operators
  | '(' '=' ')'              { EOp Eq               }
  | '(' '<>' ')'             { EOp Neq              }
  | '(' '<' ')'              { EOp Lt               }
  | '(' '<=' ')'             { EOp Le               }
  | '(' '>' ')'              { EOp Gt               }
  | '(' '>=' ')'             { EOp Ge               }

  -- Logical operators
  | '(' '&' ')'              { EOp And              }
  | '(' '|' ')'              { EOp Or               }

{

-- TODO(tobi): Better error
parseError :: L.Token -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

unTok :: L.Token -> (L.Token -> a) -> a
unTok t f = f t

-- * AST
data Name
  = Name ByteString
  deriving (Show)

data Type
  = TVar Name
  | TPar Type
  | TUnit
  | TList Type
  | TArrow Type Type
  deriving (Show)

data Argument
  = Argument Name (Maybe Type)
  deriving (Show)

data Dec
  = Dec Name [Argument] (Maybe Type) Exp
  deriving (Show)

data Operator
  = Plus
  | Minus
  | Times
  | Divide
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  deriving (Show)

data Exp
  = EInt Integer
  | EVar Name
  | EString ByteString
  | EUnit
  | EList [Exp]
  | EPar Exp
  | EApp Exp Exp
  | EIfThen Exp Exp
  | EIfThenElse Exp Exp Exp
  | ENeg Exp
  | EBinOp Exp Operator Exp
  | EOp Operator
  | ELetIn Dec Exp
  deriving (Show)

}
