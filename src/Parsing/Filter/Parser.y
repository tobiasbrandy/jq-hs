{
module Parsing.Filter.Parser (parser) where

import Parsing.Defs (Lex)
import Parsing.Filter.Lexer (lexer)
import Parsing.Filter.Tokens (Token (..))
import Parsing.Internal.Parsing (parseError, unTok)
}

-- Name of parser and first non-terminal
%name parser TopLevel

-- Tokens type
%tokentype { Token }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { Lex }

-- Lexer function to use. We need to wrap it to interface with Happy. Also we indicate the EOF token
%lexer { (lexer >>=) } { EOF }

-- Don't allow shift/reduce conflicts
%expect 0

-- Specify symbol associativity
%right try catch
%right '|'
%left ','
%left '//'
%nonassoc '=' '|=' '+=' '-=' '*=' '/=' '%=' '//='
%left or
%left and
%nonassoc '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left '?' -- We give it high precendence to avoid implicit parentheses
-- %nonassoc '?//' -- TODO(tobi): Alternative destructuring (destructuring in general) not supported

%token
  -- Identifiers
  id        { Id _      }
  field     { Field _   }

  -- Literals
  string    { String _  }
  number    { Number _  }

  -- Keywords
  module    { Module    }
  import    { Import    }
  include   { Include   }
  def       { Def       }
  as        { As        }
  if        { If        }
  then      { Then      }
  else      { Else      }
  elif      { Elif      }
  end       { End       }
  reduce    { Reduce    }
  foreach   { Foreach   }
  try       { Try       }
  catch     { Catch     }
  label     { Label     }
  break     { Break     }
  loc       { Loc       }

  -- Arithmetic operators
  '+'       { Plus      }
  '-'       { Minus     }
  '*'       { Times     }
  '/'       { Div       }
  '%'       { Mod       }

  -- Flow operators
  '|'       { Pipe      }
  '//'      { Alt       }
  '?'       { Opt       }
  ','       { Comma     }

  -- Assignment operators
  '='       { Assign    }
  '+='      { PlusA     }
  '-='      { MinusA    }
  '*='      { TimesA    }
  '/='      { DivA      }
  '%='      { ModA      }
  '|='      { PipeA     }
  '//='     { AltA      }

  -- Comparison operators
  '=='      { Eq        }
  '!='      { Neq       }
  '<'       { Lt        }
  '<='      { Le        }
  '>'       { Gt        }
  '>='      { Ge        }
  or        { Or        }
  and       { And       }

  -- Special filters
  '.'       { Dot       }
  '..'      { Recr      }

  -- Parenthesis
  '('       { LPar      }
  ')'       { RPar      }

  -- Lists
  '['       { LBrack    }
  ']'       { RBrack    }

  -- Objects
  '{'       { LBrace    }
  '}'       { RBrac     }
  ':'       { KVDelim   }

  -- Params
  ';'       { KVDelim   }

  -- Variables
  '$'       { Var       }

%%

-- Macros
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

-- Parsing Rules

-- name :: { Name }
--   : identifier { unTok $1 (\(L.Identifier name) -> Name name) }

-- type :: { Type }
--   : name           { TVar $1 }
--   | '(' ')'        { TUnit }
--   | '(' type ')'   { TPar $2 }
--   | '[' type ']'   { TList $2 }
--   | type '->' type { TArrow $1 $3 }

-- typeAnnotation :: { Type }
--   : ':' type { $2 }

-- argument :: { Argument }
--   : '(' name optional(typeAnnotation) ')' { Argument $2 $3 }
--   | name                                  { Argument $1 Nothing }

-- dec :: { Dec }
--   : let name many(argument) optional(typeAnnotation) '=' exp { Dec $2 $3 $4 $6 }

-- decs :: { [Dec] }
--   : many(dec) { $1 }

-- exp :: { Exp }
--   : expapp                   { $1                   }
--   | expcond                  { $1                   }
--   | '-' exp                  { ENeg $2              }

--   -- Arithmetic operators
--   | exp '+'  exp             { EBinOp $1 Plus $3    }
--   | exp '-'  exp             { EBinOp $1 Minus $3   }
--   | exp '*'  exp             { EBinOp $1 Times $3   }
--   | exp '/'  exp             { EBinOp $1 Divide $3  }

--   -- Comparison operators
--   | exp '='  exp             { EBinOp $1 Eq $3      }
--   | exp '<>' exp             { EBinOp $1 Neq $3     }
--   | exp '<'  exp             { EBinOp $1 Lt $3      }
--   | exp '<=' exp             { EBinOp $1 Le $3      }
--   | exp '>'  exp             { EBinOp $1 Gt $3      }
--   | exp '>=' exp             { EBinOp $1 Ge $3      }

--   -- Logical operators
--   | exp '&'  exp             { EBinOp $1 And $3     }
--   | exp '|'  exp             { EBinOp $1 Or $3      }
--   | dec in exp               { ELetIn $1 $3         }

-- expapp :: { Exp }
--   : expapp atom              { EApp $1 $2           }
--   | atom                     { $1                   }

-- expcond :: { Exp }
--   : if exp then exp %shift   { EIfThen $2 $4        }
--   | if exp then exp else exp { EIfThenElse $2 $4 $6 }

-- atom :: { Exp }
--   : integer                  { unTok $1 (\(L.Integer int) -> EInt int) }
--   | name                     { EVar $1              }
--   | string                   { unTok $1 (\(L.String string) -> EString string) }
--   | '(' ')'                  { EUnit                }
--   | '[' sepBy(exp, ',') ']'  { EList $2             }
--   | '(' exp ')'              { EPar $2              }

--     -- Arithmetic operators
--   | '(' '+' ')'              { EOp Plus             }
--   | '(' '-' ')'              { EOp Minus            }
--   | '(' '*' ')'              { EOp Times            }
--   | '(' '/' ')'              { EOp Divide           }

--   -- Comparison operators
--   | '(' '=' ')'              { EOp Eq               }
--   | '(' '<>' ')'             { EOp Neq              }
--   | '(' '<' ')'              { EOp Lt               }
--   | '(' '<=' ')'             { EOp Le               }
--   | '(' '>' ')'              { EOp Gt               }
--   | '(' '>=' ')'             { EOp Ge               }

--   -- Logical operators
--   | '(' '&' ')'              { EOp And              }
--   | '(' '|' ')'              { EOp Or               }

-- TODO(tobi): Modules, Imports, etc maybe?
TopLevel :: {  }
  : Exp                           {                       }

FuncDef :: {  }
  : def id ':' Exp ';'            {                       }
  | def id '(' Params ')' ':' Exp ';' {                   }

Params :: {  }
  : Param                         {                       }
  | Params ';' Param              {                       }

Param :: {  }
  : '$' id                        {                       }
  | id                            {                       }

Exp :: {  }
  : FuncDef Exp            %shift {                       } -- Queremos que la expresion con la que matchee sea lo mas grande posible
  | Term as Pattern '|' Exp       {                       }
  -- | reduce  Term as Pattern '(' Exp ';' Exp ')'          |
  -- | foreach Term as Pattern '(' Exp ';' Exp ';' Exp ')'  |
  -- | foreach Term as Pattern '(' Exp ';' Exp ')'          |
  | if Exp then Exp ElseBody      {                       }
  | try Exp catch Exp             {                       }
  | try Exp                       {                       }
  | label '$' id '|' Exp          {                       }
  | Exp '?'                       {                       }
  | Exp '='   Exp                 {                       }
  | Exp or    Exp                 {                       }
  | Exp and   Exp                 {                       }
  | Exp '//'  Exp                 {                       }
  | Exp '//=' Exp                 {                       }
  | Exp '|='  Exp                 {                       }
  | Exp '|'   Exp                 {                       }
  | Exp ','   Exp                 {                       }
  | Exp '+'   Exp                 {                       }
  | Exp '+='  Exp                 {                       }
  |     '-'   Exp                 {                       }
  | Exp '-'   Exp                 {                       }
  | Exp '-='  Exp                 {                       }
  | Exp '*'   Exp                 {                       }
  | Exp '*='  Exp                 {                       }
  | Exp '/'   Exp                 {                       }
  | Exp '%'   Exp                 {                       }
  | Exp '/='  Exp                 {                       }
  | Exp '%='  Exp                 {                       }
  | Exp '=='  Exp                 {                       }
  | Exp '!='  Exp                 {                       }
  | Exp '<'   Exp                 {                       }
  | Exp '>'   Exp                 {                       }
  | Exp '<='  Exp                 {                       }
  | Exp '>='  Exp                 {                       }
  | Term                          {                       }

Pattern :: {  }
  : '$' id                        {                       }
-- TODO(tobi): Por ahora no soportamos destructuring
--   | '[' ArrayPats ']'             {                       }
--   | '{' ObjPats '}'               {                       }

-- ArrayPats :: {  }
--   : Pattern                       {                       }
--   | ArrayPats ',' Pattern         {                       }

-- ObjPats :: {  }
--   : ObjPat                        {                       }
--   | ObjPats ',' ObjPat            {                       }

-- ObjPat :: {  }
--   : '$' id                        {                       }
--   | id          ':' Pattern       {                       }
--   | Keyword     ':' Pattern       {                       }
--   | string      ':' Pattern       {                       }
--   | '(' Exp ')' ':' Pattern       {                       }

ElseBody :: {  }
  : elif Exp then Exp ElseBody    {                       }
  | else Exp end                  {                       }

Term :: {  }
  : '.'                           {                       }
  | '..'                          {                       }
  | break '$' id                  {                       }
  | OptTerm                %shift {                       } -- Si se encuentra con un '?' tiene que shiftear para matchear con la regla de abajo
  | OptTerm '?'                   {                       }
  | number                        {                       }
  | string                        {                       }
  -- | FORMAT                     {                       }
  | '(' Exp ')'                   {                       }
  | '[' Exp ']'                   {                       }
  | '[' ']'                       {                       }
  | '{' MkDict '}'                {                       }
  | '$' loc                       {                       }
  | '$' id                        {                       }
  | id                            {                       }
  | id '(' Args ')'               {                       }

OptTerm:: {}
  : Term field                    {                       }
  | field                         {                       }
  | Term '.' string               {                       }
  | '.' string                    {                       }
  | Term '[' Exp ']'              {                       }
  | Term '[' ']'                  {                       }
  | Term '[' Exp ':' Exp ']'      {                       }
  | Term '[' Exp ':' ']'          {                       }
  | Term '[' ':' Exp ']'          {                       }

Args :: {  }
  : Arg                           {                       }
  | Args ';' Arg                  {                       }

Arg :: {  }
  : Exp                           {                       }

MkDict :: {  }
  : {- empty -}                   {                       }
  | MkDictPair                    {                       }
  | MkDictPair ',' MkDict         {                       }

MkDictPair :: {  }
  : id      ':' ExpD              {                       }
  | Keyword ':' ExpD              {                       }
  | string  ':' ExpD              {                       }
  | string                        {                       }
  | '$' id                        {                       }
  | id                            {                       }
  | '(' Exp ')' ':' ExpD          {                       }

ExpD :: {  }
  : ExpD '|' ExpD                 {                       }
  | '-' ExpD                      {                       }
  | Term                          {                       }

Keyword :: { Token }
  : module                        { $1                    }
  | import                        { $1                    }
  | include                       { $1                    }
  | def                           { $1                    }
  | as                            { $1                    }
  | if                            { $1                    }
  | then                          { $1                    }
  | else                          { $1                    }
  | elif                          { $1                    }
  | end                           { $1                    }
  | and                           { $1                    }
  | or                            { $1                    }
  | reduce                        { $1                    }
  | foreach                       { $1                    }
  | try                           { $1                    }
  | catch                         { $1                    }
  | label                         { $1                    }
  | break                         { $1                    }
  | loc                           { $1                    }
