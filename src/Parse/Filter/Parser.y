{
module Parse.Filter.Parser (filterParser) where

import Parse.Defs (Parser)
import Parse.Filter.Lexer (lexer)
import Parse.Filter.Tokens (Token (..))
import Parse.Internal.Parsing (parseError, untok)
}

-- Name of parser and first non-terminal
%name filterParser TopLevel

-- Tokens type
%tokentype { Token }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { Parser Token }

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
  string    { Str _     }
  number    { Num _     }

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
  '}'       { RBrace    }
  ':'       { KVDelim   }

  -- Params
  ';'       { KVDelim   }

  -- Variables
  '$'       { Var       }

%%

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
