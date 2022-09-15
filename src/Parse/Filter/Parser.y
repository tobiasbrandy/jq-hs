{
{-# LANGUAGE NoStrictData #-}
module Parse.Filter.Parser (filterParser) where

import Data.Filter (Filter (..), FuncParam (..))

import Parse.Filter.Lexer (lexer)
import Parse.Filter.Tokens (FilterToken)
import qualified Parse.Filter.Tokens as T

import Parse.Defs (Parser, ParserPos (..), parserGetLexInput)
import Parse.Internal.Parsing (parseError)

import Data.Json (Json (..))
import Data.Text (Text)
import Data.Scientific (Scientific)

import Data.Sequence (Seq ((:|>), (:<|)))
import qualified Data.Sequence as Seq
}

-- Name of parser and first non-terminal
%name filterParser Filter

-- Tokens type
%tokentype { FilterToken }

-- Error handling function
%error { parseError }

-- Monad to use through lexing/parsing
%monad { Parser FilterToken }

-- Lexer function to use. We need to wrap it to interface with Happy. Also we indicate the EOF token
%lexer { (lexer >>=) } { T.EOF }

-- Don't allow shift/reduce conflicts
%expect 0

-- Specify symbol associativity
%right '|'
%left ','
%right '//'
%nonassoc '=' '|=' '+=' '-=' '*=' '/=' '%=' '//='
%left or
%left and
%nonassoc '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%left '?' -- We give it high precendence to avoid implicit parentheses
%nonassoc try
%nonassoc catch

%token
  -- Identifiers
  id        { T.Id _      }
  field     { T.Field _   }

  -- Literals
  string    { T.Str _     }
  number    { T.Num _     }
  true      { T.True      }
  false     { T.False     }
  null      { T.Null      }

  -- Keywords
  module    { T.Module    }
  import    { T.Import    }
  include   { T.Include   }
  def       { T.Def       }
  as        { T.As        }
  if        { T.If        }
  then      { T.Then      }
  else      { T.Else      }
  elif      { T.Elif      }
  end       { T.End       }
  reduce    { T.Reduce    }
  foreach   { T.Foreach   }
  try       { T.Try       }
  catch     { T.Catch     }
  label     { T.Label     }
  break     { T.Break     }
  loc       { T.Loc       }

  -- Arithmetic operators
  '+'       { T.Plus      }
  '-'       { T.Minus     }
  '*'       { T.Times     }
  '/'       { T.Div       }
  '%'       { T.Mod       }

  -- Flow operators
  '|'       { T.Pipe      }
  '//'      { T.Alt       }
  '?'       { T.Opt       }
  '?//'     { T.OptAlt    }
  ','       { T.Comma     }

  -- Assignment operators
  '='       { T.Assign    }
  '+='      { T.PlusA     }
  '-='      { T.MinusA    }
  '*='      { T.TimesA    }
  '/='      { T.DivA      }
  '%='      { T.ModA      }
  '|='      { T.PipeA     }
  '//='     { T.AltA      }

  -- Comparison operators
  '=='      { T.Eq        }
  '!='      { T.Neq       }
  '<'       { T.Lt        }
  '<='      { T.Le        }
  '>'       { T.Gt        }
  '>='      { T.Ge        }
  or        { T.Or        }
  and       { T.And       }

  -- Special filters
  '.'       { T.Dot       }
  '..'      { T.Recr      }

  -- Parenthesis
  '('       { T.LPar      }
  ')'       { T.RPar      }

  -- Lists
  '['       { T.LBrack    }
  ']'       { T.RBrack    }

  -- Objects
  '{'       { T.LBrace    }
  '}'       { T.RBrace    }
  ':'       { T.KVDelim   }

  -- Params
  ';'       { T.ArgDelim  }

  -- Variables
  '$'       { T.Var       }

  -- Strings
  lq        { T.LQuote    }
  rq        { T.RQuote    }
  l_interp  { T.LInterp   }
  r_interp  { T.RInterp   }

%%

-- TODO(tobi): Modules, Imports, etc maybe?
Filter :: { Filter }
  : FuncDefs                      { $1                                  } -- Module
  | Exp                           { $1                                  } -- TopLevel
  | {- empty -}                   { Identity                            }

FuncDefs :: { Filter }
  : FuncDef                       { let (name, params, body) = $1 in FuncDef name params body Empty }
  | FuncDef FuncDefs              { let (name, params, body) = $1 in FuncDef name params body $2    }

FuncDef :: { (Text, Seq FuncParam, Filter) }
  : def id ':' Exp ';'            { (untokStr $2, Seq.empty, $4)        }
  | def id '(' Params ')' ':' Exp ';' { (untokStr $2, $4, $7)           }

Params :: { Seq FuncParam }
  : Param                         { Seq.singleton $1                    }
  | Params ';' Param              { $1 :|> $3                           }

Param :: { FuncParam }
  : '$' id                        { VarParam $ untokStr $2              }
  | '$' Keyword                   { VarParam $2                         }
  | id                            { FilterParam $ untokStr $1           }

Exp :: { Filter } -- `%shift` porque queremos que la expresion con la que matchee sea lo mas grande posible
  : FuncDef Exp            %shift { let (name, params, body) = $1 in FuncDef name params body $2 } 
  | Term as Pattern '|' Exp       { VarDef $3 $1 $5                     }
  | reduce  Term as Pattern '(' Exp ';' Exp ')'           { Reduce  $2 $4 $6 $8           }
  | foreach Term as Pattern '(' Exp ';' Exp ';' Exp ')'   { Foreach $2 $4 $6 $8 $10       }
  | foreach Term as Pattern '(' Exp ';' Exp ')'           { Foreach $2 $4 $6 $8 Identity  }
  | if Exp then Exp ElseBody      { IfElse $2 $4 $5                     }
  | try Exp catch Exp             { TryCatch  $2 $4                     }
  | try Exp                       { TryCatch  $2 Empty                  }
  | label '$' id '|' Exp          { Label (untokStr $3) $5              }
  | Exp '?'                       { TryCatch  $1 Empty                  }
  | Exp '='   Exp                 { funcCall2   "_assign"     $1 $3     }
  | Exp or    Exp                 { Or        $1 $3                     }
  | Exp and   Exp                 { And       $1 $3                     }
  | Exp '//'  Exp                 { Alt       $1 $3                     }
  | Exp '//=' Exp                 { funcCall2   "_modify" $1 $ Alt Identity $3 }
  | Exp '|='  Exp                 { funcCall2   "_modify"     $1 $3     }
  | Exp '|'   Exp                 { Pipe      $1 $3                     }
  | Exp ','   Exp                 { Comma     $1 $3                     }
  | Exp '+'   Exp                 { funcCall2   "_plus"       $1 $3     }     
  | Exp '+='  Exp                 { updateCall  "_plus"       $1 $3     }     
  |     '-'   Exp                 { funcCall1   "_negate"     $2        }     
  | Exp '-'   Exp                 { funcCall2   "_minus"      $1 $3     }      
  | Exp '-='  Exp                 { updateCall  "_minus"      $1 $3     }      
  | Exp '*'   Exp                 { funcCall2   "_multiply"   $1 $3     }     
  | Exp '*='  Exp                 { updateCall  "_multiply"   $1 $3     }     
  | Exp '/'   Exp                 { funcCall2   "_divide"     $1 $3     }     
  | Exp '/='  Exp                 { updateCall  "_divide"     $1 $3     }     
  | Exp '%'   Exp                 { funcCall2   "_mod"        $1 $3     }      
  | Exp '%='  Exp                 { updateCall  "_mod"        $1 $3     }      
  | Exp '=='  Exp                 { funcCall2   "_equal"      $1 $3     }      
  | Exp '!='  Exp                 { funcCall2   "_notequal"   $1 $3     }     
  | Exp '<'   Exp                 { funcCall2   "_less"       $1 $3     }     
  | Exp '>'   Exp                 { funcCall2   "_greater"    $1 $3     }      
  | Exp '<='  Exp                 { funcCall2   "_lesseq"     $1 $3     }     
  | Exp '>='  Exp                 { funcCall2   "_greatereq"  $1 $3     }      
  | Term                          { $1                                  }

Pattern :: { Text }
  : '$' id                        { untokStr $2                         }

ElseBody :: { Filter }
  : elif Exp then Exp ElseBody    { IfElse $2 $4 $5                     }
  | else Exp end                  { $2                                  }

Term :: { Filter }
  : '.'                           { Identity                            }
  | '..'                          { funcCall0 "recurse"                 }
  | Literal                       { $1                                  }
  | break '$' id                  { Break $ untokStr $3                 }
  | OptTerm                %shift { $1                                  } -- Si se encuentra con un '?' tiene que shiftear para matchear con la regla de abajo
  | OptTerm '?'                   { TryCatch $1 Empty                   }
  -- | FORMAT                     {                                     }
  | '(' Exp ')'                   { $2                                  }
  | '[' Exp ']'                   { ArrayLit $2                         }
  | '[' ']'                       { ArrayLit Empty                      }
  | '{' MkDict '}'                { ObjectLit $2                        }
  | '$' loc                       {% do inp@(ParserPos { p_line }, _, _) <- parserGetLexInput; return $ LOC "<top-level>" p_line }
  | '$' id                        { Var $ untokStr $2                   }
  | '$' KeywordNoLoc              { Var $2                              }
  | id                            { FuncCall (untokStr $1) Seq.empty    }
  | id '(' Args ')'               { FuncCall (untokStr $1) $3           }

OptTerm:: { Filter }
  : Term field                    { Project $1 $ Json $ String $ untokStr $2 }
  | field                         { Project Identity $ Json $ String $ untokStr $1 }
  | Term '.' String               { Project $1 $3                       }
  | '.' String                    { Project Identity $2                 }
  | Term '[' Exp ']'              { Project $1 $3                       }
  | Term '[' ']'                  { Pipe $1 Iter                        }
  | Term '[' Exp ':' Exp ']'      { Slice $1 (Just $3) (Just $5)        }
  | Term '[' Exp ':' ']'          { Slice $1 (Just $3) Nothing          }
  | Term '[' ':' Exp ']'          { Slice $1 Nothing (Just $4)          }

Args :: { Seq Filter }
  : Exp                           { Seq.singleton $1                    }
  | Args ';' Exp                  { $1 :|> $3                           }

MkDict :: { Seq (Filter, Filter) }
  : {- empty -}                   { Seq.empty                           }
  | MkDictPair                    { Seq.singleton $1                    }
  | MkDict ',' MkDictPair         { $1 :|> $3                           }

MkDictPair :: { (Filter, Filter) }
  : id      ':' ExpD              { (Json $ String $ untokStr $1, $3)   }
  | Keyword ':' ExpD              { (Json $ String $1, $3)              }
  | String  ':' ExpD              { ($1, $3)                            }
  | String                        { ($1, $1)                            }
  | '$' id                        { let id = untokStr $2 in (Json $ String id, Var id) }
  | '$' Keyword                   { (Json $ String $2, Var $2)              }
  | id                            { let str = Json $ String $ untokStr $1 in (str, str) }
  | Keyword                       { let str = Json $ String $1 in (str, str) }
  | '(' Exp ')' ':' ExpD          { ($2, $5)                            }

ExpD :: { Filter }
  : ExpD '|' ExpD                 { Pipe $1 $3                          }
  | '-' ExpD                      { funcCall1 "_negate" $2              }
  | Term                          { $1                                  }

Literal :: { Filter }
  : String                        { $1                                  }
  | number                        { Json $ Number $ untokNum $1         }
  | true                          { Json $ Bool True                    }
  | false                         { Json $ Bool False                   }
  | null                          { Json Null                           }

String :: { Filter }
  : lq InterpString rq            { $2                                  }

InterpString :: { Filter }
  : string                        { Json $ String $ untokStr $1         }
  | InterpString l_interp Exp r_interp string { funcCall2 "_plus" $1 $ funcCall2 "_plus" $3 $ Json $ String $ untokStr $5 }

Keyword :: { Text }
  : KeywordNoLoc                  { $1                                  }
  | loc                           { "__loc__"                           }

KeywordNoLoc :: { Text }
  : module                        { "module"                            }
  | import                        { "import"                            }
  | include                       { "include"                           }
  | def                           { "def"                               }
  | as                            { "as"                                }
  | if                            { "if"                                }
  | then                          { "then"                              }
  | else                          { "else"                              }
  | elif                          { "elif"                              }
  | end                           { "end"                               }
  | and                           { "and"                               }
  | or                            { "or"                                }
  | reduce                        { "reduce"                            }
  | foreach                       { "foreach"                           }
  | try                           { "try"                               }
  | catch                         { "catch"                             }
  | label                         { "label"                             }
  | break                         { "break"                             }


 -- Convinience functions --
{
untokStr :: FilterToken -> Text
untokStr (T.Str   s)  = s
untokStr (T.Id    s)  = s
untokStr (T.Field s)  = s
untokStr x            = error "Not a string token"

untokNum :: FilterToken -> Scientific
untokNum (T.Num n)  = n
untokNum x          = error "Not a number token"

funcCall0 :: Text -> Filter
funcCall0 name = FuncCall name Seq.empty

funcCall1 :: Text -> Filter -> Filter
funcCall1 name a = FuncCall name $ Seq.singleton a

funcCall2 :: Text -> Filter -> Filter -> Filter
funcCall2 name a b = FuncCall name $ a :<| b :<| Seq.Empty

updateCall :: Text -> Filter -> Filter -> Filter
updateCall name a c = funcCall2 "_modify" a $ funcCall2 name Identity c
}
