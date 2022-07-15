{
module Lexer
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , lexer

  , Token (..)
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monad-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

-- Ignore whitespace
<0> $white+ ;

-- Block comment
<0>       "/*" { begin comment  }
<comment> "*/" { begin 0        }
<comment> .    ;
<comment> \n   ;

-- Line comment
<0>             "//"  { begin line_comment  }
<line_comment>  .     ;
<line_comment>  \n    { begin 0             }

-- Keywords
<0> let     { tok Let     }
<0> in      { tok In      }
<0> if      { tok If      }
<0> then    { tok Then    }
<0> else    { tok Else    }

-- Arithmetic operators
<0> "+"     { tok Plus    }
<0> "-"     { tok Minus   }
<0> "*"     { tok Times   }
<0> "/"     { tok Divide  }

-- Comparison operators
<0> "="     { tok Eq      }
<0> "<>"    { tok Neq     }
<0> "<"     { tok Lt      }
<0> "<="    { tok Le      }
<0> ">"     { tok Gt      }
<0> ">="    { tok Ge      }

-- Logical operators
<0> "&"     { tok And     }
<0> "|"     { tok Or      }

-- Parenthesis
<0> "("     { tok LPar    }
<0> ")"     { tok RPar    }

-- Lists
<0> "["     { tok LBrack  }
<0> "]"     { tok RBrack  }
<0> ","     { tok Comma   }

-- Types
<0> ":"     { tok Colon   }
<0> "->"    { tok Arrow   }

-- Identifiers
<0> @id     { tokId       }

-- Constants
<0> $digit+ { tokInteger  }
<0> \"[^\"]*\" { tokString }

{

data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Logical operators
  | And
  | Or
  -- Parenthesis
  | LPar
  | RPar
  -- Lists
  | Comma
  | LBrack
  | RBrack
  -- Types
  | Colon
  | Arrow
  -- EOF
  | EOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return EOF

tok :: Token -> AlexAction Token
tok t _ _ = return t

tokId :: AlexAction Token
tokId (_, _, str, _) len = return $ Identifier $ BS.take len str

tokInteger :: AlexAction Token
tokInteger (_, _, str, _) len = return $ Integer $ read $ BS.unpack $ BS.take len str

tokString :: AlexAction Token
tokString (_, _, str, _) len = return $ String $ BS.take len str

-- Main Driver
lexer :: Alex Token
lexer = alexMonadScan

}
