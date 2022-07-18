module Lexer.Tokens (
  Token (..)
) where

import Data.Text (Text)

data Token
  -- Identifiers
  = Id          Text
  | Field       Text

  -- Literals
  | String   Text
  | Number  (Either Integer Double)

  -- Keywords
  | Module
  | Import
  | Include
  | Def
  | As
  | If
  | Then
  | Else
  | Elif
  | End
  -- | And
  -- | Or
  | Reduce
  | Foreach
  | Try
  | Catch
  | Label
  | Break
  | Loc         -- __loc__

  -- Arithmetic operators
  | Plus        -- +
  | Minus       -- -
  | Times       -- *
  | Div         -- /
  | Mod         -- %

  -- Flow operators
  | Pipe        -- |
  | Alt         -- //
  | Opt         -- ?
  | Comma       -- ,

  -- Assignment operators
  | Assign      -- =
  | PlusA       -- +=
  | MinusA      -- -=
  | TimesA      -- *=
  | DivA        -- /=
  | ModA        -- %=
  | PipeA       -- |=
  | AltA        -- //=

  -- Comparison operators
  | Eq          -- ==
  | Neq         -- !=
  | Lt          -- <
  | Le          -- <=
  | Gt          -- >
  | Ge          -- >=
  | Or          -- or
  | And         -- and

  -- Special filters
  | Dot         -- .
  | Recr        -- ..

  -- Parenthesis
  | LPar        -- (
  | RPar        -- )

  -- Lists
  | LBrack      -- [
  | RBrack      -- ]

  -- Objects
  | LBrace      -- {
  | RBrac       -- }
  | KVDelim     -- :

  -- Params
  | ArgDelim    -- ;

  -- Variables
  | Var         -- $

  -- EOF
  | EOF
  deriving (Eq, Show)
