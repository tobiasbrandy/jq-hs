module Parse.Filter.Tokens (
  FilterToken (..)
) where

import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Text.Lazy.Builder (Builder)

data FilterToken
  -- Identifiers
  = Id          Text
  | Field       Text

  -- Literals
  | Str   Text
  | Num   Scientific
  | True
  | False
  | Null

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
  | RBrace      -- }
  | KVDelim     -- :

  -- Params
  | ArgDelim    -- ;

  -- Variables
  | Var         -- $

  -- Strings
  | StrBuilder Builder
  | LQuote       -- "
  | RQuote       -- "

  -- EOF
  | EOF
  deriving (Eq, Show)
