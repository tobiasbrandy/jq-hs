module Ast (

) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Text (Text)

data Name
  = Name Text
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