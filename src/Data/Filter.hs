module Data.Filter (
  Filter (..)
, FuncParam (..)

) where

import Prelude hiding (exp, seq, any, filter, init)

import Data.Json (Json (..))
import Data.Text (Text)
import Data.Sequence (Seq)

data Filter
  -- Basic
  = Identity
  | Empty
  | Json Json

  -- Variable
  | Var Text
  | VarDef Text Filter Filter

  -- Literals
  | ArrayLit   Filter
  | ObjectLit  (Seq (Filter, Filter)) -- TODO(tobi): Evaluar si usar Vector aca

  -- Projections
  | Project Filter Filter
  | Slice Filter (Maybe Filter) (Maybe Filter)
  | Iter

  -- Flow operators
  | Pipe  Filter Filter       -- |
  | Alt   Filter Filter       -- //
  | TryCatch Filter Filter    -- try .. catch ..
  | Comma Filter Filter       -- ,
  | IfElse Filter Filter Filter -- if cond then path1 else path2

  -- Assignment operators
  | Assign    Filter Filter   -- =
  | Update   Filter Filter   -- |=

  -- Comparison operators
  | Or  Filter Filter         -- or
  | And Filter Filter         -- and

  -- Reductions
  | Reduce  Filter Text Filter Filter         -- reduce stream_expression as $name (initial_value; update_expression)
  | Foreach Filter Text Filter Filter Filter  -- foreach stream_expression as $name (initial_value; update_expression; extract_expression)

  -- Functions
  | FuncDef Text (Seq FuncParam) Filter Filter
  | FuncCall Text (Seq Filter)

  -- Label & break
  | Label Text Filter
  | Break Text

  -- Special
  | LOC Text Int
  deriving (Eq, Show)

data FuncParam
  = VarParam Text
  | FilterParam Text
  deriving (Eq, Show)
