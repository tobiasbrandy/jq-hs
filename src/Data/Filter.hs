module Data.Filter (
  Filter (..)
, FuncParam (..)
, runFilter
) where

import Prelude hiding (exp, seq)

import Data.Json (Json (..))
import Data.Text (Text)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (fromMaybe)

data Filter
  -- Basic
  = Identity
  | Empty
  | Recursive
  | Json Json

  -- Variable
  | Var Text
  | VarDef Text Filter

  -- Literals
  | ArrayLit   Filter
  | ObjectLit  (Seq (Filter, Filter)) -- TODO(tobi): Evaluar si usar Vector aca

  -- Projections
  | ObjProject      Filter
  | GenericProject  Filter
  | Slice (Maybe Filter) (Maybe Filter)
  | Iter

  -- Arithmetic operators
  | Neg   Filter              -- -
  | Plus  Filter Filter       -- +
  | Minus Filter Filter       -- -
  | Times Filter Filter       -- *
  | Div   Filter Filter       -- /
  | Mod   Filter Filter       -- %

  -- Flow operators
  | Pipe  Filter Filter       -- |
  | Alt   Filter Filter       -- //
  | TryCatch Filter Filter    -- try .. catch ..
  | Comma Filter Filter       -- ,
  | IfElse Filter Filter Filter -- if cond then path1 else path2

  -- Assignment operators
  | Assign    Filter Filter   -- =
  | UpdateA   Filter Filter   -- |=
  | PlusA     Filter Filter   -- +=
  | MinusA    Filter Filter   -- -=
  | TimesA    Filter Filter   -- *=
  | DivA      Filter Filter   -- /=
  | ModA      Filter Filter   -- %=
  | AltA      Filter Filter   -- //=

  -- Comparison operators
  | Eq  Filter Filter         -- ==
  | Neq Filter Filter         -- !=
  | Lt  Filter Filter         -- <
  | Le  Filter Filter         -- <=
  | Gt  Filter Filter         -- >
  | Ge  Filter Filter         -- >=
  | Or  Filter Filter         -- or
  | And Filter Filter         -- and

  -- Functions
  | FuncDef Text (Seq FuncParam) Filter
  | FuncCall Text (Seq Filter)

  -- Label & break
  | Label Text
  | Break Text

  -- Special
  | LOC Text Int
  deriving (Eq, Show)

data FuncParam
  = VarParam Text
  | FilterParam Text
  deriving (Eq, Show)


-- TODO(tobi): Agregar una monada y poner los errors ahi
runFilter :: Filter -> Json -> [Json]
-- Basic
runFilter Identity              json  = [json]
runFilter Empty                 _     = []
runFilter Recursive             _     = error "Recursive filter not implemented"
runFilter (Json json)           _     = [json]
-- Variable
runFilter (Var name)            _     = error "Var filter not implemented"
runFilter (VarDef name body)    _     = error "VarDef filter not implemented"
-- Literals
runFilter (ArrayLit items)      json  = runArrayLit items json
runFilter (ObjectLit entries)   json  = runObjectLit entries json
-- Projections
runFilter (ObjProject prop)     json  = runObjProject prop json
runFilter (GenericProject exp)  json  = runGenericProject exp json
runFilter (Slice left right)    json  = runSlice left right json
runFilter Iter                json  = runIter json
-- ...
runFilter (Pipe left right)   json = concatMap (runFilter right) (runFilter left json)

runFilter _ _ = error "Not implemented. BORRAR"

runArrayLit :: Filter -> Json -> [Json]
runArrayLit items json = [Array $ foldl' (:|>) Seq.empty $ runFilter items json]

runObjectLit :: Seq (Filter, Filter) -> Json -> [Json]
runObjectLit entries json = map Object $ foldr entryCrossMaps [Map.empty] entries
  where
    entryCrossMaps (key, val) maps = concat [map (insertKvInMap k v) maps | k <- runFilter key json, v <- runFilter val json]

    -- TODO(tobi): Mejor error: "jq: error (at <stdin>:1): Cannot use object ({"hola":"ch...) as object key"
    insertKvInMap (String key)  val m  = Map.insert key val m
    insertKvInMap _             _   _  = error "Object keys must be strings"

runIter :: Json -> [Json]
runIter (Array items)     = toList items
runIter (Object entries)  = Map.elems entries
runIter _                 = error "Iterator (.[]) may only be applied to arrays or objects"

runObjProject :: Filter -> Json -> [Json]
runObjProject prop json = map (project json) (runFilter prop json)
  where
    project (Object m)  (String key)  = Map.findWithDefault Null key m
    project (Object _)  _             = error "Objects can only be projected with a string"
    project _           (String _)    = error "Only objects can be projected with a string"
    project _           _             = error "Object projection is only valid for string filters and object jsons"

runGenericProject :: Filter -> Json -> [Json]
runGenericProject exp json = map (project json) (runFilter exp json)
  where
    project (Object m)    (String key)  = Map.findWithDefault Null key m
    project (Object _)    _             = error "Only objects can be projected with a string"
    project _             (String _)    = error "Objects can only be projected with a string"
    project (Array items) (Number n)    = arrayLookup items n
    project (Array _)     _             = error "Arrays can only be projected with a number"
    project _             (Number _)    = error "Only arrays can be projected with a number"
    project _             _             = error "Projection is only available for objects using strings, or arrays using numbers"

    arrayLookup items n =
      if isInteger n
      then case toBoundedInteger n of
        Nothing -> Null
        Just i  -> fromMaybe Null $ Seq.lookup (cycleIndex i items) items
      else Null

    cycleIndex i items = if i < 0 then length items + i else i

runSlice :: Maybe Filter -> Maybe Filter -> Json -> [Json]
runSlice left right json@(Array items) = [Array $ slice l r items | l <- getIndeces (0::Int) left json, r <- getIndeces itemsLen right json]
  where
    itemsLen = Seq.length items

    getIndeces def indexExp j = maybe [Number $ fromInteger $ toInteger def] (`runFilter` j) indexExp

    slice (Number l)  (Number r)  = seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r))
    slice _           (Number _)  = error "Start index of array slice must be a number or empty"
    slice (Number _)  _           = error "End index of array slice must be a number or empty"
    slice _           _           = error "Start and end index of array slice must be a number or empty"

    cycleIndex i = if i < 0 then itemsLen + i else i

    seqSlice l r = Seq.take (r-1) . Seq.drop l
runSlice _ _ _ = error "Slice operator is only valid for arrays"
