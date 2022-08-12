module Data.Filter (
  Filter (..)
, FuncParam (..)
, runFilter
) where

import Prelude hiding (exp, seq, any)

import Data.Json (Json (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger, scientific)
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


-- TODO(tobi): Agregar una monada y poner los errors ahi. Copiar los error messages de jq
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
runFilter Iter                  json  = runIter json
-- Arithmetic operators
runFilter (Neg    num)          json  = runUnary  runNeg    num         json
runFilter (Plus   left right)   json  = runBinary runPlus   left right  json
runFilter (Minus  left right)   json  = runBinary runMinus  left right  json
runFilter (Times  left right)   json  = runBinary runTimes  left right  json
runFilter (Div    left right)   json  = runBinary runDiv    left right  json
runFilter (Mod    left right)   json  = runBinary runMod    left right  json
  -- Flow operators
runFilter (Pipe left right)   json = concatMap (runFilter right) (runFilter left json)
  -- | Alt   Filter Filter       -- //
  -- | TryCatch Filter Filter    -- try .. catch ..
runFilter (Comma left right)  json = runFilter left json <> runFilter right json
  -- | IfElse Filter Filter Filter -- if cond then path1 else path2

runFilter _ _ = error "Not implemented. BORRAR"

-- Auxiliary functions --
runUnary :: (Json -> Json) -> Filter -> Json -> [Json]
runUnary op exp json = map op (runFilter exp json)

runBinary :: (Json -> Json -> Json) -> Filter -> Filter -> Json -> [Json]
runBinary op left right json = [op l r | l <- runFilter left json, r <- runFilter right json]

-- Filter operators implementations --
runArrayLit :: Filter -> Json -> [Json]
runArrayLit items json = [Array $ foldl' (:|>) Seq.empty $ runFilter items json]

runObjectLit :: Seq (Filter, Filter) -> Json -> [Json]
runObjectLit entries json = map Object $ foldr entryCrossMaps [Map.empty] entries
  where
    entryCrossMaps (key, val) maps = concat [map (insertKvInMap k v) maps | k <- runFilter key json, v <- runFilter val json]

    insertKvInMap (String key)  val m  = Map.insert key val m
    insertKvInMap any           _   _  = error ("Cannot use " <> jsonShowError any <> " as object key")

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

runNeg :: Json -> Json
runNeg (Number n)  = Number $ negate n
runNeg any         = error (jsonShowError any <> " cannot be negated")

runPlus :: Json -> Json -> Json
runPlus (Number l) (Number r)  = Number  $ l +  r
runPlus (Array l)  (Array  r)  = Array   $ l <> r
runPlus (String l) (String r)  = String  $ l <> r
runPlus (Object l) (Object r)  = Object  $ r <> l -- On collisions conserves the entry from the right
runPlus Null       any         = any
runPlus any        Null        = any
runPlus l          r           = error (jsonShowError l <> " and " <> jsonShowError r <> " cannot be added")

runMinus :: Json -> Json -> Json
runMinus (Number l) (Number r)  = Number  $ l - r
runMinus (Array l)  (Array  r)  = Array   $ Seq.filter (`notElem` r) l
runMinus l          r           = error (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

runTimes :: Json -> Json -> Json
runTimes (Number l)   (Number r)    = Number  $ l * r
runTimes (String l)   (Number r)    = if r > 0 then String $ T.replicate (floor r) l else Null
runTimes l@(Object _) r@(Object  _) = merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
runTimes l            r             = error (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

runDiv :: Json -> Json -> Json
runDiv jl@(Number l) jr@(Number r)
  | r == 0    = error (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = Number $ l / r
runDiv (String l) (String r) = Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
runDiv l          r          = error (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

runMod :: Json -> Json -> Json
runMod jl@(Number l) jr@(Number r) 
  | r == 0    = error (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = Number $ fromInteger $ truncate l `mod` truncate (abs r)
runMod l          r           = error (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

-- Error handling --
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> String
jsonShowError json@(Number  _)  = "number ("  <> show json <> ")"
jsonShowError json@(String  _)  = "string ("  <> show json <> ")"
jsonShowError json@(Bool    _)  = "boolean (" <> show json <> ")"
jsonShowError json@(Object  _)  = "object ("  <> show json <> ")"
jsonShowError json@(Array   _)  = "array ("   <> show json <> ")"
jsonShowError json@Null         = "null ("    <> show json <> ")"
