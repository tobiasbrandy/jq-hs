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
  | Project Filter Filter
  | Slice Filter (Maybe Filter) (Maybe Filter)
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
runFilter Identity                  json  = [json]
runFilter Empty                     _     = []
runFilter Recursive                 json  = runRecursive json
runFilter (Json json)               _     = [json]
-- Variable
runFilter (Var name)                _     = error "Var filter not implemented"
runFilter (VarDef name body)        _     = error "VarDef filter not implemented"
-- Literals
runFilter (ArrayLit items)          json  = runArrayLit items json
runFilter (ObjectLit entries)       json  = runObjectLit entries json
-- Projections
runFilter (Project term exp)        json  = runBinary runProject term exp json
runFilter (Slice term left right)   json  = runSlice term left right json
runFilter Iter                      json  = runIter json
-- Arithmetic operators
runFilter (Neg    num)              json  = runUnary  runNeg    num         json
runFilter (Plus   left right)       json  = runBinary runPlus   left right  json
runFilter (Minus  left right)       json  = runBinary runMinus  left right  json
runFilter (Times  left right)       json  = runBinary runTimes  left right  json
runFilter (Div    left right)       json  = runBinary runDiv    left right  json
runFilter (Mod    left right)       json  = runBinary runMod    left right  json
-- Flow operators
runFilter (Pipe left right)         json  = concatMap (runFilter right) (runFilter left json)
runFilter (Alt left right)          json  = runBinary runAlt    left right  json
runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runFilter (Comma left right)        json  = runFilter left json <> runFilter right json
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Assignment operators
runFilter (Assign   left right)     json  = error "Assign filter not implemented"
runFilter (UpdateA  left right)     json  = error "UpdateA filter not implemented"
runFilter (PlusA    left right)     json  = error "PlusA filter not implemented"
runFilter (MinusA   left right)     json  = error "MinusA filter not implemented"
runFilter (TimesA   left right)     json  = error "TimesA filter not implemented"
runFilter (DivA     left right)     json  = error "DivA filter not implemented"
runFilter (ModA     left right)     json  = error "ModA filter not implemented"
runFilter (AltA     left right)     json  = error "AltA filter not implemented"
-- Comparison operators
runFilter (Eq   left right)         json  = runComparison (==)  left right  json
runFilter (Neq  left right)         json  = runComparison (/=)  left right  json
runFilter (Lt   left right)         json  = runComparison (<)   left right  json
runFilter (Le   left right)         json  = runComparison (<=)  left right  json
runFilter (Gt   left right)         json  = runComparison (>)   left right  json
runFilter (Ge   left right)         json  = runComparison (>=)  left right  json
runFilter (Or   left right)         json  = runBoolComp   (||)  left right  json
runFilter (And  left right)         json  = runBoolComp   (&&)  left right  json
-- Functions
runFilter (FuncDef name params body) json = error "FuncDef filter not implemented"
runFilter (FuncCall name params)    json  = error "FuncCall filter not implemented"
-- Label & break
runFilter (Label label)             json  = error "Label filter not implemented"
runFilter (Break label)             json  = error "Break filter not implemented"
-- Special
runFilter (LOC file line)           _     = [Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]]

-- Auxiliary functions --
runUnary :: (Json -> Json) -> Filter -> Json -> [Json]
runUnary op exp json = map op (runFilter exp json)

runBinary :: (Json -> Json -> Json) -> Filter -> Filter -> Json -> [Json]
runBinary op left right json = [op l r | l <- runFilter left json, r <- runFilter right json]

runComparison :: (Json -> Json -> Bool) -> Filter -> Filter -> Json -> [Json]
runComparison op = runBinary (\l -> Bool . op l)

runBoolComp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Json -> [Json]
runBoolComp op = runComparison (\l r -> op (jsonBool l) (jsonBool r))

jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

-- Filter operators implementations --
runRecursive :: Json -> [Json]
runRecursive json@(Object m)    = json : concatMap runRecursive (Map.elems m)
runRecursive json@(Array items) = json : concatMap runRecursive items
runRecursive json               = [json]

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

runProject :: Json -> Json -> Json
runProject (Object m)    (String key)  = Map.findWithDefault Null key m
runProject (Array items) (Number n)    =
  if isInteger n
  then case toBoundedInteger n of
    Nothing -> Null
    Just i  -> fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
  else Null
runProject anyl          anyr          = error ("Cannot index " <> jsonShowError anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> [Json]
runSlice term left right json = [Array $ slice t l r |
    t <- runFilter term json,
    l <- getIndeces (0::Int) left json,
    r <- getIndeces (itemsLen t) right json
  ]
  where
    itemsLen (Array items)  = Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe [Number $ fromInteger $ toInteger def] (`runFilter` j) indexExp

    slice (Array items) (Number l)  (Number r)  = seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r)) items
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Array _) anyl anyr = error ("Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr)
    slice any _ _ = error (jsonShowError any <> " cannot be sliced, only arrays")

    seqSlice l r = Seq.take (r-1) . Seq.drop l

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

runAlt :: Json -> Json -> Json
runAlt Null         json  = json
runAlt (Bool False) json  = json
runAlt json         _     = json

-- TODO: Suppress errors
runTryCatch :: Filter -> Filter -> Json -> [Json]
runTryCatch try catch json = let tried = runFilter try json in
  if null tried -- TODO cambiar a si hay errores
  then
    runFilter catch json
  else
    tried

runIfElse :: Filter -> Filter -> Filter -> Json -> [Json]
runIfElse if' then' else' json = concatMap eval (runFilter if' json)
  where eval cond = runFilter (if jsonBool cond then  then' else else') json

-- Error handling --
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> String
jsonShowError json@(Number  _)  = "number ("  <> show json <> ")"
jsonShowError json@(String  _)  = "string ("  <> show json <> ")"
jsonShowError json@(Bool    _)  = "boolean (" <> show json <> ")"
jsonShowError json@(Object  _)  = "object ("  <> show json <> ")"
jsonShowError json@(Array   _)  = "array ("   <> show json <> ")"
jsonShowError json@Null         = "null ("    <> show json <> ")"
