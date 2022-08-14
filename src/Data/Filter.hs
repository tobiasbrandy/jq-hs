module Data.Filter (
  Filter (..)
, FuncParam (..)
, filterRun
) where

import Prelude hiding (exp, seq, any, filter)

import Data.Json (Json (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, ap, liftM2)
import TextShow (showt)

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

data FilterRunState = FilterRunState {
  fr_vars   :: [Text],
  fr_funcs  :: [Text]
}

filterRunInitState :: FilterRunState
filterRunInitState = FilterRunState {
  fr_vars   = [],
  fr_funcs  = []
}

data FilterRunResult a
  = Ok (FilterRunState, a)
  | Error Text

newtype FilterRun a = FilterRun (FilterRunState -> FilterRunResult a)

instance Functor FilterRun where
  fmap = liftM

instance Applicative FilterRun where
  pure x = FilterRun (\s -> Ok (s, x))

  (<*>) = ap

instance Monad FilterRun where
  m >>= k = FilterRun $ \s ->
    let FilterRun f = m in
      case f s of
        Error msg  -> Error msg
        Ok (s', a) -> let FilterRun f' = k a in f' s'

filterRun :: Filter -> Json -> Either Text [Json]
filterRun filter json = let (FilterRun f) = runFilter filter json in
  case f filterRunInitState of
    Ok (_, jsons) -> Right jsons
    Error msg     -> Left msg

filterRunFail :: Text -> FilterRun a
filterRunFail msg = FilterRun $ const $ Error msg

filterRunHasFailed :: FilterRun a -> FilterRun Bool
filterRunHasFailed (FilterRun f) = FilterRun $ \s -> Ok $
  case f s of
    Ok    _ -> (s, False)
    Error _ -> (s, True)

-- TODO(tobi): Agregar una monada y poner los errors ahi. Copiar los error messages de jq
runFilter :: Filter -> Json -> FilterRun [Json]
-- Basic
runFilter Identity                  json  = return [json]
runFilter Empty                     _     = return []
runFilter Recursive                 json  = runRecursive json
runFilter (Json json)               _     = return [json]
-- Variable
runFilter (Var name)                _     = filterRunFail "Var filter not implemented"
runFilter (VarDef name body)        _     = filterRunFail "VarDef filter not implemented"
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
runFilter (Pipe left right)         json  = concat <$> (mapM (runFilter right) =<< runFilter left json)
runFilter (Alt left right)          json  = runBinary runAlt    left right  json
runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runFilter (Comma left right)        json  = liftM2 (<>) (runFilter left json) (runFilter right json)
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Assignment operators
runFilter (Assign   left right)     json  = filterRunFail "Assign filter not implemented"
runFilter (UpdateA  left right)     json  = filterRunFail "UpdateA filter not implemented"
runFilter (PlusA    left right)     json  = filterRunFail "PlusA filter not implemented"
runFilter (MinusA   left right)     json  = filterRunFail "MinusA filter not implemented"
runFilter (TimesA   left right)     json  = filterRunFail "TimesA filter not implemented"
runFilter (DivA     left right)     json  = filterRunFail "DivA filter not implemented"
runFilter (ModA     left right)     json  = filterRunFail "ModA filter not implemented"
runFilter (AltA     left right)     json  = filterRunFail "AltA filter not implemented"
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
runFilter (FuncDef name params body)json = filterRunFail "FuncDef filter not implemented"
runFilter (FuncCall name params)    json  = filterRunFail "FuncCall filter not implemented"
-- Label & break
runFilter (Label label)             json  = filterRunFail "Label filter not implemented"
runFilter (Break label)             json  = filterRunFail "Break filter not implemented"
-- Special
runFilter (LOC file line)           _     = return [Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]]

-- Auxiliary functions --
runUnary :: (Json -> FilterRun Json) -> Filter -> Json -> FilterRun [Json]
runUnary op exp json = mapM op =<< runFilter exp json

runBinary :: (Json -> Json -> FilterRun Json) -> Filter -> Filter -> Json -> FilterRun [Json]
runBinary op left right json = do
  leftRet   <- runFilter left json
  rightRet  <- runFilter right json
  sequence [op l r | l <- leftRet, r <- rightRet]

runComparison :: (Json -> Json -> Bool) -> Filter -> Filter -> Json -> FilterRun [Json]
runComparison op = runBinary (\l -> return . Bool . op l)

runBoolComp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Json -> FilterRun [Json]
runBoolComp op = runComparison (\l r -> op (jsonBool l) (jsonBool r))

jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

-- Filter operators implementations --
runRecursive :: Json -> FilterRun [Json]
runRecursive json@(Object m)    = (json :) . concat <$> mapM runRecursive (Map.elems m)
runRecursive json@(Array items) = (json :) . concat <$> mapM runRecursive items
runRecursive json               = return [json]

runArrayLit :: Filter -> Json -> FilterRun [Json]
runArrayLit items json = (:[]) . Array . foldl' (:|>) Seq.empty <$> runFilter items json

runObjectLit :: Seq (Filter, Filter) -> Json -> FilterRun [Json]
runObjectLit entries json = map Object <$> foldr entryCrossMaps (return [Map.empty]) entries 
  where
    entryCrossMaps (key, val) mmaps = do
      ks    <- runFilter key json
      vs    <- runFilter val json
      maps  <- mmaps
      sequence $ concat [map (insertKvInMap k v) maps | k <- ks, v <- vs]

    insertKvInMap (String key)  val m  = return $ Map.insert key val m
    insertKvInMap any           _   _  = filterRunFail ("Cannot use " <> jsonShowError any <> " as object key")

runIter :: Json -> FilterRun [Json]
runIter (Array items)     = return $ toList items
runIter (Object entries)  = return $ Map.elems entries
runIter any               = filterRunFail ("Cannot iterate over " <> jsonShowError any)

runProject :: Json -> Json -> FilterRun Json
runProject (Object m)    (String key)  = return $ Map.findWithDefault Null key m
runProject (Array items) (Number n)    = return $
  if isInteger n
  then case toBoundedInteger n of
    Nothing -> Null
    Just i  -> fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
  else Null
runProject anyl          anyr          = filterRunFail ("Cannot index " <> jsonShowError anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun [Json]
runSlice term left right json = do
  ts <- runFilter term json
  ls <- getIndeces (0::Int) left json
  rs <- getIndeces (itemsLen $ Array Seq.empty) right json -- TODO CORREGIR URGENTE
  sequence [Array <$> slice t l r | t <- ts, l <- ls, r <- rs]
    where
      itemsLen (Array items)  = Seq.length items
      itemsLen _              = 0

      getIndeces def indexExp j = maybe (return $ (:[]) $ Number $ fromInteger $ toInteger def) (`runFilter` j) indexExp

      slice (Array items) (Number l)  (Number r)  = return $ seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r)) items
        where
          len = Seq.length items
          cycleIndex i = if i < 0 then len + i else i
      slice (Array _) anyl anyr = filterRunFail (
        "Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr
        )
      slice any _ _ = filterRunFail (jsonShowError any <> " cannot be sliced, only arrays")

      seqSlice l r = Seq.take (r-1) . Seq.drop l

runNeg :: Json -> FilterRun Json
runNeg (Number n)  = return $ Number $ negate n
runNeg any         = filterRunFail (jsonShowError any <> " cannot be negated")

runPlus :: Json -> Json -> FilterRun Json
runPlus (Number l) (Number r)  = return $ Number  $ l +  r
runPlus (Array l)  (Array  r)  = return $ Array   $ l <> r
runPlus (String l) (String r)  = return $ String  $ l <> r
runPlus (Object l) (Object r)  = return $ Object  $ r <> l -- On collisions conserves the entry from the right
runPlus Null       any         = return   any
runPlus any        Null        = return   any
runPlus l          r           = filterRunFail (jsonShowError l <> " and " <> jsonShowError r <> " cannot be added")

runMinus :: Json -> Json -> FilterRun Json
runMinus (Number l) (Number r)  = return $ Number  $ l - r
runMinus (Array l)  (Array  r)  = return $ Array   $ Seq.filter (`notElem` r) l
runMinus l          r           = filterRunFail (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

runTimes :: Json -> Json -> FilterRun Json
runTimes (Number l)   (Number r)    = return $ Number  $ l * r
runTimes (String l)   (Number r)    = return $ if r > 0 then String $ T.replicate (floor r) l else Null
runTimes l@(Object _) r@(Object  _) = return $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
runTimes l            r             = filterRunFail (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

runDiv :: Json -> Json -> FilterRun Json
runDiv jl@(Number l) jr@(Number r)
  | r == 0    = filterRunFail (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = return $ Number $ l / r
runDiv (String l) (String r) = return $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
runDiv l          r          = filterRunFail (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

runMod :: Json -> Json -> FilterRun Json
runMod jl@(Number l) jr@(Number r)
  | r == 0    = filterRunFail (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = return $ Number $ fromInteger $ truncate l `mod` truncate (abs r)
runMod l          r           = filterRunFail (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

runAlt :: Json -> Json -> FilterRun Json
runAlt Null         json  = return json
runAlt (Bool False) json  = return json
runAlt json         _     = return json

-- TODO: Suppress errors
runTryCatch :: Filter -> Filter -> Json -> FilterRun [Json]
runTryCatch try catch json = let tried = runFilter try json in do
  cond <- filterRunHasFailed tried
  if cond then runFilter catch json else tried

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun [Json]
runIfElse if' then' else' json = concat <$> (mapM eval =<< runFilter if' json)
  where eval cond = runFilter (if jsonBool cond then  then' else else') json

-- Error handling --
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> Text
jsonShowError json@(Number  _)  = "number ("  <> showt json <> ")"
jsonShowError json@(String  _)  = "string ("  <> showt json <> ")"
jsonShowError json@(Bool    _)  = "boolean (" <> showt json <> ")"
jsonShowError json@(Object  _)  = "object ("  <> showt json <> ")"
jsonShowError json@(Array   _)  = "array ("   <> showt json <> ")"
jsonShowError json@Null         = "null ("    <> showt json <> ")"
