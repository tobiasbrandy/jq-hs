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
import Control.Monad (liftM, ap, liftM2, when)
import TextShow (showt)
import Data.HashMap.Internal.Strict (HashMap)

data Filter
  -- Basic
  = Identity
  | Empty
  | Recursive
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
  | FuncDef Text (Seq FuncParam) Filter Filter
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

data FilterRunFile
  = TopLevel
  | Module Text
  deriving(Eq, Show)

filterRun :: Filter -> Json -> [Either Text Json]
filterRun filter json = let
    (FilterRun f) = runFilter filter json
    (_, ret)      = f filterRunInitState
  in ret

------------------------ Internal --------------------------

data FilterRunState = FilterRunState {
  fr_vars   :: HashMap Text Json,
  fr_funcs  :: HashMap (Text, Int) (FilterRunState, Seq FuncParam, Filter),
  fr_file   :: FilterRunFile
}

filterRunInitState :: FilterRunState
filterRunInitState = FilterRunState {
  fr_vars   = Map.empty,
  fr_funcs  = Map.empty,
  fr_file   = TopLevel
}

type FilterRunResult a = (FilterRunState, a)

newtype FilterRun a = FilterRun (FilterRunState -> FilterRunResult a)

instance Functor FilterRun where
  fmap = liftM

instance Applicative FilterRun where
  pure x = FilterRun (, x)

  (<*>) = ap

instance Monad FilterRun where
  m >>= k = FilterRun $ \s ->
    let
      FilterRun f   = m
      (s', a)       = f s
      FilterRun f'  = k a
    in f' s'

filterRunGetState :: FilterRun FilterRunState
filterRunGetState = FilterRun $ \s -> (s, s)

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_file } -> (s, fr_file == TopLevel)

filterRunVarInsert :: Text -> Json -> FilterRun ()
filterRunVarInsert name body = FilterRun $ \s@FilterRunState { fr_vars } -> (s { fr_vars = Map.insert name body fr_vars }, ())

filterRunVarDelete :: Text -> FilterRun ()
filterRunVarDelete name = FilterRun $ \s@FilterRunState { fr_vars } -> (s { fr_vars = Map.delete name fr_vars }, ())

filterRunVarGet :: Text -> FilterRun (Maybe Json)
filterRunVarGet name = FilterRun $ \s@FilterRunState { fr_vars } -> (s, Map.lookup name fr_vars)

filterRunFuncInsert :: Text -> FilterRunState -> Seq FuncParam -> Filter -> FilterRun ()
filterRunFuncInsert name state params body = FilterRun $ \s@FilterRunState { fr_funcs } ->
  (s { fr_funcs = Map.insert (name, Seq.length params) (state, params, body) fr_funcs }, ())

filterRunFuncDelete :: Text -> Int -> FilterRun ()
filterRunFuncDelete name argCount = FilterRun $ \s@FilterRunState { fr_funcs } -> (s { fr_funcs = Map.delete (name, argCount) fr_funcs }, ())

filterRunFuncGet :: Text -> Int -> FilterRun (Maybe (FilterRunState, Seq FuncParam, Filter))
filterRunFuncGet name argCount = FilterRun $ \s@FilterRunState { fr_funcs } -> (s, Map.lookup (name, argCount) fr_funcs)

type JsonOrErr = Either Text Json

runFilter :: Filter -> Json -> FilterRun [JsonOrErr]
-- Basic
runFilter Identity                  json  = return [Right json]
runFilter Empty                     _     = return []
runFilter Recursive                 json  = runRecursive json
runFilter (Json json)               _     = return [Right json]
-- Variable
runFilter (Var name)                _     = runVar name
runFilter (VarDef name body next)   json  = runVarDef name body next json
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
runFilter (Pipe left right)         json  = concatMapMOrErr (runFilter right) =<< runFilter left json
runFilter (Alt left right)          json  = runBinary runAlt    left right  json
runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runFilter (Comma left right)        json  = liftM2 (<>) (runFilter left json) (runFilter right json)
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Assignment operators
runFilter (Assign   left right)     json  = notImplemented "Assign"
runFilter (UpdateA  left right)     json  = notImplemented "UpdateA"
runFilter (PlusA    left right)     json  = notImplemented "PlusA"
runFilter (MinusA   left right)     json  = notImplemented "MinusA"
runFilter (TimesA   left right)     json  = notImplemented "TimesA"
runFilter (DivA     left right)     json  = notImplemented "DivA"
runFilter (ModA     left right)     json  = notImplemented "ModA"
runFilter (AltA     left right)     json  = notImplemented "AltA"
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
runFilter (FuncDef name params body next) json = runFuncDef name params body next json
runFilter (FuncCall name args)    json    = runFuncCall name args json
-- Label & break
runFilter (Label label)             json  = notImplemented "Label"
runFilter (Break label)             json  = notImplemented "Break"
-- Special
runFilter (LOC file line)           _     = runLOC file line

-- Auxiliary functions --
notImplemented :: Text -> FilterRun [JsonOrErr]
notImplemented msg = return [Left $ msg <> " filter not implemented"]

runUnary :: (Json -> FilterRun JsonOrErr) -> Filter -> Json -> FilterRun [JsonOrErr]
runUnary op exp json = mapMOrErr op =<< runFilter exp json

runBinary :: (Json -> Json -> FilterRun JsonOrErr) -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runBinary op left right json = concatMapMOrErr (\l -> mapMOrErr (op l) =<< runFilter right json) =<< runFilter left json

runComparison :: (Json -> Json -> Bool) -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runComparison op = runBinary (\l -> retJson . Bool . op l)

runBoolComp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runBoolComp op = runComparison (\l r -> op (jsonBool l) (jsonBool r))

jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

-- Filter operators implementations --
runVar :: Text -> FilterRun [JsonOrErr]
runVar name = maybe (retSingleErr $ "$" <> name <> " is not defined") retSingleJson =<< filterRunVarGet name

runVarDef :: Text -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runVarDef name body next json = do
  bodies <- runFilter body json
  ret <- concatMapMOrErr run bodies
  filterRunVarDelete name
  return ret
  where
    run body' = do
      filterRunVarInsert name body'
      runFilter next json

runRecursive :: Json -> FilterRun [JsonOrErr]
runRecursive json@(Object m)    = (Right json :) <$> concatMapM runRecursive (Map.elems m)
runRecursive json@(Array items) = (Right json :) <$> concatMapM runRecursive items
runRecursive json               = return [Right json]

runArrayLit :: Filter -> Json -> FilterRun [JsonOrErr]
runArrayLit items json = (:[]) . fmap (Array . foldl' (:|>) Seq.empty) . sequence <$> runFilter items json

runObjectLit :: Seq (Filter, Filter) -> Json -> FilterRun [JsonOrErr]
runObjectLit entries json = mapOrErr (Right . Object) <$> foldr entryCrossMaps (return [Right Map.empty]) entries
  where
    entryCrossMaps (key, val) maps = let
        ks = runFilter key json
        vs = runFilter val json
      in concatMapMOrErr (\k -> concatMapMOrErr (\v -> mapMOrErr (insertKvInMap k v) =<< maps) =<< vs) =<< ks

    insertKvInMap (String key)  val m  = return $ Right $ Map.insert key val m
    insertKvInMap any           _   _  = return $ Left ("Cannot use " <> jsonShowError any <> " as object key")

runIter :: Json -> FilterRun [JsonOrErr]
runIter (Array items)     = return $ sequence $ Right $ toList items
runIter (Object entries)  = return $ sequence $ Right $ Map.elems entries
runIter any               = retSingleErr ("Cannot iterate over " <> jsonShowError any)

runProject :: Json -> Json -> FilterRun JsonOrErr
runProject (Object m)    (String key)  = retJson $ Map.findWithDefault Null key m
runProject (Array items) (Number n)    = retJson $
  if isInteger n
  then case toBoundedInteger n of
    Nothing -> Null
    Just i  -> fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
  else Null
runProject anyl          anyr          = retErr ("Cannot index " <> jsonShowError anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun [JsonOrErr]
runSlice term left right json = let
    ts = runFilter term json
    ls = getIndeces (0::Int) left json
  in concatMapMOrErr (\t -> concatMapMOrErr (\l -> mapMOrErr (slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items)  = Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe (retSingleJson $ Number $ fromInteger $ toInteger def) (`runFilter` j) indexExp

    slice (Array items) (Number l)  (Number r)  = retJson $ Array $ seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r)) items
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Array _) anyl anyr = retErr (
      "Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr
      )
    slice any _ _ = retErr (jsonShowError any <> " cannot be sliced, only arrays")

    seqSlice l r = Seq.take (r-l) . Seq.drop l

runNeg :: Json -> FilterRun JsonOrErr
runNeg (Number n)  = retJson $ Number $ negate n
runNeg any         = retErr (jsonShowError any <> " cannot be negated")

runPlus :: Json -> Json -> FilterRun JsonOrErr
runPlus (Number l) (Number r)  = retJson $ Number  $ l +  r
runPlus (Array l)  (Array  r)  = retJson $ Array   $ l <> r
runPlus (String l) (String r)  = retJson $ String  $ l <> r
runPlus (Object l) (Object r)  = retJson $ Object  $ r <> l -- On collisions conserves the entry from the right
runPlus Null       any         = retJson   any
runPlus any        Null        = retJson   any
runPlus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be added")

runMinus :: Json -> Json -> FilterRun JsonOrErr
runMinus (Number l) (Number r)  = retJson $ Number  $ l - r
runMinus (Array l)  (Array  r)  = retJson $ Array   $ Seq.filter (`notElem` r) l
runMinus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

runTimes :: Json -> Json -> FilterRun JsonOrErr
runTimes (Number l)   (Number r)    = retJson $ Number  $ l * r
runTimes (String l)   (Number r)    = retJson $ if r > 0 then String $ T.replicate (floor r) l else Null
runTimes l@(Object _) r@(Object  _) = retJson $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
runTimes l            r             = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

runDiv :: Json -> Json -> FilterRun JsonOrErr
runDiv jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = retJson $ Number $ l / r
runDiv (String l) (String r) = retJson $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
runDiv l          r          = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

runMod :: Json -> Json -> FilterRun JsonOrErr
runMod jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = retJson $ Number $ fromInteger $ truncate l `mod` truncate (abs r)
runMod l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

runAlt :: Json -> Json -> FilterRun JsonOrErr
runAlt Null         json  = retJson json
runAlt (Bool False) json  = retJson json
runAlt json         _     = retJson json

runTryCatch :: Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runTryCatch try catch json = concatMapM errorToCatched =<< runFilter try json
  where
    catched = runFilter catch json

    errorToCatched (Left _) = catched
    errorToCatched right    = return [right]

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runIfElse if' then' else' json = concatMapMOrErr eval =<< runFilter if' json
  where eval cond = runFilter (if jsonBool cond then then' else else') json

runFuncDef :: Text -> Seq FuncParam -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runFuncDef name params body next json = do
  state <- filterRunGetState
  filterRunFuncInsert name state params body
  ret <- runFilter next json
  isTopLevel <- filterRunIsTopLevel
  when isTopLevel $
    filterRunFuncDelete name $ Seq.length params
  return ret

runFuncCall :: Text -> Seq Filter -> Json -> FilterRun [JsonOrErr]
runFuncCall name args json = undefined -- TODO!!! Re dificil :((
-- 1. Buscar la definicion en el state
-- 2. Mapear args con params (cross product) y agregarlos al state guardado
-- 3. Ejecutar el body con el state guardado
-- 4. Restaurar el state actual 

runLOC :: Integral a => Text -> a -> FilterRun [JsonOrErr]
runLOC file line = retSingleJson $ Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]

-- Error handling --
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> Text
jsonShowError json@(Number  _)  = "number ("  <> showt json <> ")"
jsonShowError json@(String  _)  = "string ("  <> showt json <> ")"
jsonShowError json@(Bool    _)  = "boolean (" <> showt json <> ")"
jsonShowError json@(Object  _)  = "object ("  <> showt json <> ")"
jsonShowError json@(Array   _)  = "array ("   <> showt json <> ")"
jsonShowError json@Null         = "null ("    <> showt json <> ")"

-- Utils --
retJson :: Json -> FilterRun JsonOrErr
retJson = return . Right

retErr :: Text -> FilterRun JsonOrErr
retErr = return . Left

retSingleJson :: Json -> FilterRun [JsonOrErr]
retSingleJson = return . (:[]) . Right

retSingleErr :: Text -> FilterRun [JsonOrErr]
retSingleErr = return . (:[]) . Left

ifErrElse :: (Either err ok' -> b) -> (ok -> b) -> Either err ok -> b
ifErrElse err else' okOrErr = case okOrErr of
  (Left x) -> err (Left x)
  Right json -> else' json

mapOrErr :: (ok -> Either err ok') -> [Either err ok] -> [Either err ok']
mapOrErr f = map $ ifErrElse id f

concatMapM :: (Traversable t, Monad f) => (a -> f [b]) -> t a -> f [b]
concatMapM f = fmap concat . mapM f

mapMAndErr :: (Traversable t, Monad m) => (Either err ok' -> b) -> (ok -> m b) -> t (Either err ok) -> m (t b)
mapMAndErr errF f = mapM $ ifErrElse (return . errF) f

mapMOrErr :: (Traversable t, Monad m) => (ok -> m (Either err ok')) -> t (Either err ok) -> m (t (Either err ok'))
mapMOrErr f = mapM $ ifErrElse return f

concatMapMOrErr :: (Traversable t, Monad m) => (ok -> m [Either err ok']) -> t (Either err ok) -> m [Either err ok']
concatMapMOrErr f = fmap concat . mapMAndErr (:[]) f
