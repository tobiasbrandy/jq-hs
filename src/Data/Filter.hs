module Data.Filter (
  Filter (..)
, FuncParam (..)
, filterRun
) where

import Prelude hiding (exp, seq, any, filter, init)

import Data.Json (Json (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as Map
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, ap, liftM2, when, foldM)
import TextShow (showt)
import Data.HashMap.Internal.Strict (HashMap)
import Data.Either (rights)

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
  | Update   Filter Filter   -- |=

  -- Comparison operators
  | Eq  Filter Filter         -- ==
  | Neq Filter Filter         -- !=
  | Lt  Filter Filter         -- <
  | Le  Filter Filter         -- <=
  | Gt  Filter Filter         -- >
  | Ge  Filter Filter         -- >=
  | Or  Filter Filter         -- or
  | And Filter Filter         -- and

  -- Reductions
  | Reduce  Filter Text Filter Filter         -- reduce stream_expression as $name (initial_value; update_expression)
  | Foreach Filter Text Filter Filter Filter  -- foreach stream_expression as $name (initial_value; update_expression; extract_expression)

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
  fr_funcs  :: HashMap (Text, Int) (Seq Filter -> Json -> FilterRun [Either Text Json]),
  fr_file   :: FilterRunFile,
  fr_current_func :: (Text, Int, Seq Filter -> Json -> FilterRun [Either Text Json])
}

filterRunInitState :: FilterRunState
filterRunInitState = FilterRunState {
  fr_vars         = Map.empty,
  fr_funcs        = Map.empty,
  fr_file         = TopLevel,
  fr_current_func = ("<top_level>", 0, error "No recursive function for top level")
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

filterRunSetState :: FilterRunState -> FilterRun ()
filterRunSetState s = FilterRun $ const (s, ())

filterRunGetState :: FilterRun FilterRunState
filterRunGetState = FilterRun $ \s -> (s, s)

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_file } -> (s, fr_file == TopLevel)

filterRunVarInsert :: Text -> Json -> FilterRun ()
filterRunVarInsert name body = FilterRun $ \s@FilterRunState { fr_vars } -> (s { fr_vars = Map.insert name body fr_vars }, ())

filterRunVarGet :: Text -> FilterRun (Maybe Json)
filterRunVarGet name = FilterRun $ \s@FilterRunState { fr_vars } -> (s, Map.lookup name fr_vars)

filterRunFuncInsert :: Text -> Int -> (Seq Filter -> Json -> FilterRun [Either Text Json]) -> FilterRun ()
filterRunFuncInsert name argc f = FilterRun $ \s@FilterRunState { fr_funcs } -> (s { fr_funcs = Map.insert (name, argc) f fr_funcs }, ())

filterRunFuncGet :: Text -> Int -> FilterRun (Maybe (Seq Filter -> Json -> FilterRun [Either Text Json]))
filterRunFuncGet name argc = FilterRun $ \s@FilterRunState { fr_funcs, fr_current_func = (c_name, c_argc, c_f) } ->
  if name == c_name && argc == c_argc
  then (s, Just c_f)
  else (s, Map.lookup (name, argc) fr_funcs)

type JsonOrErr = Either Text Json

-- TODO(tobi): Si agregamos modulos, agregar en que file paso el error/no esta defninida la cosa (como jq)
runFilter :: Filter -> Json -> FilterRun [JsonOrErr]
-- Basic
runFilter Identity                  json  = retSingleJson json
runFilter Empty                     _     = return []
runFilter Recursive                 json  = runRecursive json
runFilter (Json json)               _     = retSingleJson json
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
runFilter (Assign left right)       json  = notImplemented "Assign"
runFilter (Update left right)       json  = notImplemented "Update"
-- Comparison operators
runFilter (Eq   left right)         json  = runComparison (==)  left right  json
runFilter (Neq  left right)         json  = runComparison (/=)  left right  json
runFilter (Lt   left right)         json  = runComparison (<)   left right  json
runFilter (Le   left right)         json  = runComparison (<=)  left right  json
runFilter (Gt   left right)         json  = runComparison (>)   left right  json
runFilter (Ge   left right)         json  = runComparison (>=)  left right  json
runFilter (Or   left right)         json  = runBoolComp   (||)  left right  json
runFilter (And  left right)         json  = runBoolComp   (&&)  left right  json
-- Reductions
runFilter (Reduce exp name initial update)          json  = runReduce   exp name initial update         json
runFilter (Foreach exp name initial update extract) json  = runForeach  exp name initial update extract json
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
  ogState <- filterRunGetState
  ret <- concatMapMOrErr run bodies
  filterRunSetState ogState
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
runProject Null          _             = retJson Null
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
    slice Null  _ _ = retJson Null
    slice any   _ _ = retErr (jsonShowError any <> " cannot be sliced, only arrays")

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

runReduce :: Filter -> Text -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runReduce exp name initial update json = do
  stream  <- runFilter exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret <- mapM (\base -> foldMOrErr run base stream) init
  filterRunSetState ogState
  return ret
  where
    run ret val = do
      filterRunVarInsert name val
      newRet <- runFilter update ret
      if null newRet
      then
        retJson Null
      else
        return $ last <$> sequence newRet

-- TOD(tobi): Malisimo ir concatenando. Se podria usar dlist, pero habria que usarlas siempre para que valga la pena.
runForeach :: Filter -> Text -> Filter -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runForeach exp name initial update extract json = do
  stream  <- runFilter exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret <- concatMapMOrErr (\base -> snd <$> foldM run (base, []) stream) init
  filterRunSetState ogState
  return ret
  where
    run (dot, ret) (Right val) = do
      filterRunVarInsert name val
      updated   <- runFilter update dot
      extracted <- concatMapMOrErr (runFilter extract) updated
      let newDot = let updatedOk = rights updated in if null updatedOk then dot else last updatedOk
      return (newDot, ret <> extracted)
    run (dot, ret) err = return (dot, ret <> [err])

runFuncDef :: Text -> Seq FuncParam -> Filter -> Filter -> Json -> FilterRun [JsonOrErr]
runFuncDef name params body next json = do
  let argc = Seq.length params
  -- Conseguimos el state de la funcion, es decir, el current state con el current_func actualizado
  ogState <- filterRunGetState
  let state = ogState { fr_current_func = (name, argc, runFunc state params body) }
  -- Insertamos la funcion en el state
  filterRunFuncInsert name argc $ runFunc state params body
  -- Ejecutamos next con la funcion ya declarada
  ret <- runFilter next json
  -- Al retornar, solo dejamos la funcion declarada si estamos en un modulo, sino restauramos el state original
  isTopLevel <- filterRunIsTopLevel
  when isTopLevel $
    filterRunSetState ogState
  return ret

runFuncCall :: Text -> Seq Filter -> Json -> FilterRun [JsonOrErr]
runFuncCall name args json = do
  -- Guardar el state original
  ogState <- filterRunGetState
  -- Buscar la funcion en el state
  mFunc <- filterRunFuncGet name $ Seq.length args
  -- Ejecutar la funcion
  ret <- case mFunc of
    Nothing   -> retSingleErr $ name <> "/" <> showt (Seq.length args) <> " is not defined"
    Just f -> f args json
  -- Restaurar el state original
  filterRunSetState ogState
  return ret

runFunc :: FilterRunState -> Seq FuncParam -> Filter -> Seq Filter -> Json -> FilterRun [Either Text Json]
runFunc state params body args json = do
  -- Mapear args con params (cross product) y agregarlos al state guardado
  states <- foldr argCrossState (return [Right state]) (Seq.zip params args)
  -- Ejecutar el body con cada state calculado
  concatMapMOrErr runBodyWithState states
  where
    runBodyWithState s = do
      filterRunSetState s
      runFilter body json

    argCrossState (param, arg) states = concatMapMOrErr (insertArgInState param arg) =<< states

    insertArgInState (VarParam    param) arg s@FilterRunState { fr_vars  } = mapMOrErr (\argVal -> return $ Right $ s { fr_vars = Map.insert param argVal fr_vars }) =<< runFilter arg json
    insertArgInState (FilterParam param) arg s@FilterRunState { fr_funcs } = return $ (:[]) $ Right $ s { fr_funcs = Map.insert (param, 0) (runFunc s Seq.empty arg) fr_funcs }

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

foldMOrErr :: (Monad m, Foldable t) => (b -> a -> m (Either err b)) -> Either err b -> t (Either err a) -> m (Either err b)
foldMOrErr f = foldM run
  where
    run _           (Left err)  = return $ Left err
    run (Right ret) (Right x)   = f ret x
    run (Left _)    _           = error "Should never happen. Case 1 already catches this case. We want to short circuit on errors."

concatMapM :: (Traversable t, Monad f) => (a -> f [b]) -> t a -> f [b]
concatMapM f = fmap concat . mapM f

mapMAndErr :: (Traversable t, Monad m) => (Either err ok' -> b) -> (ok -> m b) -> t (Either err ok) -> m (t b)
mapMAndErr errF f = mapM $ ifErrElse (return . errF) f

mapMOrErr :: (Traversable t, Monad m) => (ok -> m (Either err ok')) -> t (Either err ok) -> m (t (Either err ok'))
mapMOrErr f = mapM $ ifErrElse return f

concatMapMOrErr :: (Traversable t, Monad m) => (ok -> m [Either err ok']) -> t (Either err ok) -> m [Either err ok']
concatMapMOrErr f = fmap concat . mapMAndErr (:[]) f
