module Data.Filter.Internal.Run (
-- Public
  filterRunExp
, filterRunModule

, FilterRunFile (..)
, FilterFunc

-- Private
, runFilter

-- FilterRun
, FilterRun

-- Utils
, jsonBool

-- Errors
, jsonShowError
) where

import Prelude hiding (exp, seq, any, filter, init)

import Data.Filter (Filter (..), FuncParam (..))

import Data.Filter.Internal.Result
  ( FilterRet (..)
  , retOk
  , retErr
  , applyRet
  , retToEither

  , FilterResult
  , foldrRet
  , resultOk
  , resultErr
  , resultHalt
  , mapMRet
  , concatMapMRet
  , mapRet
  , foldMRet

  , concatMapM
  )

import Data.Json (Json (..), jsonShowType)

import Data.Text (Text)
import Data.Sequence (Seq ((:|>)))
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, ap, liftM2, when, foldM)
import TextShow (showt)

------------------------ State --------------------------

data FilterRunFile
  = TopLevel
  | Module Text
  deriving(Eq, Show)

type FilterFunc = Seq Filter -> Json -> FilterRun (FilterResult Json)

data FilterRunState = FilterRunState {
  fr_vars   :: HashMap Text Json,
  fr_funcs  :: HashMap (Text, Int) FilterFunc,
  fr_labels :: HashSet Text,
  fr_file   :: FilterRunFile,
  fr_current_func :: (Text, Int, FilterFunc)
}

filterRunInitState :: FilterRunFile -> HashMap (Text, Int) FilterFunc -> FilterRunState
filterRunInitState file builtins = FilterRunState {
  fr_vars         = Map.empty,
  fr_funcs        = builtins,
  fr_labels       = Set.empty,
  fr_file         = file,
  fr_current_func = ("<top_level>", 0, error "No recursive function for top level")
}

------------------------ Monad --------------------------

newtype FilterRun a = FilterRun (FilterRunState -> (FilterRunState, a))

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

filterRunVarInsert :: Text -> Json -> FilterRun ()
filterRunVarInsert name body = FilterRun $ \s@FilterRunState { fr_vars } -> (s { fr_vars = Map.insert name body fr_vars }, ())

filterRunVarGet :: Text -> FilterRun (Maybe Json)
filterRunVarGet name = FilterRun $ \s@FilterRunState { fr_vars } -> (s, Map.lookup name fr_vars)

filterRunFuncInsert :: Text -> Int -> (Seq Filter -> Json -> FilterRun (FilterResult Json)) -> FilterRun ()
filterRunFuncInsert name argc f = FilterRun $ \s@FilterRunState { fr_funcs } -> (s { fr_funcs = Map.insert (name, argc) f fr_funcs }, ())

filterRunFuncGet :: Text -> Int -> FilterRun (Maybe (Seq Filter -> Json -> FilterRun (FilterResult Json)))
filterRunFuncGet name argc = FilterRun $ \s@FilterRunState { fr_funcs, fr_current_func = (c_name, c_argc, c_f) } ->
  if name == c_name && argc == c_argc
  then (s, Just c_f)
  else (s, Map.lookup (name, argc) fr_funcs)

filterRunAddLabel :: Text -> FilterRun ()
filterRunAddLabel label = FilterRun $ \s@FilterRunState { fr_labels } -> (s { fr_labels = Set.insert label fr_labels }, ())

filterRunExistsLabel :: Text -> FilterRun Bool
filterRunExistsLabel label = FilterRun $ \s@FilterRunState { fr_labels } -> (s, Set.member label fr_labels)

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_file } -> (s, fr_file == TopLevel)

------------------------ Run --------------------------

filterRunExp :: HashMap (Text, Int) FilterFunc -> Filter -> Json -> [Either Text Json]
filterRunExp funcs filter json = let
    (FilterRun f) = runFilter filter json
    (_, ret)      = f $ filterRunInitState TopLevel funcs
  in foldrRet ((:) . retToEither) [] ret

filterRunModule :: Text -> HashMap (Text, Int) FilterFunc -> Filter -> HashMap (Text, Int) FilterFunc
filterRunModule moduleName funcs filter = let
    (FilterRun f) = runFilter filter Null
    (FilterRunState { fr_funcs }, _) = f $ filterRunInitState (Module moduleName) funcs
  in fr_funcs

-- TODO(tobi): Si agregamos modulos, agregar en que file paso el error/no esta defninida la cosa (como jq)
runFilter :: Filter -> Json -> FilterRun (FilterResult Json)
-- Basic
runFilter Identity                  json  = resultOk json
runFilter Empty                     _     = return []
runFilter Recursive                 json  = return $ map Ok $ runRecursive json
runFilter (Json json)               _     = resultOk json
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
-- Flow operators
runFilter (Pipe left right)         json  = concatMapMRet (runFilter right) =<< runFilter left json
runFilter (Alt left right)          json  = runBinary runAlt    left right  json
runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runFilter (Comma left right)        json  = liftM2 (<>) (runFilter left json) (runFilter right json)
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Assignment operators
runFilter (Assign left right)       json  = notImplemented "Assign"
runFilter (Update left right)       json  = notImplemented "Update"
-- Comparison operators
runFilter (Or   left right)         json  = runBoolComp   (||)  left right  json
runFilter (And  left right)         json  = runBoolComp   (&&)  left right  json
-- Reductions
runFilter (Reduce exp name initial update)          json  = runReduce   exp name initial update         json
runFilter (Foreach exp name initial update extract) json  = runForeach  exp name initial update extract json
-- Functions
runFilter (FuncDef name params body next) json = runFuncDef name params body next json
runFilter (FuncCall name args)      json  = runFuncCall name args json
-- Label & break
runFilter (Label label next)        json  = runLabel label next json
runFilter (Break label)             _     = runBreak label
-- Special
runFilter (LOC file line)           _     = runLOC file line

------------------------ Filter Operators Implementations --------------------------

runVar :: Text -> FilterRun (FilterResult Json)
runVar name = maybe (resultErr $ "$" <> name <> " is not defined") resultOk =<< filterRunVarGet name

runVarDef :: Text -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runVarDef name body next json = do
  bodies <- runFilter body json
  ogState <- filterRunGetState
  ret <- concatMapMRet run bodies
  filterRunSetState ogState
  return ret
  where
    run body' = do
      filterRunVarInsert name body'
      runFilter next json

runRecursive :: Json -> [Json]
runRecursive json@(Object m)    = (json :) $ concatMap runRecursive (Map.elems m)
runRecursive json@(Array items) = (json :) $ concatMap runRecursive items
runRecursive json               = [json]

runArrayLit :: Filter -> Json -> FilterRun (FilterResult Json)
runArrayLit items json = (:[]) . fmap (Array . foldl' (:|>) Seq.empty) . sequence <$> runFilter items json

runObjectLit :: Seq (Filter, Filter) -> Json -> FilterRun (FilterResult Json)
runObjectLit entries json = mapRet (Ok . Object) <$> foldr entryCrossMaps (resultOk Map.empty) entries
  where
    entryCrossMaps (key, val) maps = let
        ks = runFilter key json
        vs = runFilter val json
      in concatMapMRet (\k -> concatMapMRet (\v -> mapMRet (insertKvInMap k v) =<< maps) =<< vs) =<< ks

    insertKvInMap (String key)  val m  = retOk $ Map.insert key val m
    insertKvInMap any           _   _  = retErr ("Cannot use " <> jsonShowError any <> " as object key")

runIter :: Json -> FilterRun (FilterResult Json)
runIter (Array items)     = return $ map Ok $ toList items
runIter (Object entries)  = return $ map Ok $ Map.elems entries
runIter any               = resultErr ("Cannot iterate over " <> jsonShowError any)

runProject :: Json -> Json -> FilterRun (FilterRet Json)
runProject (Object m)    (String key)  = retOk $ Map.findWithDefault Null key m
runProject (Array items) (Number n)    = retOk $
  if isInteger n
  then case toBoundedInteger n of
    Nothing -> Null
    Just i  -> fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
  else Null
runProject Null          _             = retOk Null
runProject anyl          anyr          = retErr ("Cannot index " <> jsonShowType anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun (FilterResult Json)
runSlice term left right json = let
    ts = runFilter term json
    ls = getIndeces 0 left json
  in concatMapMRet (\t -> concatMapMRet (\l -> mapMRet (slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items)  = toInteger $ Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe (resultOk $ Number $ fromInteger def) (`runFilter` j) indexExp

    slice (Array items) (Number l)  (Number r)  = retOk $ Array $ seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r)) items
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Array _) anyl anyr = retErr (
      "Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr
      )
    slice Null  _ _ = retOk Null
    slice any   _ _ = retErr (jsonShowError any <> " cannot be sliced, only arrays or null")

    seqSlice l r = Seq.take (r-l) . Seq.drop l

runAlt :: Json -> Json -> FilterRun (FilterRet Json)
runAlt Null         json  = retOk json
runAlt (Bool False) json  = retOk json
runAlt json         _     = retOk json

runTryCatch :: Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runTryCatch try catch json = concatMapM errorToCatched =<< runFilter try json
  where
    catched = runFilter catch json

    errorToCatched (Err _)  = catched
    errorToCatched other    = return [other]

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runIfElse if' then' else' json = concatMapMRet eval =<< runFilter if' json
  where eval cond = runFilter (if jsonBool cond then then' else else') json

runReduce :: Filter -> Text -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runReduce exp name initial update json = do
  stream  <- runFilter exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret     <- mapM (\base -> foldMRet run base stream) init
  filterRunSetState ogState
  return ret
  where
    run ret val = do
      filterRunVarInsert name val
      newRet <- runFilter update ret
      if null newRet
      then
        retOk Null
      else
        return $ last <$> sequence newRet

-- TODO(tobi): Malisimo ir concatenando. Se podria usar dlist, pero habria que usarlas siempre para que valga la pena.
runForeach :: Filter -> Text -> Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runForeach exp name initial update extract json = do
  stream  <- runFilter exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret <- concatMapMRet (\base -> snd <$> foldM run (base, []) stream) init
  filterRunSetState ogState
  return ret
  where
    run (dot, ret) (Ok val) = do
      filterRunVarInsert name val
      updated   <- runFilter update dot
      extracted <- concatMapMRet (runFilter extract) updated
      let newDot = let oks = foldrRet (\x r -> applyRet (:r) (const r) x) [] updated in if null oks then dot else last oks
      return (newDot, ret <> extracted)
    run (dot, ret) other  = return (dot, ret <> [other])

runFuncDef :: Text -> Seq FuncParam -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runFuncDef name params body next json = do
  let argc = Seq.length params
  -- Conseguimos el state de la funcion, es decir, el current state con el current_func actualizado
  ogState <- filterRunGetState
  let state = ogState { fr_current_func = (name, argc, buildFilterFunc state params body) }
  -- Insertamos la funcion en el state
  filterRunFuncInsert name argc $ buildFilterFunc state params body
  -- Ejecutamos next con la funcion ya declarada
  ret <- runFilter next json
  -- Al retornar, solo dejamos la funcion declarada si estamos en un modulo, sino restauramos el state original
  isTopLevel <- filterRunIsTopLevel
  when isTopLevel $
    filterRunSetState ogState
  return ret

runFuncCall :: Text -> Seq Filter -> Json -> FilterRun (FilterResult Json)
runFuncCall name args json = do
  -- Guardar el state original
  ogState <- filterRunGetState
  -- Buscar la funcion en el state
  mFunc <- filterRunFuncGet name $ Seq.length args
  -- Ejecutar la funcion
  ret <- case mFunc of
    Nothing   -> resultErr $ name <> "/" <> showt (Seq.length args) <> " is not defined"
    Just f -> f args json
  -- Restaurar el state original
  filterRunSetState ogState
  return ret

buildFilterFunc :: FilterRunState -> Seq FuncParam -> Filter -> FilterFunc
buildFilterFunc state params body args json = do
  -- Mapear args con params (cross product) y agregarlos al state guardado
  states <- foldr argCrossState (resultOk state) (Seq.zip params args)
  -- Ejecutar el body con cada state calculado
  concatMapMRet runBodyWithState states
  where
    runBodyWithState s = do
      filterRunSetState s
      runFilter body json

    argCrossState (param, arg) states = concatMapMRet (insertArgInState param arg) =<< states

    insertArgInState (VarParam    param) arg s@FilterRunState { fr_vars  } = mapMRet (\argVal -> retOk $ s { fr_vars = Map.insert param argVal fr_vars }) =<< runFilter arg json
    insertArgInState (FilterParam param) arg s@FilterRunState { fr_funcs } = resultOk $ s { fr_funcs = Map.insert (param, 0) (buildFilterFunc s Seq.empty arg) fr_funcs }

runLabel :: Text -> Filter -> Json -> FilterRun (FilterResult Json)
runLabel label next json = do
  ogState <- filterRunGetState
  filterRunAddLabel label
  haltedRet <- runFilter next json
  filterRunSetState ogState
  
  return $ foldrRet filterLabelHalts [] haltedRet
  where
    filterLabelHalts h@(Halt label') ret = if label == label' then ret else h : ret
    filterLabelHalts other ret = other : ret

runBreak :: Text -> FilterRun (FilterResult Json)
runBreak label = do
  hasLabel <- filterRunExistsLabel label
  if hasLabel then resultHalt label else resultErr $ "$*label-" <> label <> " is not defined"

runLOC :: Integral a => Text -> a -> FilterRun (FilterResult Json)
runLOC file line = resultOk $ Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]

------------------------ Aux --------------------------

notImplemented :: Text -> FilterRun (FilterResult Json)
notImplemented msg = resultErr $ msg <> " filter not implemented"

runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op left right json = concatMapMRet (\l -> mapMRet (op l) =<< runFilter right json) =<< runFilter left json

runComparison :: (Json -> Json -> Bool) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runComparison op = runBinary (\l -> retOk . Bool . op l)

runBoolComp :: (Bool -> Bool -> Bool) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBoolComp op = runComparison (\l r -> op (jsonBool l) (jsonBool r))

jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

------------------------ Error Handling --------------------------
-- TODO(tobi): Agregar cantidad maxima de caracteres y luego ...
jsonShowError :: Json -> Text
jsonShowError json = jsonShowType json <> " (" <> showt json <> ")"
