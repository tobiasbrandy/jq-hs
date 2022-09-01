module Data.Filter.Internal.Run (
-- Public
  filterRunExp
, filterRunModule

, FilterRunFile (..)
, PathExpStatus (..)
, FilterFunc

-- Private
, runFilter

-- FilterRun
, FilterRun
, filterRunSetPathExp
, filterRunGetPathExp

-- Utils
, jsonBool
, invalidPathExpErr
, notPathExp
, ifPathExp
, runFilterNoPath

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
import Data.Sequence (Seq ((:|>), (:<|)))
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Foldable (Foldable(foldl', toList))
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (liftM, ap, liftM2, when, foldM)
import TextShow (showt)
import Data.Bifunctor (second)

------------------------ State --------------------------

data FilterRunFile
  = TopLevel
  | Module Text
  deriving(Eq, Show)

data PathExpStatus
  = PathOn
  | PathOff
  | PathTry
  deriving (Eq, Show)

type FilterFunc = Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))

data FilterRunState = FilterRunState {
  fr_vars         :: HashMap Text Json,
  fr_funcs        :: HashMap (Text, Int) FilterFunc,
  fr_labels       :: HashSet Text,
  fr_path_exp     :: PathExpStatus,
  fr_file         :: FilterRunFile,
  fr_current_func :: (Text, Int, FilterFunc)
}

filterRunInitState :: FilterRunFile -> HashMap (Text, Int) FilterFunc -> FilterRunState
filterRunInitState file builtins = FilterRunState {
  fr_vars         = Map.empty,
  fr_funcs        = builtins,
  fr_labels       = Set.empty,
  fr_path_exp     = PathOff,
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

filterRunFuncInsert :: Text -> Int -> (Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))) -> FilterRun ()
filterRunFuncInsert name argc f = FilterRun $ \s@FilterRunState { fr_funcs } -> (s { fr_funcs = Map.insert (name, argc) f fr_funcs }, ())

filterRunFuncGet :: Text -> Int -> FilterRun (Maybe (Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))))
filterRunFuncGet name argc = FilterRun $ \s@FilterRunState { fr_funcs, fr_current_func = (c_name, c_argc, c_f) } ->
  if name == c_name && argc == c_argc
  then (s, Just c_f)
  else (s, Map.lookup (name, argc) fr_funcs)

filterRunAddLabel :: Text -> FilterRun ()
filterRunAddLabel label = FilterRun $ \s@FilterRunState { fr_labels } -> (s { fr_labels = Set.insert label fr_labels }, ())

filterRunExistsLabel :: Text -> FilterRun Bool
filterRunExistsLabel label = FilterRun $ \s@FilterRunState { fr_labels } -> (s, Set.member label fr_labels)

filterRunSetPathExp :: PathExpStatus -> FilterRun ()
filterRunSetPathExp pathExp = FilterRun $ \s -> (s { fr_path_exp = pathExp }, ())

filterRunGetPathExp :: FilterRun PathExpStatus
filterRunGetPathExp = FilterRun $ \s@FilterRunState { fr_path_exp } -> (s, fr_path_exp)

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_file } -> (s, fr_file == TopLevel)

------------------------ Run --------------------------

filterRunExp :: HashMap (Text, Int) FilterFunc -> Filter -> Json -> [Either Text Json]
filterRunExp funcs filter json = let
    (FilterRun f) = runFilter filter json
    (_, ret)      = f $ filterRunInitState TopLevel funcs
  in foldrRet ((:) . retToEither . fmap fst) [] ret

filterRunModule :: Text -> HashMap (Text, Int) FilterFunc -> Filter -> HashMap (Text, Int) FilterFunc
filterRunModule moduleName funcs filter = let
    (FilterRun f) = runFilter filter Null
    (FilterRunState { fr_funcs }, _) = f $ filterRunInitState (Module moduleName) funcs
  in fr_funcs

runFilter :: Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
-- Basic
runFilter Identity                  json  = ifPathExp (resultOk (json, Just Seq.empty)) (resultOk (json, Nothing))
runFilter Empty                     _     = return []
runFilter Recursive                 json  = ifPathExp (return $ map (Ok . second Just) $ runRecursive json) (return $ map (Ok . second (const Nothing)) $ runRecursive json) -- MMM
runFilter (Json json)               _     = notPathExp $ resultOk json
-- Variable
runFilter (Var name)                _     = notPathExp $ runVar name
runFilter (VarDef name body next)   json  = runVarDef name body next json
-- Literals
runFilter (ArrayLit items)          json  = notPathExp $ runArrayLit items json
runFilter (ObjectLit entries)       json  = notPathExp $ runObjectLit entries json
-- Projections
runFilter (Project term exp)        json  = runProject term exp json
runFilter (Slice term left right)   json  = runSlice term left right json
runFilter Iter                      json  = runIter json
-- Flow operators
runFilter (Pipe left right)         json  = concatMapMRet (\(jl, pl) -> mapMRet (\(jr, pr) -> retOk (jr, pl <> pr)) =<< runFilter right jl) =<< runFilter left json
runFilter (Alt left right)          json  = runAlt    left right  json
runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runFilter (Comma left right)        json  = liftM2 (<>) (runFilter left json) (runFilter right json)
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Assignment operators
runFilter (Assign left right)       json  = notImplemented "Assign"
runFilter (Update left right)       json  = notImplemented "Update"
-- Comparison operators
runFilter (Or   left right)         json  = notPathExp $ runBoolComp   (||)  left right  json
runFilter (And  left right)         json  = notPathExp $ runBoolComp   (&&)  left right  json
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
runFilter (LOC file line)           _     = notPathExp $ runLOC file line

------------------------ Filter Operators Implementations --------------------------

runRecursive :: Json -> [(Json, Seq Json)]
runRecursive json@(Object m)     = ((json, Seq.empty) :) $ concatMap (\(k, v) -> map (second (String k :<|)) $ runRecursive v) (Map.toList m)
runRecursive json@(Array items)  = ((json, Seq.empty) :) $ concatMap (\(idx, item) -> map (second (Number (fromInteger idx) :<|)) $ runRecursive item) (zip [0..] $ toList items)
runRecursive json                = [(json, Seq.empty)]

runVar :: Text -> FilterRun (FilterResult Json)
runVar name = maybe (resultErr $ "$" <> name <> " is not defined") resultOk =<< filterRunVarGet name

runVarDef :: Text -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runVarDef name body next json = do
  bodies <- runFilterNoPath body json
  ogState <- filterRunGetState
  ret <- concatMapMRet run bodies
  filterRunSetState ogState
  return ret
  where
    run body' = do
      filterRunVarInsert name body'
      runFilter next json

runArrayLit :: Filter -> Json -> FilterRun (FilterResult Json)
runArrayLit items json = (:[]) . fmap (Array . foldl' (:|>) Seq.empty) . sequence <$> runFilterNoPath items json

runObjectLit :: Seq (Filter, Filter) -> Json -> FilterRun (FilterResult Json)
runObjectLit entries json = mapRet (Ok . Object) <$> foldr entryCrossMaps (resultOk Map.empty) entries
  where
    entryCrossMaps (key, val) maps = let
        ks = runFilterNoPath key json
        vs = runFilterNoPath val json
      in concatMapMRet (\k -> concatMapMRet (\v -> mapMRet (insertKvInMap k v) =<< maps) =<< vs) =<< ks

    insertKvInMap (String key)  val m  = retOk $ Map.insert key val m
    insertKvInMap any           _   _  = retErr ("Cannot use " <> jsonShowError any <> " as object key")

runIter :: Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runIter (Array items)     = ifPathExp
  (return $ zipWith (\idx item -> Ok (item, Just $ Seq.singleton $ Number $ fromInteger idx)) [0..] (toList items))
  (return $ map (Ok . (, Nothing)) $ toList items)
runIter (Object entries)  = ifPathExp
  (return $ map (\(k, v) -> Ok (v, Just $ Seq.singleton $ String k)) $ Map.toList entries)
  (return $ map (Ok . (, Nothing)) $ Map.elems entries)
runIter any               = resultErr ("Cannot iterate over " <> jsonShowError any)

runProject :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runProject term exp json = concatMapMRet (\l -> mapMRet (project l) =<< runFilterNoPath exp json) =<< runFilter term json
  where
    project (Object m, lp)    r@(String key)  = let p = (:|> r) <$> lp in retOk $ maybe (Null, p) (, p) $ Map.lookup key m
    project (Array items, lp) r@(Number n)    = let p = (:|> r) <$> lp in retOk $
      if isInteger n
      then case toBoundedInteger n of
        Nothing -> (Null, p)
        Just i  -> (, p) $ fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
      else (Null, p)
    project (Null, lp)        r@(String _)  = retOk (Null, (:|> r) <$> lp)
    project (Null, lp)        r@(Number _)  = retOk (Null, (:|> r) <$> lp)
    project (anyl,_)          anyr          = retErr ("Cannot index " <> jsonShowType anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runSlice term left right json = let
    ts = runFilter term json
    ls = getIndeces 0 left json
  in concatMapMRet (\t -> concatMapMRet (\l -> mapMRet (slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items,_)  = toInteger $ Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe (resultOk $ Number $ fromInteger def) (`runFilterNoPath` j) indexExp

    slice :: (Json, Maybe (Seq Json)) -> Json -> Json -> FilterRun (FilterRet (Json, Maybe (Seq Json)))
    slice (Array items, pl) (Number l)  (Number r)  = let
        start     = floor l
        end       = ceiling r
        sliced    = Array $ seqSlice (cycleIndex start) (cycleIndex end) items
        slicePath = Object $ Map.fromList [("start", toNum $ toInteger start), ("end", Number $ fromInteger $ toInteger end)]
      in retOk $ (sliced,) $ (:|> slicePath) <$> pl
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Null, pl)  (Number l)  (Number r) = let
        slicePath = Object $ Map.fromList [("start", toNum $ floor l), ("end", toNum $ ceiling r)]
      in retOk (Null, (:|> slicePath) <$> pl)
    slice (Array _,_)   anyl  anyr  = sliceError anyl anyr
    slice (Null,_)      anyl  anyr  = sliceError anyl anyr
    slice (any,_)       _     _     = retErr (jsonShowError any <> " cannot be sliced, only arrays or null")

    toNum = Number . fromInteger

    seqSlice l r = Seq.take (r-l) . Seq.drop l

    sliceError anyl anyr = retErr ("Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr)

runAlt :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runAlt left right json = concatMapMRet (\l -> mapMRet (run l) =<< runFilter right json) =<< runFilterTryPath left json
  where
    run (Null,_)        (j, p)   = retOk (j, p)
    run (Bool False,_)  (j, p)   = retOk (j, p)
    run (j, p)       _           = do
      pathStatus <- filterRunGetPathExp
      if pathStatus == PathOn && isNothing p then invalidPathExpErr j else retOk (j, p)

runTryCatch :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runTryCatch try catch json = concatMapM errorToCatched =<< runFilter try json
  where
    catched = runFilter catch json

    errorToCatched (Err _)  = catched
    errorToCatched other    = return [other]

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runIfElse if' then' else' json = concatMapMRet eval =<< runFilterNoPath if' json
  where eval cond = runFilter (if jsonBool cond then then' else else') json

runReduce :: Filter -> Text -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runReduce exp name initial update json = do
  stream  <- runFilterNoPath exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret     <- mapM (\base -> foldMRet run base stream) init
  filterRunSetState ogState
  return ret
  where
    run (jret, pret) val = do
      filterRunVarInsert name val
      newRet <- mapMRet (\(jnret, pnret) -> retOk (jnret, pret <> pnret)) =<< runFilter update jret
      if null newRet
      then
        retOk (Null, const (Just Seq.empty) =<< pret)
      else
        return $ last <$> sequence newRet

-- TODO(tobi): Malisimo ir concatenando. Se podria usar dlist, pero habria que usarlas siempre para que valga la pena.
runForeach :: Filter -> Text -> Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runForeach exp name initial update extract json = do
  stream  <- runFilterNoPath exp json
  init    <- runFilter initial json
  ogState <- filterRunGetState
  ret <- concatMapMRet (\base -> snd <$> foldM run (base, []) stream) init
  filterRunSetState ogState
  return ret
  where
    run (dot@(jdot, pdot), ret) (Ok val) = do
      filterRunVarInsert name val
      updated   <- mapMRet (\(ju, pu) -> retOk (ju, pdot <> pu)) =<< runFilter update jdot
      extracted <- concatMapMRet (\(ju, pu) -> mapMRet (\(je, pe) -> retOk (je, pu <> pe)) =<< runFilter extract ju) updated
      let newDot = let oks = foldrRet (\x r -> applyRet (:r) (const r) x) [] updated in if null oks then dot else last oks
      return (newDot, ret <> extracted)
    run (dot, ret) (Err msg)    = return (dot, ret <> [Err msg])
    run (dot, ret) (Halt label) = return (dot, ret <> [Halt label])

runFuncDef :: Text -> Seq FuncParam -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
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

runFuncCall :: Text -> Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
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

    insertArgInState (VarParam    param) arg s@FilterRunState { fr_vars  } = mapMRet (\argVal -> retOk $ s { fr_vars = Map.insert param argVal fr_vars }) =<< runFilterNoPath arg json
    insertArgInState (FilterParam param) arg s@FilterRunState { fr_funcs } = resultOk $ s { fr_funcs = Map.insert (param, 0) (buildFilterFunc s Seq.empty arg) fr_funcs }

runLabel :: Text -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runLabel label next json = do
  ogState <- filterRunGetState
  filterRunAddLabel label
  haltedRet <- runFilter next json
  filterRunSetState ogState

  return $ foldrRet filterLabelHalts [] haltedRet
  where
    filterLabelHalts h@(Halt label') ret = if label == label' then ret else h : ret
    filterLabelHalts other ret = other : ret

runBreak :: Text -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runBreak label = do
  hasLabel <- filterRunExistsLabel label
  if hasLabel then resultHalt label else resultErr $ "$*label-" <> label <> " is not defined"

runLOC :: Integral a => Text -> a -> FilterRun (FilterResult Json)
runLOC file line = resultOk $ Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]

------------------------ Aux --------------------------

notImplemented :: Text -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
notImplemented msg = resultErr $ msg <> " filter not implemented"

invalidPathExpErr :: Json -> FilterRun (FilterRet a)
invalidPathExpErr json = retErr ("Invalid path expression with result " <> jsonShowError json)

notPathExp :: FilterRun (FilterResult Json) -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
notPathExp f = do
  pathStatus <- filterRunGetPathExp
  let pathEnabled = pathStatus == PathOn
  mapMRet (\json -> if pathEnabled then invalidPathExpErr json else retOk (json, Nothing)) =<< f

ifPathExp :: FilterRun a -> FilterRun a -> FilterRun a
ifPathExp then' else' = do
  pathStatus <- filterRunGetPathExp
  let doPath = pathStatus /= PathOff
  if doPath then then' else else'

runFilterNoPath :: Filter -> Json -> FilterRun (FilterResult Json)
runFilterNoPath filter json = do
  pathStatus <- filterRunGetPathExp
  if pathStatus /= PathOff
  then do
    filterRunSetPathExp PathOff
    ret <- runFilter filter json
    filterRunSetPathExp pathStatus
    return $ mapRet (Ok . fst) ret
  else do
    ret <- runFilter filter json
    return $ mapRet (Ok . fst) ret

runFilterTryPath :: Filter -> Json -> FilterRun (FilterResult (Json, Maybe (Seq Json)))
runFilterTryPath filter json = do
  pathStatus <- filterRunGetPathExp
  if pathStatus /= PathTry
  then do
    filterRunSetPathExp PathTry
    ret <- runFilter filter json
    filterRunSetPathExp pathStatus
    return ret
  else
    runFilter filter json

runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op left right json = concatMapMRet (\l -> mapMRet (op l) =<< runFilterNoPath right json) =<< runFilterNoPath left json

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
