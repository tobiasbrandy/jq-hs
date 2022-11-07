module Data.Filter.Internal.Run
-- Public
( filterRunExp
, filterRunModule

-- Defs
, FilterFunc
, PathExp

-- Main driver
, runFilter

-- FilterRun monad and state
, FilterRun
, FilterRunFile (..)
, PathExpStatus (..)
, filterRunSetPathExp
, filterRunGetPathExp

-- Implementations
, project
, slice

-- Path Utils
, invalidPathExpErr
, notPathExp
, ifPathExp
, runFilterNoPath

-- Json Utils
, jsonBool
, jsonShowBounded
, jsonShowError
, jsonShowError'

-- Misc Utils
, cycleIdx

) where

import Prelude hiding (exp, seq, any, filter, init, tail)

import Data.Filter (Filter (..), FuncParam (..))

import Data.Filter.Internal.Result
  ( FilterRet (Ok, Err)
  , retOk
  , retErr
  , applyByOk

  , FilterResult
  , resultOk
  , mapRet
  , resultErr
  , filterRet
  , foldMRet
  , mapMRet
  , concatMapMRet

  , concatMapM
  )
import qualified Data.Filter.Internal.Result as Ret (FilterRet (..))

import Data.Filter.Internal.Sci (sciFloor, sciCeiling, IntNum, intNumToInt, sciTruncate)

import Data.Json (Json (..), JsonNum (..), jsonShowType)
import Data.Json.Encode (jsonEncode, compactFormat)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Sequence (Seq ((:|>), (:<|)))
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set
import Data.Foldable (foldl', toList)
import Data.Scientific (Scientific, isInteger)
import Data.Maybe (fromMaybe)
import Control.Monad (liftM, ap, when, foldM)
import qualified Data.ByteString.Lazy as BS

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

-- Similar in concept to a json pointer (https://datatracker.ietf.org/doc/html/rfc6901) encoded as an array
type PathExp = Seq Json

type FilterFunc = Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))

data FilterRunCtx = FilterRunCtx
  { fr_vars         :: HashMap Text Json
  , fr_funcs        :: HashMap (Text, Int) FilterFunc
  , fr_labels       :: HashSet Text
  , fr_file         :: FilterRunFile
  , fr_current_func :: (Text, Int, FilterFunc)
  }

data FilterRunState = FilterRunState
  { fr_ctx      :: FilterRunCtx
  , fr_path_exp :: PathExpStatus
  }

filterRunInitState :: FilterRunFile -> HashMap (Text, Int) FilterFunc -> FilterRunState
filterRunInitState file builtins = FilterRunState
  { fr_ctx = FilterRunCtx
    { fr_vars         = Map.empty
    , fr_funcs        = builtins
    , fr_labels       = Set.empty
    , fr_file         = file
    , fr_current_func = ("<top_level>", 0, error "No recursive function for top level")
    }
  , fr_path_exp = PathOff
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

filterRunSetCtx :: FilterRunCtx -> FilterRun ()
filterRunSetCtx ctx = FilterRun $ \s -> (s { fr_ctx = ctx }, ())

filterRunGetCtx :: FilterRun FilterRunCtx
filterRunGetCtx = FilterRun $ \s@FilterRunState { fr_ctx } -> (s, fr_ctx)

filterRunVarInsert :: Text -> Json -> FilterRun ()
filterRunVarInsert name body = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_vars } } ->
  (s { fr_ctx = (fr_ctx s) { fr_vars = Map.insert name body fr_vars } }, ())

filterRunVarGet :: Text -> FilterRun (Maybe Json)
filterRunVarGet name = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_vars } } -> (s, Map.lookup name fr_vars)

filterRunFuncInsert :: Text -> Int -> (Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))) -> FilterRun ()
filterRunFuncInsert name argc f = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_funcs } } ->
  (s { fr_ctx = (fr_ctx s) { fr_funcs = Map.insert (name, argc) f fr_funcs } }, ())

filterRunFuncGet :: Text -> Int -> FilterRun (Maybe (Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))))
filterRunFuncGet name argc = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_funcs, fr_current_func = (c_name, c_argc, c_f) } } ->
  case Map.lookup (name, argc) fr_funcs of
    Nothing ->
      if name == c_name && argc == c_argc
      then (s, Just c_f)
      else (s, Nothing)
    f -> (s, f)

filterRunAddLabel :: Text -> FilterRun ()
filterRunAddLabel label = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_labels } } ->
  (s { fr_ctx = (fr_ctx s) { fr_labels = Set.insert label fr_labels } }, ())

filterRunExistsLabel :: Text -> FilterRun Bool
filterRunExistsLabel label = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_labels } } -> (s, Set.member label fr_labels)

filterRunSetPathExp :: PathExpStatus -> FilterRun ()
filterRunSetPathExp pathExp = FilterRun $ \s -> (s { fr_path_exp = pathExp }, ())

filterRunGetPathExp :: FilterRun PathExpStatus
filterRunGetPathExp = FilterRun $ \s@FilterRunState { fr_path_exp } -> (s, fr_path_exp)

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_ctx = FilterRunCtx { fr_file } } -> (s, fr_file == TopLevel)

------------------------ Run --------------------------

filterRunExp :: HashMap (Text, Int) FilterFunc -> Filter -> Json -> FilterResult Json
filterRunExp funcs filter json = let
    (FilterRun f) = runFilter filter json
    (_, ret)      = f $ filterRunInitState TopLevel funcs
  in map (fmap fst) ret

filterRunModule :: Text -> HashMap (Text, Int) FilterFunc -> Filter -> HashMap (Text, Int) FilterFunc
filterRunModule moduleName funcs filter = let
    (FilterRun f) = runFilter filter Null
    (FilterRunState { fr_ctx = FilterRunCtx { fr_funcs } }, _) = f $ filterRunInitState (Module moduleName) funcs
  in fr_funcs

runFilter :: Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
-- Basic
runFilter Identity                  json  = ifPathExp (resultOk (json, Just Seq.empty)) (resultOk (json, Nothing))
runFilter Empty                     _     = return []
runFilter (Json json)               _     = runJson json
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
runFilter (Alt left right)          json  = runAlt left right  json
runFilter (TryCatch try catch)      json  = runTryCatch try catch json
runFilter (Comma left right)        json  = concatMapMRet (`runFilter` json) [Ok left, Ok right]
runFilter (IfElse if' then' else')  json  = runIfElse if' then' else' json
-- Reductions
runFilter (Reduce  exp name initial update)         json  = runReduce   exp name initial update         json
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

runJson :: Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runJson Null = ifPathExp (resultOk (Null, Just Seq.empty)) (resultOk (Null, Nothing))
runJson json = notPathExp $ resultOk json

runVar :: Text -> FilterRun (FilterResult Json)
runVar name = maybe (resultErr $ "$" <> name <> " is not defined") resultOk =<< filterRunVarGet name

runVarDef :: Text -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runVarDef name body next json = do
  bodies <- runFilterNoPath body json
  ogCtx <- filterRunGetCtx
  ret <- concatMapMRet run bodies
  filterRunSetCtx ogCtx
  return ret
  where
    run body' = do
      filterRunVarInsert name body'
      runFilter next json

runArrayLit :: Filter -> Json -> FilterRun (FilterResult Json)
runArrayLit items json = (:[]) . fmap (Array . foldl' (:|>) Seq.empty) . sequence . take maxBound <$> runFilterNoPath items json

runObjectLit :: Seq (Filter, Filter) -> Json -> FilterRun (FilterResult Json)
runObjectLit entries json = mapRet (Ok . Object) <$> foldr entryCrossMaps (resultOk Map.empty) entries
  where
    entryCrossMaps (key, val) maps = let
        ks = runFilterNoPath key json
        vs = runFilterNoPath val json
      in concatMapMRet (\k -> concatMapMRet (\v -> mapMRet (insertKvInMap k v) =<< maps) =<< vs) =<< ks

    insertKvInMap (String key)  val m  = retOk $ Map.insert key val m
    insertKvInMap any           _   _  = retErr ("Cannot use " <> jsonShowError any <> " as object key")

runIter :: Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runIter (Array items)     = ifPathExp
  (return $ toList $ Seq.mapWithIndex (\idx item -> Ok (item, Just $ Seq.singleton $ Number $ fromIntegral idx)) items)
  (return $ map (Ok . (, Nothing)) $ toList items)
runIter (Object entries)  = ifPathExp
  (return $ map (\(k, v) -> Ok (v, Just $ Seq.singleton $ String k)) $ Map.toList entries)
  (return $ map (Ok . (, Nothing)) $ Map.elems entries)
runIter any               = resultErr ("Cannot iterate over " <> jsonShowError any)

runProject :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runProject term exp json = concatMapMRet (\l -> mapMRet (return . project l) =<< runFilterNoPath exp json) =<< runFilter term json

project :: (Json, Maybe PathExp) -> Json -> FilterRet (Json, Maybe PathExp)
project (Object m,    lp) r@(String key)      = let p = (:|> r) <$> lp in Ok $ maybe (Null, p) (, p) $ Map.lookup key m
project (Array items, lp) r@(Number (Num n))  = let p = (:|> r) <$> lp in Ok $
  if isInteger n
  then (, p) $ fromMaybe Null $ Seq.lookup (intNumToInt $ cycleIdx (fromIntegral $ Seq.length items) $ sciTruncate n) items
  else (Null, p)
project (Array _,     lp) r@(Number _nan)        = Ok (Null, (:|> r) <$> lp)
project (Array haystack, lp) r@(Array needle) = let
    p   = (:|> r) <$> lp
    hayLen = Seq.length haystack
  in Ok $ (, p) $ Array $
    if null needle
    then Seq.empty
    else fmap (Number . fromIntegral . (hayLen -) . Seq.length) $ Seq.filter (seqIsPrefixOf needle) $ Seq.tails haystack
  where
    seqIsPrefixOf Seq.Empty _           =  True
    seqIsPrefixOf _         Seq.Empty   =  False
    seqIsPrefixOf (x :<| xs) (y :<| ys) =  x == y && seqIsPrefixOf xs ys
project (Null, lp)        r@(String _)  = Ok (Null, (:|> r) <$> lp)
project (Null, lp)        r@(Number _)  = Ok (Null, (:|> r) <$> lp)
project (anyl,_)          anyr          = Err ("Cannot index " <> jsonShowType anyl <> " with " <> jsonShowError' anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runSlice term left right json = let
    ts = runFilter term json
    ls = getIndeces (0::Int) left json
  in concatMapMRet (\t -> concatMapMRet (\l -> mapMRet (return . slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items,_)  = Seq.length items
    itemsLen (String s,_)     = T.length s
    itemsLen _                = 0

    getIndeces def indexExp j = maybe (resultOk $ Number $ fromIntegral def) (`runFilterNoPath` j) indexExp

slice :: (Json, Maybe PathExp) -> Json -> Json -> FilterRet (Json, Maybe PathExp)
slice (Array items, pl) (Number (Num l)) (Number (Num r))  = let
    len       = fromIntegral $ Seq.length items
    start     = sciFloor l
    end       = sciCeiling r
    sliced    = Array $ seqSlice (cycleIdx len $ fromIntegral start) (cycleIdx len $ fromIntegral end) items
  in Ok $ (sliced,) $ (:|> slicePath (fromIntegral start) (fromIntegral end)) <$> pl
  where
    seqSlice start end = let
        l' = intNumToInt start
        r' = intNumToInt end
      in Seq.take (r'-l') . Seq.drop l'
slice (Array _, pl) l@(Number _nan) r@(Number _nan') = slice (Null, pl) l r
slice (String s, pl) (Number (Num l)) (Number (Num r)) = let
    len       = fromIntegral $ T.length s
    start     = sciFloor l
    end       = sciCeiling r
    sliced    = String $ textSlice (cycleIdx len $ fromIntegral start) (cycleIdx len $ fromIntegral end) s
  in Ok $ (sliced,) $ (:|> slicePath (fromIntegral start) (fromIntegral end)) <$> pl
  where
    textSlice start end = let
        l' = intNumToInt start
        r' = intNumToInt end
      in T.take (r'-l') . T.drop l'
slice (String _, pl) l@(Number _nan) r@(Number _nan') = slice (Null, pl) l r
slice (Null, pl) (Number l) (Number r) = Ok (Null, (:|> slicePath (fromIntegral . sciFloor <$> l) (fromIntegral . sciCeiling <$> r)) <$> pl)
slice (Array _,_)   anyl  anyr  = sliceError anyl anyr
slice (Null,_)      anyl  anyr  = sliceError anyl anyr
slice (any,_)       _     _     = Err (jsonShowError any <> " cannot be sliced, only arrays or null")

slicePath :: JsonNum Scientific -> JsonNum Scientific -> Json
slicePath start end = Object $ Map.fromList [("start", Number start), ("end", Number end)]

sliceError :: Json -> Json -> FilterRet a
sliceError anyl anyr = Err ("Start and end indices of an array slice must be numbers, not " <> jsonShowType anyl <> " and " <> jsonShowType anyr)

runAlt :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runAlt left right json = do
  leftRet       <- runFilterTryPath left json
  let validLeft = filterRet (jsonBool . fst) leftRet
  if null validLeft
  then runFilter right json
  else return validLeft

runTryCatch :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runTryCatch try catch json = concatMapM errorToCatched =<< runFilter try json
  where
    errorToCatched (Err msg)  = runFilter catch $ String msg
    errorToCatched other      = return [other]

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runIfElse if' then' else' json = concatMapMRet eval =<< runFilterNoPath if' json
  where eval cond = runFilter (if jsonBool cond then then' else else') json

runReduce :: Filter -> Text -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runReduce exp name initial update json = do
  stream  <- runFilterNoPath exp json
  init    <- runFilter initial json
  ogCtx   <- filterRunGetCtx
  ret     <- mapM (\base -> foldMRet run base stream) init
  filterRunSetCtx ogCtx
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
runForeach :: Filter -> Text -> Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runForeach exp name initial update extract json = do
  stream  <- runFilterNoPath exp json
  init    <- runFilter initial json
  ogCtx <- filterRunGetCtx
  ret <- concatMapMRet (\base -> snd <$> foldM (\d@(dot, ret) -> applyByOk (run d) (return . (dot,) . (ret <>) . (:[]))) (base, []) stream) init
  filterRunSetCtx ogCtx
  return ret
  where
    run (dot@(jdot, pdot), ret) val = do
      filterRunVarInsert name val
      updated   <- mapMRet (\(ju, pu) -> retOk (ju, pdot <> pu)) =<< runFilter update jdot
      extracted <- concatMapMRet (\(ju, pu) -> mapMRet (\(je, pe) -> retOk (je, pu <> pe)) =<< runFilter extract ju) updated
      let newDot = let oks = foldr (\x r -> applyByOk (:r) (const r) x) [] updated in if null oks then dot else last oks
      return (newDot, ret <> extracted)

runFuncDef :: Text -> Seq FuncParam -> Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runFuncDef name params body next json = do
  let argc = Seq.length params
  -- Conseguimos el ctx de la funcion, es decir, el current ctx con el current_func actualizado
  ogCtx <- filterRunGetCtx
  let ctx = ogCtx { fr_current_func = (name, argc, buildFilterFunc ctx params body) }
  -- Insertamos la funcion en el ctx
  filterRunFuncInsert name argc $ buildFilterFunc ctx params body
  -- Ejecutamos next con la funcion ya declarada
  ret <- runFilter next json
  -- Al retornar, solo dejamos la funcion declarada si estamos en un modulo, sino restauramos el ctx original
  isTopLevel <- filterRunIsTopLevel
  when isTopLevel $
    filterRunSetCtx ogCtx
  return ret

runFuncCall :: Text -> Seq Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runFuncCall name args json = do
  -- Buscar la funcion en el ctx
  mFunc <- filterRunFuncGet name $ Seq.length args
  -- Ejecutar la funcion
  case mFunc of
    Nothing -> resultErr $ name <> "/" <> T.pack (show $ Seq.length args) <> " is not defined"
    Just f  -> f args json

buildFilterFunc :: FilterRunCtx -> Seq FuncParam -> Filter -> FilterFunc
buildFilterFunc context params body args json = do
  -- Guardar el ctx original
  ogCtx <- filterRunGetCtx
  -- Mapear args con params (cross product) y agregarlos al ctx guardado
  -- Usamos foldl' porque queremos que el procesamiento sea de izquierda a derecha
  contexts <- foldl' (argCrossCtx ogCtx) (resultOk context) (Seq.zip params args)
  -- Ejecutar el body con cada ctx calculado
  ret <- concatMapMRet runBodyWithCtx contexts
  -- Restaurar el ctx original
  filterRunSetCtx ogCtx
  return ret
  where
    runBodyWithCtx ctx = do
      filterRunSetCtx ctx
      runFilter body json

    argCrossCtx ogCtx ctxs (param, arg) = concatMapMRet (insertArgInCtx ogCtx param arg) =<< ctxs

    insertArgInCtx _ (VarParam param) arg ctx@FilterRunCtx { fr_vars } = mapMRet (\argVal -> retOk $ ctx { fr_vars = Map.insert param argVal fr_vars }) =<< runFilterNoPath arg json
    insertArgInCtx ogCtx (FilterParam param) arg ctx@FilterRunCtx { fr_funcs } = resultOk $ ctx { fr_funcs = Map.insert (param, 0) (buildFilterFunc ogCtx Seq.empty arg) fr_funcs }

runLabel :: Text -> Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
runLabel label next json = do
  ogCtx <- filterRunGetCtx
  filterRunAddLabel label
  haltedRet <- runFilter next json
  filterRunSetCtx ogCtx

  return $ foldr filterLabelHalts [] haltedRet
  where
    filterLabelHalts h@(Ret.Break label') ret = if label == label' then ret else h : ret
    filterLabelHalts other ret = other : ret

runBreak :: Text -> FilterRun (FilterResult (Json, Maybe PathExp))
runBreak label = do
  hasLabel <- filterRunExistsLabel label
  if hasLabel then return [Ret.Break label] else resultErr $ "$*label-" <> label <> " is not defined"

runLOC :: Integral a => Text -> a -> FilterRun (FilterResult Json)
runLOC file line = resultOk $ Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]

------------------------ Path Exp Utils --------------------------

invalidPathExpErr :: Json -> FilterRun (FilterRet a)
invalidPathExpErr json = retErr ("Invalid path expression with result " <> jsonShowBounded json)

notPathExp :: FilterRun (FilterResult Json) -> FilterRun (FilterResult (Json, Maybe PathExp))
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

runFilterTryPath :: Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
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

------------------------ Json Utils --------------------------
jsonBool :: Json -> Bool
jsonBool Null         = False
jsonBool (Bool False) = False
jsonBool _            = True

jsonShowBounded :: Json -> Text
jsonShowBounded json = let
    maxSize = 11
    jt = decodeUtf8With lenientDecode $ BS.toStrict $ jsonEncode compactFormat json
  in if T.length jt > maxSize
    then T.take maxSize jt <> "..."
    else jt

jsonShowError :: Json -> Text
jsonShowError json = jsonShowType json <> " (" <> jsonShowBounded json <> ")"

jsonShowError' :: Json -> Text
jsonShowError' json = jsonShowType json <> " " <> jsonShowBounded json

------------------------ Misc Utils --------------------------
cycleIdx :: IntNum -> IntNum -> IntNum
cycleIdx len i = if i < 0 then len + i else i
