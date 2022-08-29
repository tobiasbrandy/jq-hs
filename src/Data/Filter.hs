module Data.Filter (
  Filter (..)
, FuncParam (..)
, filterRun
) where

import Prelude hiding (exp, seq, any, filter, init)

import Data.Filter.Internal
  ( FilterRet (..)
  , retOk
  , retErr
  , applyRet
  , retToEither

  , FilterResult
  , foldrRet
  , resultOk
  , resultErr
  , mapMRet
  , concatMapMRet
  , mapRet
  , foldMRet

  , jsonShowError

  , concatMapM
  )

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
  | Label Text Filter
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
  in foldrRet ((:) . retToEither) [] ret

------------------------ Internal --------------------------

data FilterRunState = FilterRunState {
  fr_vars   :: HashMap Text Json,
  fr_funcs  :: HashMap (Text, Int) (Seq Filter -> Json -> FilterRun (FilterResult Json)),
  fr_file   :: FilterRunFile,
  fr_current_func :: (Text, Int, Seq Filter -> Json -> FilterRun (FilterResult Json))
}

filterRunInitState :: FilterRunState
filterRunInitState = FilterRunState {
  fr_vars         = Map.empty,
  fr_funcs        = Map.empty,
  fr_file         = TopLevel,
  fr_current_func = ("<top_level>", 0, error "No recursive function for top level")
}

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

filterRunIsTopLevel :: FilterRun Bool
filterRunIsTopLevel = FilterRun $ \s@FilterRunState { fr_file } -> (s, fr_file == TopLevel)

-- TODO(tobi): Si agregamos modulos, agregar en que file paso el error/no esta defninida la cosa (como jq)
runFilter :: Filter -> Json -> FilterRun (FilterResult Json)
-- Basic
runFilter Identity                  json  = resultOk json
runFilter Empty                     _     = return []
runFilter Recursive                 json  = runRecursive json
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
-- Arithmetic operators
runFilter (Neg    num)              json  = runUnary  runNeg    num         json
runFilter (Plus   left right)       json  = runBinary runPlus   left right  json
runFilter (Minus  left right)       json  = runBinary runMinus  left right  json
runFilter (Times  left right)       json  = runBinary runTimes  left right  json
runFilter (Div    left right)       json  = runBinary runDiv    left right  json
runFilter (Mod    left right)       json  = runBinary runMod    left right  json
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
runFilter (FuncCall name args)      json  = runFuncCall name args json
-- Label & break
runFilter (Label label body)        json  = notImplemented "Label"
runFilter (Break label)             json  = notImplemented "Break"
-- Special
runFilter (LOC file line)           _     = runLOC file line

-- Auxiliary functions --
notImplemented :: Text -> FilterRun (FilterResult Json)
notImplemented msg = resultErr $ msg <> " filter not implemented"

runUnary :: (Json -> FilterRun (FilterRet Json)) -> Filter -> Json -> FilterRun (FilterResult Json)
runUnary op exp json = mapMRet op =<< runFilter exp json

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

-- Filter operators implementations --
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

runRecursive :: Json -> FilterRun (FilterResult Json)
runRecursive json@(Object m)    = (Ok json :) <$> concatMapM runRecursive (Map.elems m)
runRecursive json@(Array items) = (Ok json :) <$> concatMapM runRecursive items
runRecursive json               = resultOk json

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
runProject anyl          anyr          = retErr ("Cannot index " <> jsonShowError anyl <> " with " <> jsonShowError anyr)

runSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun (FilterResult Json)
runSlice term left right json = let
    ts = runFilter term json
    ls = getIndeces (0::Int) left json
  in concatMapMRet (\t -> concatMapMRet (\l -> mapMRet (slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items)  = Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe (resultOk $ Number $ fromInteger $ toInteger def) (`runFilter` j) indexExp

    slice (Array items) (Number l)  (Number r)  = retOk $ Array $ seqSlice (cycleIndex (floor l)) (cycleIndex (ceiling r)) items
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Array _) anyl anyr = retErr (
      "Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr
      )
    slice Null  _ _ = retOk Null
    slice any   _ _ = retErr (jsonShowError any <> " cannot be sliced, only arrays")

    seqSlice l r = Seq.take (r-l) . Seq.drop l

runNeg :: Json -> FilterRun (FilterRet Json)
runNeg (Number n)  = retOk $ Number $ negate n
runNeg any         = retErr (jsonShowError any <> " cannot be negated")

runPlus :: Json -> Json -> FilterRun (FilterRet Json)
runPlus (Number l) (Number r)  = retOk $ Number  $ l +  r
runPlus (Array l)  (Array  r)  = retOk $ Array   $ l <> r
runPlus (String l) (String r)  = retOk $ String  $ l <> r
runPlus (Object l) (Object r)  = retOk $ Object  $ r <> l -- On collisions conserves the entry from the right
runPlus Null       any         = retOk   any
runPlus any        Null        = retOk   any
runPlus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be added")

runMinus :: Json -> Json -> FilterRun (FilterRet Json)
runMinus (Number l) (Number r)  = retOk $ Number  $ l - r
runMinus (Array l)  (Array  r)  = retOk $ Array   $ Seq.filter (`notElem` r) l
runMinus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

runTimes :: Json -> Json -> FilterRun (FilterRet Json)
runTimes (Number l)   (Number r)    = retOk $ Number  $ l * r
runTimes (String l)   (Number r)    = retOk $ if r > 0 then String $ T.replicate (floor r) l else Null
runTimes l@(Object _) r@(Object  _) = retOk $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
runTimes l            r             = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

runDiv :: Json -> Json -> FilterRun (FilterRet Json)
runDiv jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = retOk $ Number $ l / r
runDiv (String l) (String r) = retOk $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
runDiv l          r          = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

runMod :: Json -> Json -> FilterRun (FilterRet Json)
runMod jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = retOk $ Number $ fromInteger $ truncate l `mod` truncate (abs r)
runMod l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

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
  ret     <- mapM (\base -> foldMRet run base stream) init -- TODO(tobi): Sacarle el '
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

runFunc :: FilterRunState -> Seq FuncParam -> Filter -> Seq Filter -> Json -> FilterRun (FilterResult Json)
runFunc state params body args json = do
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
    insertArgInState (FilterParam param) arg s@FilterRunState { fr_funcs } = resultOk $ s { fr_funcs = Map.insert (param, 0) (runFunc s Seq.empty arg) fr_funcs }

runLOC :: Integral a => Text -> a -> FilterRun (FilterResult Json)
runLOC file line = resultOk $ Object $ Map.fromList [("file", String file), ("line", Number $ fromIntegral line)]
