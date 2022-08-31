{-# LANGUAGE TemplateHaskell #-}
module Data.Filter.Builtins (builtins) where

import Prelude hiding (filter, any, exp)

import Data.Filter.Internal.Run
  ( filterRunModule

  , FilterRun
  , runFilter
  , FilterFunc

  , jsonBool

  , jsonShowError
  )

import Data.Filter.Internal.Result
  (FilterRet (..)
  , retOk
  , retErr

  , FilterResult
  , resultOk
  , resultErr
  , mapMRet
  , concatMapMRet
  )

import Data.Filter (Filter (..))

import Data.Json (Json (..), jsonShowType)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import Data.FileEmbed (embedFile)
import Lib (parseFilter)
import Parse.Defs (parserStateInit)
import Data.Foldable (foldl', toList)
import TextShow (showt)
import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger, isInteger)

builtins :: HashMap (Text, Int) FilterFunc
builtins = case parseFilter $ parserStateInit $ BS.fromStrict $(embedFile "src/Data/Filter/builtins.jq") of
  Left msg      -> error $ "Fatal - builtins.jq parsing failed: " <> show msg
  Right filter  -> let
      ret = filterRunModule "builtins.jq" hsBuiltins filter
    -- Agregamos la funcion builtins/0
      builtins0Sig = ("builtins", 0)
    in Map.insert builtins0Sig (nullary $ builtins0 $ Map.keys ret <> [builtins0Sig]) ret

hsBuiltins :: HashMap (Text, Int) FilterFunc
hsBuiltins = Map.fromList
  [ (("empty",      0),   nullary   (return []))
  , (("not",        0),   nullary'  (resultOk . Bool . not . jsonBool))
  , (("path",       1),   func1     path)
  -- , (("range",   1),   binary    plus)
  , (("_plus",      2),   binary    plus)
  , (("_negate",    1),   unary     neg)
  , (("_minus",     2),   binary    minus)
  , (("_multiply",  2),   binary    multiply)
  , (("_divide",    2),   binary    divide)
  , (("_mod",       2),   binary    modulus)
  --  {(cfunction_ptr)f_dump, "tojson", 1},
  -- {(cfunction_ptr)f_json_parse, "fromjson", 1},
  -- {(cfunction_ptr)f_tonumber, "tonumber", 1},
  -- {(cfunction_ptr)f_tostring, "tostring", 1},
  -- {(cfunction_ptr)f_keys, "keys", 1},
  -- {(cfunction_ptr)f_keys_unsorted, "keys_unsorted", 1},
  -- {(cfunction_ptr)f_startswith, "startswith", 2},
  -- {(cfunction_ptr)f_endswith, "endswith", 2},
  -- {(cfunction_ptr)f_ltrimstr, "ltrimstr", 2},
  -- {(cfunction_ptr)f_rtrimstr, "rtrimstr", 2},
  -- {(cfunction_ptr)f_string_split, "split", 2},
  -- {(cfunction_ptr)f_string_explode, "explode", 1},
  -- {(cfunction_ptr)f_string_implode, "implode", 1},
  --  {(cfunction_ptr)f_string_indexes, "_strindices", 2},
  -- {(cfunction_ptr)f_setpath, "setpath", 3}, // FIXME typechecking
  -- {(cfunction_ptr)f_getpath, "getpath", 2},
  -- {(cfunction_ptr)f_delpaths, "delpaths", 2},
  , (("has",        1),   unary'    has)
  , (("_equal",     2),   comp      (==))
  , (("_notequal",  2),   comp      (/=))
  , (("_less",      2),   comp      (<))
  , (("_greater",   2),   comp      (>))
  , (("_lesseq",    2),   comp      (<=))
  , (("_greatereq", 2),   comp      (>=))
  --  {(cfunction_ptr)f_contains, "contains", 2},
  -- {(cfunction_ptr)f_length, "length", 1},
  -- {(cfunction_ptr)f_utf8bytelength, "utf8bytelength", 1},
  -- {(cfunction_ptr)f_type, "type", 1},
  -- {(cfunction_ptr)f_isinfinite, "isinfinite", 1},
  -- {(cfunction_ptr)f_isnan, "isnan", 1},
  -- {(cfunction_ptr)f_isnormal, "isnormal", 1},
  -- {(cfunction_ptr)f_infinite, "infinite", 1},
  -- {(cfunction_ptr)f_nan, "nan", 1},
  -- {(cfunction_ptr)f_sort, "sort", 1},
  -- {(cfunction_ptr)f_sort_by_impl, "_sort_by_impl", 2},
  -- {(cfunction_ptr)f_group_by_impl, "_group_by_impl", 2},
  -- {(cfunction_ptr)f_min, "min", 1},
  -- {(cfunction_ptr)f_max, "max", 1},
  -- {(cfunction_ptr)f_min_by_impl, "_min_by_impl", 2},
  -- {(cfunction_ptr)f_max_by_impl, "_max_by_impl", 2},
  , (("error",      0),   nullary'  error0)
  -- {(cfunction_ptr)f_format, "format", 2},
  -- {(cfunction_ptr)f_env, "env", 1},
  -- {(cfunction_ptr)f_halt, "halt", 1},
  -- {(cfunction_ptr)f_halt_error, "halt_error", 2},
  -- {(cfunction_ptr)f_get_search_list, "get_search_list", 1},
  -- {(cfunction_ptr)f_get_prog_origin, "get_prog_origin", 1},
  -- {(cfunction_ptr)f_get_jq_origin, "get_jq_origin", 1},
  -- {(cfunction_ptr)f_match, "_match_impl", 4},
  -- {(cfunction_ptr)f_modulemeta, "modulemeta", 1},
  -- {(cfunction_ptr)f_input, "input", 1},
  -- {(cfunction_ptr)f_debug, "debug", 1},
  -- {(cfunction_ptr)f_stderr, "stderr", 1},
  -- {(cfunction_ptr)f_strptime, "strptime", 2},
  -- {(cfunction_ptr)f_strftime, "strftime", 2},
  -- {(cfunction_ptr)f_strflocaltime, "strflocaltime", 2},
  -- {(cfunction_ptr)f_mktime, "mktime", 1},
  -- {(cfunction_ptr)f_gmtime, "gmtime", 1},
  -- {(cfunction_ptr)f_localtime, "localtime", 1},
  -- {(cfunction_ptr)f_now, "now", 1},
  -- {(cfunction_ptr)f_current_filename, "input_filename", 1},
  -- {(cfunction_ptr)f_current_line, "input_line_number", 1},
  ]

------------------------ Function Declaration Utils --------------------------

func2 :: (Filter -> Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func2 f (a :<| b :<| Seq.Empty) json = f a b json
func2 _ _ _ = error "Binary functions only allow 2 params"

func1 :: (Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func1 f (a :<| Seq.Empty) json = f a json
func1 _ _ _ = error "Unary functions only allow 1 param"

func0 :: (Json -> FilterRun (FilterResult Json)) -> FilterFunc
func0 f Seq.Empty json = f json
func0 _ _ _ = error "Nullary functions don't have params"

binary :: (Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
binary f = func2 $ runBinary f

binary' :: (Json -> Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
binary' f params json = binary (\a b -> f a b json) params json

unary :: (Json -> FilterRun (FilterRet Json)) -> FilterFunc
unary f = func1 $ runUnary f

unary' :: (Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
unary' f params json = unary (`f` json) params json

nullary :: FilterRun (FilterResult Json) -> FilterFunc
nullary f = func0 $ const f

nullary' :: (Json -> FilterRun (FilterResult Json)) -> FilterFunc
nullary' f params json = nullary (f json) params json

comp :: (Json -> Json -> Bool) -> Seq Filter -> Json -> FilterRun (FilterResult Json)
comp op = binary (\l -> retOk . Bool . op l)

runUnary :: (Json -> FilterRun (FilterRet Json)) -> Filter -> Json -> FilterRun (FilterResult Json)
runUnary op exp json = mapMRet op =<< runFilter exp json

runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op left right json = concatMapMRet (\l -> mapMRet (op l) =<< runFilter right json) =<< runFilter left json

------------------------ Builtins --------------------------

path :: Filter -> Json -> FilterRun (FilterResult Json)
path filter json = mapMRet (retOk . Array . snd) =<< runPath filter json

plus :: Json -> Json -> FilterRun (FilterRet Json)
plus (Number l) (Number r)  = retOk $ Number  $ l +  r
plus (Array l)  (Array  r)  = retOk $ Array   $ l <> r
plus (String l) (String r)  = retOk $ String  $ l <> r
plus (Object l) (Object r)  = retOk $ Object  $ r <> l -- On collisions conserves the entry from the right
plus Null       any         = retOk   any
plus any        Null        = retOk   any
plus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be added")

neg :: Json -> FilterRun (FilterRet Json)
neg (Number n)  = retOk $ Number $ negate n
neg any         = retErr (jsonShowError any <> " cannot be negated")

minus :: Json -> Json -> FilterRun (FilterRet Json)
minus (Number l) (Number r)  = retOk $ Number  $ l - r
minus (Array l)  (Array  r)  = retOk $ Array   $ Seq.filter (`notElem` r) l
minus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

multiply :: Json -> Json -> FilterRun (FilterRet Json)
multiply (Number l)   (Number r)    = retOk $ Number  $ l * r
multiply (String l)   (Number r)    = retOk $ if r > 0 then String $ T.replicate (floor r) l else Null
multiply l@(Object _) r@(Object  _) = retOk $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
multiply l            r             = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

divide :: Json -> Json -> FilterRun (FilterRet Json)
divide jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = retOk $ Number $ l / r
divide (String l) (String r) = retOk $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
divide l          r          = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

modulus :: Json -> Json -> FilterRun (FilterRet Json)
modulus jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = retOk $ Number $ fromInteger $ truncate l `mod` truncate (abs r)
modulus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

has :: Json -> Json -> FilterRun (FilterRet Json)
has (String key)  (Object m)    = retOk $ Bool $ Map.member key m
has (Number n)    (Array items) = let n' = truncate n in retOk $ Bool $ n' >= 0 && n' < Seq.length items
has json key = retErr $ "Cannot check whether " <> jsonShowType json <> " has a " <> jsonShowType key <> " key"

error0 :: Json -> FilterRun (FilterResult Json)
error0 (String msg)  = resultErr msg
error0 _             = resultErr "(not a string)"

builtins0 :: [(Text, Int)] -> FilterRun (FilterResult Json)
builtins0 = resultOk . Array . foldl' sigToNames Seq.empty
  where
    sigToNames ret (name, argc) =
      if T.null name || T.head name == '_'
      then ret
      else ret :|> String (name <> "/" <> showt argc)

------------------------ Path Builtin Internals --------------------------

runPath :: Filter -> Json -> FilterRun (FilterResult (Json, Seq Json))
runPath Identity  json     = resultOk (json, Seq.empty)
runPath Empty     _     = return []
runPath Recursive json  = return $ map (Ok . (json,)) $ pathRecursive json
-- TODO(tobi): runPath (Json Null) Null = [] vale???
-- -- Variable
-- runFilter (Var name)                _     = runVar name
-- runFilter (VarDef name body next)   json  = runVarDef name body next json
-- -- Projections
runPath (Project term exp)        json  = pathProject term exp json
runPath (Slice term left right)   json  = pathSlice term left right json
runPath Iter                      json  = pathIter json
-- -- Flow operators
runPath (Pipe left right)         json  = concatMapMRet (\(jl, pl) -> mapMRet (\(jr, pr) -> retOk (jr, pl <> pr)) =<< runPath right jl) =<< runPath left json
-- runFilter (Alt left right)          json  = runBinary runAlt    left right  json
-- runFilter (TryCatch try catch)      json  = runTryCatch         try  catch  json
runPath (Comma left right)        json  = liftM2 (<>) (runPath left json) (runPath right json)
-- runPath (IfElse if' then' else')  json  = pathIfElse if' then' else' json
-- -- Assignment operators
-- runFilter (Assign left right)       json  = notImplemented "Assign"
-- runFilter (Update left right)       json  = notImplemented "Update"
-- -- Comparison operators
-- runFilter (Or   left right)         json  = runBoolComp   (||)  left right  json
-- runFilter (And  left right)         json  = runBoolComp   (&&)  left right  json
-- -- Reductions
-- runFilter (Reduce exp name initial update)          json  = runReduce   exp name initial update         json
-- runFilter (Foreach exp name initial update extract) json  = runForeach  exp name initial update extract json
-- -- Functions
-- runFilter (FuncDef name params body next) json = runFuncDef name params body next json
-- runFilter (FuncCall name args)      json  = runFuncCall name args json
-- -- Label & break
-- runFilter (Label label next)        json  = runLabel label next json
-- runFilter (Break label)             _     = runBreak label
-- -- Special
-- runFilter (LOC file line)           _     = runLOC file line
runPath _ _ = undefined

pathRecursive :: Json -> [Seq Json]
pathRecursive (Object m)    = (Seq.empty :) $ concatMap (\(k, v) -> map (\xs -> String k :<| xs) $ pathRecursive v) (Map.toList m)
pathRecursive (Array items) = (Seq.empty :) $ concatMap (\(idx, item) -> map (\xs -> Number (fromInteger idx) :<| xs) $ pathRecursive item) (zip [0..] $ toList items)
pathRecursive _             = [Seq.empty]

pathProject :: Filter -> Filter -> Json -> FilterRun (FilterResult (Json, Seq Json))
pathProject term exp json = concatMapMRet (\l -> mapMRet (project l) =<< runFilter exp json) =<< runPath term json
  where
    project :: (Json, Seq Json) -> Json -> FilterRun (FilterRet (Json, Seq Json))
    project (Object m, lp)    r@(String key)  = let p = lp :|> r in retOk $ maybe (Null, p) (, p) $ Map.lookup key m
    project (Array items, lp) r@(Number n)    = let p = lp :|> r in retOk $
      if isInteger n
      then case toBoundedInteger n of
        Nothing -> (Null, p)
        Just i  -> (, p) $ fromMaybe Null $ Seq.lookup (if i < 0 then length items + i else i) items
      else (Null, p)
    project (Null, lp)        r@(String _)  = retOk (Null, lp :|> r)
    project (Null, lp)        r@(Number _)  = retOk (Null, lp :|> r)
    project (anyl,_)          anyr          = retErr ("Cannot index " <> jsonShowType anyl <> " with " <> jsonShowError anyr)

pathSlice :: Filter -> Maybe Filter -> Maybe Filter -> Json -> FilterRun (FilterResult (Json, Seq Json))
pathSlice term left right json = let
    ts = runPath term json
    ls = getIndeces 0 left json
  in concatMapMRet (\t -> concatMapMRet (\l -> mapMRet (slice t l) =<< getIndeces (itemsLen t) right json) =<< ls) =<< ts
  where
    itemsLen (Array items,_)  = toInteger $ Seq.length items
    itemsLen _              = 0

    getIndeces def indexExp j = maybe (resultOk $ Number $ fromInteger def) (`runFilter` j) indexExp

    slice (Array items, pl) (Number l)  (Number r)  = let
          start :: Int
          start   = floor l
          end :: Int
          end     = ceiling r
          sliced  = Array $ seqSlice (cycleIndex start) (cycleIndex end) items
        in retOk $ (sliced,) $ pl :|> Object (Map.fromList [("start", toNum $ toInteger start), ("end", Number $ fromInteger $ toInteger end)])
      where
        len = Seq.length items
        cycleIndex i = if i < 0 then len + i else i
    slice (Null, pl)  (Number l)  (Number r) = retOk (Null, pl :|> Object (Map.fromList [("start", toNum $ floor l), ("end", toNum $ ceiling r)]))

    slice (Array _,_)   anyl  anyr  = sliceError anyl anyr
    slice (Null,_)      anyl  anyr  = sliceError anyl anyr
    slice (any,_)       _     _     = retErr (jsonShowError any <> " cannot be sliced, only arrays or null")

    toNum = Number . fromInteger

    seqSlice l r = Seq.take (r-l) . Seq.drop l

    sliceError anyl anyr = retErr ("Start and end indices of an array slice must be numbers, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr)

pathIter :: Json -> FilterRun (FilterResult (Json, Seq Json))
pathIter (Array items)     = return $ zipWith (\idx item -> Ok (item, Seq.singleton $ Number $ fromInteger idx)) [0..] (toList items)
pathIter (Object entries)  = return $ map (\(k, v) -> Ok (v, Seq.singleton $ String k)) $ Map.toList entries
pathIter any               = resultErr ("Cannot iterate over " <> jsonShowError any)

runIfElse :: Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runIfElse if' then' else' json = concatMapMRet eval =<< runFilter if' json
  where eval cond = runFilter (if jsonBool cond then then' else else') json
