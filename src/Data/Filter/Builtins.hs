{-# LANGUAGE TemplateHaskell #-}
module Data.Filter.Builtins (builtins) where

import Prelude hiding (filter, any, exp)

import Data.Filter.Internal.Run
  ( filterRunModule
  , runFilter

  , FloatNum
  , FilterFunc
  , PathExpStatus (..)
  , FilterRun
  , filterRunGetPathExp
  , filterRunSetPathExp

  , project
  , slice

  , jsonBool
  , runFilterNoPath
  , notPathExp
  , invalidPathExpErr
  , cycleIdx

  , jsonShowError
  )

import Data.Filter.Internal.Result
  (FilterRet (..)
  , retOk
  , retErr
  , flatRet

  , FilterResult
  , resultOk
  , resultErr
  , mapMRet
  , concatMapMRet
  )

import Data.Filter (Filter (..))

import Data.Json (Json (..), jsonShowType, toNum)

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
import Data.Foldable (foldl')
import TextShow (showt)
import Control.Monad (foldM)
import Data.Scientific (toRealFloat, fromFloatDigits, Scientific)
import Data.Maybe (fromMaybe)

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
  , (("setpath",    2),   binary'   setpath)
  , (("getpath",    1),   unary'    getpath)
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
func2 f (a :<| b :<| Seq.Empty) json = notPathExp $ f a b json
func2 _ _ _ = error "Binary functions only allow 2 params"

func1 :: (Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func1 f (a :<| Seq.Empty) json = notPathExp $ f a json
func1 _ _ _ = error "Unary functions only allow 1 param"

func0 :: (Json -> FilterRun (FilterResult Json)) -> FilterFunc
func0 f Seq.Empty json = notPathExp $ f json
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

comp :: (Json -> Json -> Bool) -> FilterFunc
comp op = binary (\l -> retOk . Bool . op l)

runUnary :: (Json -> FilterRun (FilterRet Json)) -> Filter -> Json -> FilterRun (FilterResult Json)
runUnary op exp json = mapMRet op =<< runFilterNoPath exp json

runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op left right json = concatMapMRet (\l -> mapMRet (op l) =<< runFilterNoPath right json) =<< runFilterNoPath left json

------------------------ Builtins --------------------------

path :: Filter -> Json -> FilterRun (FilterResult Json)
path filter json = do
  ogPathState <- filterRunGetPathExp
  filterRunSetPathExp PathOn
  ret <- mapMRet (\(j, p) -> maybe (invalidPathExpErr j) (retOk . Array) p) =<< runFilter filter json
  filterRunSetPathExp ogPathState
  return ret

plus :: Json -> Json -> FilterRun (FilterRet Json)
plus (Number l) (Number r)  = retOk $ Number  $ applyToSci (+) l r
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
minus (Number l) (Number r)  = retOk $ Number  $ applyToSci (-) l r
minus (Array l)  (Array  r)  = retOk $ Array   $ Seq.filter (`notElem` r) l
minus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

multiply :: Json -> Json -> FilterRun (FilterRet Json)
multiply (Number l)   (Number r)    = retOk $ Number  $ applyToSci (*) l r
multiply (String l)   (Number r)    = retOk $ if r > 0 then String $ T.replicate (floor r) l else Null
multiply l@(Object _) r@(Object  _) = retOk $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
multiply l            r             = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

divide :: Json -> Json -> FilterRun (FilterRet Json)
divide jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = retOk $ Number $ applyToSci (/) l r
divide (String l) (String r) = retOk $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
divide l          r          = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

modulus :: Json -> Json -> FilterRun (FilterRet Json)
modulus jl@(Number l) jr@(Number r)
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = retOk $ Number $ fromInteger $ truncate l `mod` truncate (abs r)
modulus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

setpath :: Json -> Json -> Json -> FilterRun (FilterRet Json)
setpath (Array paths) value json = return $ foldl' run (const $ Ok value) paths json
  where
    run fret (Object range) j@(Array items) = let
        len       = Seq.length items
        start     = getIndex len $ Left   $ Map.lookup "start" range
        end       = getIndex len $ Right  $ Map.lookup "end" range
        newSlice  = flatRet $ fromArray <$> flatRet (fret . fst <$> flatRet (slice (j, Nothing) <$> (toNum <$> start) <*> (toNum <$> end)))
      in Array <$> ((\a b c -> a <> b <> c) <$> ((`Seq.take` items) <$> start) <*> newSlice <*> ((`Seq.drop` items) <$> end))
      where
        fromArray (Array i) = Ok i
        fromArray _ = Err "A slice of an array can only be assigned another array"
    run fret (String k) (Object m) = Object . (flip $ Map.insert k) m <$> fret (fromMaybe Null $ Map.lookup k m)
    run fret (Number n) (Array items) = let i = cycleIdx (Seq.length items) $ truncate n in
      if i < 0
      then Err "Out of bounds negative array index"
      else Array . (Seq.replicate i Null :|>) <$> fret (fromMaybe Null $ Seq.lookup i items)
    run fret s@(String _) Null = run fret s $ Object Map.empty
    run fret n@(Number _) Null = run fret n $ Array Seq.empty
    run fret r@(Object _) Null = run (const $ fret Null) r $ Array Seq.empty
    run _ p          j          = Err ("Cannot index " <> jsonShowType j <> " with " <> jsonShowError p)
setpath _ _ _ = retErr "Path must be specified as an array"

getpath :: Json -> Json -> FilterRun (FilterRet Json)
getpath (Array paths) json = return $ foldM run json paths
  where
    run j@(Array items) (Object range) = let
        len   = Seq.length items
        start = getIndex len $ Left   $ Map.lookup "start" range
        end   = getIndex len $ Right  $ Map.lookup "end" range
      in fst <$> flatRet (slice (j, Nothing) <$> (toNum <$> start) <*> (toNum <$> end))
    run Null (Object _) = Ok Null
    run j p = fst <$> project (j, Nothing) p
getpath _ _ = retErr "Path must be specified as an array"

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

------------------------ Aux --------------------------

applyToSci :: RealFloat a => (a -> a -> FloatNum) -> Scientific -> Scientific -> Scientific
applyToSci op l r  = fromFloatDigits $ toRealFloat l `op` toRealFloat r

-- For slice operations
getIndex :: Integral a => a -> Either (Maybe Json) (Maybe Json) -> FilterRet a
getIndex _    (Left  (Just Null))        = Ok 0
getIndex len  (Right (Just Null))        = Ok len
getIndex len  (Left  (Just (Number n)))  = Ok $ cycleIdx len $ floor n
getIndex len  (Right (Just (Number n)))  = Ok $ cycleIdx len $ ceiling n
getIndex _    (Left  Nothing)            = Err "'start' and 'end' properties must be included in range object"
getIndex _    (Right Nothing)            = Err "'start' and 'end' properties must be included in range object"
getIndex _    (Left  (Just any))         = Err $ "Start and end indices of an array slice must be numbers not " <> jsonShowType any <> " (start)"
getIndex _    (Right (Just any))         = Err $ "Start and end indices of an array slice must be numbers not " <> jsonShowType any <> " (end)"
