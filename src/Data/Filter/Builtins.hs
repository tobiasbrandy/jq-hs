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

import Data.Filter (Filter)

import Data.Json (Json (..), jsonShowType)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as BS
import Data.FileEmbed (embedFile)
import Lib (parseFilter)
import Parse.Defs (parserStateInit)
import Data.Foldable (foldl')
import TextShow (showt)

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
  -- , (("path",    1),   binary    plus)
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
  , (("has",        1),   unary'      has)
  , (("_equal",     2),   comp      (==))
  , (("_notequal",  2),   comp      (/=))
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

binary :: (Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
binary f (a :<| b :<| Empty) = runBinary f a b
binary _ _ = error "binary: Binary functions only allow 2 params"

binary' :: (Json -> Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
binary' f (a :<| b :<| Empty) json = runBinary (f json) a b json
binary' _ _ _ = error "binary': Binary functions only allow 2 params"

unary :: (Json -> FilterRun (FilterRet Json)) -> FilterFunc
unary f (a :<| Empty) = runUnary f a
unary _ _ = error "unary: Unary functions only allow 1 param"

unary' :: (Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
unary' f (a :<| Empty) json = runUnary (f json) a json
unary' _ _ _ = error "unary'': Unary functions only allow 1 params"

nullary :: FilterRun (FilterResult Json) -> FilterFunc
nullary f Seq.Empty _ = f
nullary _ _ _ = error "nullary: Nullary functions dont have params"

nullary' :: (Json -> FilterRun (FilterResult Json)) -> FilterFunc
nullary' f Seq.Empty json = f json
nullary' _ _ _ = error "nullary': Nullary functions dont have params"

comp :: (Json -> Json -> Bool) -> Seq Filter -> Json -> FilterRun (FilterResult Json)
comp op = binary (\l -> retOk . Bool . op l)

runUnary :: (Json -> FilterRun (FilterRet Json)) -> Filter -> Json -> FilterRun (FilterResult Json)
runUnary op exp json = mapMRet op =<< runFilter exp json

runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op left right json = concatMapMRet (\l -> mapMRet (op l) =<< runFilter right json) =<< runFilter left json

------------------------ Builtins --------------------------

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
has (Object m)    (String key)  = retOk $ Bool $ Map.member key m
has (Array items) (Number n)    = let n' = truncate n in retOk $ Bool $ n' >= 0 && n' < Seq.length items
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
