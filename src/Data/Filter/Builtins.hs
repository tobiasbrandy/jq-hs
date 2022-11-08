{-# LANGUAGE TemplateHaskell #-}
module Data.Filter.Builtins (builtins) where

import Prelude hiding (filter, any, elem, length)

import Data.Filter.Internal.Run
  ( filterRunModule
  , runFilter

  , FilterFunc
  , PathExpStatus (..)
  , PathExp
  , FilterRun
  , filterRunVarGet
  , filterRunGetPathExp
  , filterRunSetPathExp

  , project
  , slice

  , runFilterNoPath
  , notPathExp
  , invalidPathExpErr
  , cycleIdx

  , jsonShowError
  )

import Data.Filter.Internal.Result
  ( FilterRet (..)
  , retOk
  , retErr

  , FilterResult
  , resultOk
  , resultErr
  , mapRet
  , mapMRet
  , concatMapMRet
  )

import Data.Filter.Internal.Sci
  ( IntNum
  , FloatNum
  , infinity
  , sciBinOp
  , sciTruncate
  , toFloatNum
  , fromFloat
  , intNumToInt
  , sciFloor
  , sciCeiling
  , nan
  )

import Data.Filter (Filter (..))
import Data.Filter.Parsing.Parser (filterParser)

import Data.Json (Json (..), JsonNum (..), jsonNumMaybe, fromJsonNum, jsonBool, jsonShowType)
import Data.Json.Parsing.Parser (jsonParser)

import Data.Parser.Parse (parseOne, parseAll, parserStateInit)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Sequence (Seq ((:<|), (:|>)))
import qualified Data.Sequence as Seq

import qualified Data.Filter.Internal.CMath as C
import Data.Filter.Internal.CRegex
  ( compile
  , test
  , match
  , Match
  , MatchCapture

  , RegexOpt
  , optIgnoreCase
  , optExtend
  , optMultiLine
  , optSingleLine
  , optFindLongest
  , optFindNotEmpty
  )

import Data.Filter.Internal.Format
  ( jsonFormat
  , textFormat
  , csvFormat
  , tsvFormat
  , htmlFormat
  , uriFormat
  , shFormat
  , base64EncodeFormat
  , base64DecodeFormat
  )

import Data.FileEmbed (embedFile)
import Data.Foldable (foldl', minimumBy, maximumBy, toList)
import qualified Data.Foldable as F (any)
import Control.Monad (foldM, join)
import Data.Maybe (fromMaybe)
import Data.List (genericTake)
import Data.Ord (comparing)
import Data.Bifunctor (bimap, second)
import Data.Function ((&))

builtins :: HashMap (Text, Int) FilterFunc
builtins = case parseOne filterParser $ parserStateInit $ BS.fromStrict $(embedFile "src/Data/Filter/builtins.jq") of
  Left msg      -> error $ "Fatal - builtins.jq parsing failed: " <> show msg
  Right filter  -> let
      ret = filterRunModule "builtins.jq" [] hsBuiltins filter
    -- Agregamos la funcion builtins/0
      builtins0Sig = ("builtins", 0)
    in Map.insert builtins0Sig (nullary $ builtins0 $ Map.keys ret <> [builtins0Sig]) ret

hsBuiltins :: HashMap (Text, Int) FilterFunc
hsBuiltins = Map.fromList
  [ (("empty",          0),   nullary   (return []))
  , (("not",            0),   nullary'  (resultOk . Bool . not . jsonBool))
  , (("path",           1),   func1     path)
  , (("range",          2),   func2     range)
  , (("_plus",          2),   binary    plus)
  , (("_negate",        1),   unary     neg)
  , (("_minus",         2),   binary    minus)
  , (("_multiply",      2),   binary    multiply)
  , (("_divide",        2),   binary    divide)
  , (("_mod",           2),   binary    modulus)
  , (("tojson",         0),   nullary'  (resultOk . String . jsonFormat))
  , (("fromjson",       0),   nullary'  fromjson)
  , (("tonumber",       0),   nullary'  tonumber)
  , (("tostring",       0),   nullary'  (resultOk . String . textFormat))
  , (("keys",           0),   nullary'  keys)
  , (("keys_unsorted",  0),   nullary'  keysUnsorted)
  , (("startswith",     1),   unary'    startswith)
  , (("endswith",       1),   unary'    endswith)
  , (("ltrimstr",       1),   unary'    ltrimstr)
  , (("rtrimstr",       1),   unary'    rtrimstr)
  , (("split",          1),   unary'    split)
  , (("explode",        0),   nullary'  explode)
  , (("implode",        0),   nullary'  implode)
  , (("_strindices",    1),   unary'    strindices)
  , (("setpath",        2),   binary'   setpath)
  , (("getpath",        1),   func1PE   getpath)
  , (("delpaths",       1),   unary'    delpaths)
  , (("has",            1),   unary'    has)
  , (("_equal",         2),   comp      (==))
  , (("_notequal",      2),   comp      (/=))
  , (("_less",          2),   comp      (<))
  , (("_greater",       2),   comp      (>))
  , (("_lesseq",        2),   comp      (<=))
  , (("_greatereq",     2),   comp      (>=))
  , (("contains",       1),   unary'    contains)
  , (("length",         0),   nullary'  length0)
  , (("utf8bytelength", 0),   nullary'  utf8bytelength)
  , (("type",           0),   nullary'  (resultOk . String . jsonShowType))
  , (("isinfinite",     0),   nullary'  isinfinite)
  , (("isnan",          0),   nullary'  isnan)
  , (("isnormal",       0),   nullary'  isnormal)
  , (("infinite",       0),   nullary   (resultOk $ Number $ fromFloat infinity))
  , (("nan",            0),   nullary   (resultOk $ Number NaN))
  , (("sort",           0),   nullary'  (reqSortable $ Array . Seq.sort))
  , (("sort_by",        1),   func1     sortBy)
  , (("group_by",       1),   func1     groupBy)
  , (("min",            0),   nullary'  (reqSortable min0))
  , (("max",            0),   nullary'  (reqSortable max0))
  , (("min_by",         1),   func1     minBy)
  , (("max_by",         1),   func1     maxBy)
  , (("error",          0),   nullary'  error0)
  , (("format",         1),   unary'    format)
  , (("env",            0),   nullary   ((:[]) . Ok . fromMaybe Null <$> filterRunVarGet "ENV"))
  , (("halt",           0),   nullary   (return [Halt 0 Nothing]))
  , (("halt_error",     1),   unary'    haltError)
  -- {(cfunction_ptr)f_get_search_list, "get_search_list", 1},
  -- {(cfunction_ptr)f_get_prog_origin, "get_prog_origin", 1},
  -- {(cfunction_ptr)f_get_jq_origin, "get_jq_origin", 1},
  , (("_match_impl",    3),   ternary'  matchImpl)
  -- {(cfunction_ptr)f_modulemeta, "modulemeta", 1},
  -- {(cfunction_ptr)f_input, "input", 1},
  , (("debug",          0),   nullary'  (return . flip map [Debug,  Ok] . (&)))
  , (("stderr",         0),   nullary'  (return . flip map [Stderr, Ok] . (&)))
  -- {(cfunction_ptr)f_strptime, "strptime", 2},
  -- {(cfunction_ptr)f_strftime, "strftime", 2},
  -- {(cfunction_ptr)f_strflocaltime, "strflocaltime", 2},
  -- {(cfunction_ptr)f_mktime, "mktime", 1},
  -- {(cfunction_ptr)f_gmtime, "gmtime", 1},
  -- {(cfunction_ptr)f_localtime, "localtime", 1},
  -- {(cfunction_ptr)f_now, "now", 1},
  -- {(cfunction_ptr)f_current_filename, "input_filename", 1},
  -- {(cfunction_ptr)f_current_line, "input_line_number", 1},
  , math1' "frexp"      (Array . Seq.fromList . toList . fmap Number . bimap fromFloat fromIntegral . C.frexp)
  , math1' "modf"       (Array . Seq.fromList . toList . fmap (Number . fromFloat) . C.modf)
  , math1NotDefined "lgamma_r"  -- Not found in the c standard
  , math1 "acos"        C.acos
  , math1 "acosh"       C.acosh
  , math1 "asin"        C.asin
  , math1 "asinh"       C.asinh
  , math1 "atan"        C.atan
  , math2 "atan2"       C.atan2
  , math1 "atanh"       C.atanh
  , math1 "cbrt"        C.cbrt
  , math1 "cos"         C.cos
  , math1 "cosh"        C.cosh
  , math1 "exp"         C.exp
  , math1 "exp2"        C.exp2
  , math1 "floor"       C.floor
  , math2 "hypot"       C.hypot
  , math1NotDefined "j0"        -- Not found in the c standard
  , math1NotDefined "j1"        -- Not found in the c standard
  , math1 "log"         C.log
  , math1 "log10"       C.log10
  , math1 "log2"        C.log2
  , math2 "pow"         C.pow
  , math2 "remainder"   C.remainder
  , math1 "sin"         C.sin
  , math1 "sinh"        C.sinh
  , math1 "sqrt"        C.sqrt
  , math1 "tan"         C.tan
  , math1 "tanh"        C.tanh
  , math1 "tgamma"      C.tgamma
  , math1NotDefined "y0"        -- Not found in the c standard
  , math1NotDefined "y1"        -- Not found in the c standard
  , math2NotDefined "jn"        -- Not found in the c standard
  , math2NotDefined "yn"        -- Not found in the c standard
  , math1 "ceil"        C.ceil
  , math2 "copysign"    C.copysign
  , math2NotDefined "drem"      -- Not found in the c standard
  , math1 "erf"         C.erf
  , math1 "erfc"        C.erfc
  , math1NotDefined "exp10"     -- Not found in the c standard
  , math1 "expm1"       C.expm1
  , math1 "fabs"        C.fabs
  , math2 "fdim"        C.fdim
  , math3 "fma"         C.fma
  , math2 "fmax"        C.fmax
  , math2 "fmin"        C.fmin
  , math2 "fmod"        C.fmod
  , math1NotDefined "gamma"     -- Not found in the c standard
  , math1 "lgamma"      C.lgamma
  , math1 "lgamma"      C.lgamma
  , math1 "log1p"       C.log1p
  , math1 "logb"        C.logb
  , math1 "nearbyint"   C.nearbyint
  , math2 "nextafter"   C.nextafter
  , math2 "nexttoward"  C.nexttoward
  , math1NotDefined "pow10"     -- Not found in the c standard
  , math1 "rint"        C.rint
  , math1 "round"       C.round
  , math2NotDefined "scalb"     -- Not found in the c standard
  , math2 "scalbln"     (flip C.scalbln . truncate)
  , math1 "significand" ((2*) . fst . C.frexp)
  , math1 "trunc"       C.trunc
  , math2 "ldexp"       (flip C.ldexp . truncate)
  ]

------------------------ Function Declaration Utils --------------------------

func3 :: (Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func3 f (a :<| b :<| c :<| Seq.Empty) json = notPathExp $ f a b c json
func3 _ _ _ = error "Ternary functions only allow 3 params"

func2 :: (Filter -> Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func2 f (a :<| b :<| Seq.Empty) json = notPathExp $ f a b json
func2 _ _ _ = error "Binary functions only allow 2 params"

func1 :: (Filter -> Json -> FilterRun (FilterResult Json)) -> FilterFunc
func1 f (a :<| Seq.Empty) json = notPathExp $ f a json
func1 _ _ _ = error "Unary functions only allow 1 param"

-- Exception for getpath because it is a path exp
func1PE :: (Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))) -> FilterFunc
func1PE f (a :<| Seq.Empty) json = f a json
func1PE _ _ _ = error "Unary functions only allow 1 param"

func0 :: (Json -> FilterRun (FilterResult Json)) -> FilterFunc
func0 f Seq.Empty json = notPathExp $ f json
func0 _ _ _ = error "Nullary functions don't have params"

-- Argument cross product order is left to right
runTernary :: (Json -> Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runTernary op a b c json = concatMapMRet (\z ->
    concatMapMRet (\y ->
        mapMRet (\x -> op x y z) =<< runFilterNoPath a json
      ) =<< runFilterNoPath b json
  ) =<< runFilterNoPath c json

-- Argument cross product order is left to right
runBinary :: (Json -> Json -> FilterRun (FilterRet Json)) -> Filter -> Filter -> Json -> FilterRun (FilterResult Json)
runBinary op a b json = concatMapMRet (\y ->
    mapMRet (`op` y) =<< runFilterNoPath a json
  ) =<< runFilterNoPath b json

runUnary :: (Json -> FilterRun (FilterRet Json)) -> Filter -> Json -> FilterRun (FilterResult Json)
runUnary op a json = mapMRet op =<< runFilterNoPath a json

ternary :: (Json -> Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
ternary f = func3 $ runTernary f

ternary' :: (Json -> Json -> Json -> Json -> FilterRun (FilterRet Json)) -> FilterFunc
ternary' f params json = ternary (\a b c -> f a b c json) params json

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

reqNumber :: Json -> FilterRet FloatNum
reqNumber (Number n) = Ok $ maybe nan toFloatNum $ jsonNumMaybe n
reqNumber any = Err $ jsonShowError any <> " number required"

math3 :: Text -> (FloatNum -> FloatNum -> FloatNum -> FloatNum) -> ((Text, Int), FilterFunc)
math3 name f = ((name, 3),) $ ternary (\a b c -> return $ Number . fromFloat <$> (f <$> reqNumber a <*> reqNumber b <*> reqNumber c))

math2 :: Text -> (FloatNum -> FloatNum -> FloatNum) -> ((Text, Int), FilterFunc)
math2 name f = ((name, 2),) $ binary (\a b -> return $ Number . fromFloat <$> (f <$> reqNumber a <*> reqNumber b))

math1' :: Text -> (FloatNum -> Json) -> ((Text, Int), FilterFunc)
math1' name f = ((name, 0),) $ nullary' (return . (:[]) . fmap f . reqNumber)

math1 :: Text -> (FloatNum -> FloatNum) -> ((Text, Int), FilterFunc)
math1 name f = math1' name (Number . fromFloat . f)

math2NotDefined :: Text -> ((Text, Int), FilterFunc)
math2NotDefined name = ((name, 2),) $ func2 (\_ _ _ -> resultErr $ name <> "/2 not found at build time. Only math functions from the C standard are included.")

math1NotDefined :: Text -> ((Text, Int), FilterFunc)
math1NotDefined name = ((name, 0),) $ func0 (\_ -> resultErr $ name <> "/0 not found at build time. Only math functions from the C standard are included.")

------------------------ Builtins --------------------------

path :: Filter -> Json -> FilterRun (FilterResult Json)
path filter json = do
  ogPathState <- filterRunGetPathExp
  filterRunSetPathExp PathOn
  ret <- mapMRet (\(j, p) -> maybe (invalidPathExpErr j) (retOk . Array) p) =<< runFilter filter json
  filterRunSetPathExp ogPathState
  return ret

-- Argument cross product order is special (don't know why, ask jq)
range :: Filter -> Filter -> Json -> FilterRun (FilterResult Json)
range left right json = concatMapMRet (\l -> concatMapMRet (return . run l) =<< runFilterNoPath right json) =<< runFilterNoPath left json
  where
    run :: Json -> Json -> FilterResult Json
    run (Number start) (Number end) = let
        l = maybe nan toFloatNum $ jsonNumMaybe start
        r = maybe nan toFloatNum $ jsonNumMaybe end
        len = ceiling $ r - l
      in
        if len > (0::IntNum)
        then genericTake len $ map (Ok . Number . fromFloat) $ iterate (+ 1) l
        else []
    run _ _ = [Err "Range bounds must be numeric"]

plus :: Json -> Json -> FilterRun (FilterRet Json)
plus (Number l) (Number r)  = retOk $ Number  $ sciBinOp (+) l r
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
minus (Number l) (Number r)  = retOk $ Number  $ sciBinOp (-) l r
minus (Array l)  (Array  r)  = retOk $ Array   $ Seq.filter (`notElem` r) l
minus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be substracted")

multiply :: Json -> Json -> FilterRun (FilterRet Json)
multiply (Number l)   (Number r)        = retOk $ Number  $ sciBinOp (*) l r
multiply (String l)   (Number (Num r))  = retOk $ if r > 0 then String $ T.replicate (floor r) l else Null
multiply (String _)   (Number _nan)     = retOk Null
multiply l@(Object _) r@(Object  _)     = retOk $ merge l r
  where
    merge (Object l') (Object r') = Object  $ Map.unionWith merge l' r'
    merge _           r'          = r'
multiply l            r             = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be multiplied")

divide :: Json -> Json -> FilterRun (FilterRet Json)
divide jl@(Number l@(Num _)) jr@(Number r@(Num r'))
  | r' == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided because the divisor is zero")
  | otherwise = retOk $ Number $ sciBinOp (/) l r
divide (Number _nan) (Number _nan') = retOk $ Number NaN
divide (String l) (String r) = retOk $ Array $ Seq.fromList $ map String $ if T.null r then map T.singleton $ T.unpack l else T.splitOn r l
divide l          r          = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided")

modulus :: Json -> Json -> FilterRun (FilterRet Json)
modulus jl@(Number (Num l)) jr@(Number (Num r))
  | r == 0    = retErr (jsonShowError jl <> " and " <> jsonShowError jr <> " cannot be divided (remainder) because the divisor is zero")
  | otherwise = let
      left  = sciTruncate l
      ret   = abs left `mod` sciTruncate (abs r)
    in retOk $ Number $ fromIntegral $ if left < 0 then -ret else ret
modulus (Number _nan) (Number _nan') = retOk $ Number NaN
modulus l          r           = retErr (jsonShowError l <> " and " <> jsonShowError r <> " cannot be divided (remainder)")

-- We could parse and send ALL jsons encountered on parsing, but we honor jq behaviour
fromjson :: Json -> FilterRun (FilterResult Json)
fromjson (String jsonStr) = let state = parserStateInit $ BS.fromStrict $ encodeUtf8 jsonStr in
  case parseAll jsonParser id state of
    []            -> resultErr $ "Expected JSON value" <> errSuffix
    Left msg:_    -> resultErr $ msg <> errSuffix
    [Right json]  -> resultOk json
    _             -> resultErr $ "Unexpected extra JSON values" <> errSuffix
    where errSuffix = " (while parsing '" <> jsonStr <> "')"
fromjson any = resultErr $ jsonShowError any <> " only strings can be parsed"

tonumber :: Json -> FilterRun (FilterResult Json)
tonumber (String nStr)  = let state = parserStateInit $ BS.fromStrict $ encodeUtf8 nStr in
  case parseAll jsonParser id state of
  []            -> resultErr $ "Expected JSON value" <> errSuffix
  Left _:_      -> resultErr $ "Invalid numeric literal" <> errSuffix
  [Right json]  -> case json of
    r@(Number _)  -> resultOk r
    any           -> resultErr $ jsonShowError any <> " cannot be parsed as a number"
  _             -> resultErr $ "Unexpected extra JSON values" <> errSuffix
  where errSuffix = " (while parsing '" <> nStr <> "')"
tonumber n@(Number _)   = resultOk n
tonumber any            = resultErr $ jsonShowError any <> " cannot be parsed as a number"

keys :: Json -> FilterRun (FilterResult Json)
keys (Object m)     = resultOk $ Array $ fmap String $ Seq.sort $ Seq.fromList $ Map.keys m
keys (Array items)  = resultOk $ Array $ Number . fromIntegral <$> Seq.iterateN (Seq.length items) (+ 1) (0::Int)
keys any = resultErr $ jsonShowType any <> " has no keys"

keysUnsorted :: Json -> FilterRun (FilterResult Json)
keysUnsorted (Object m)     = resultOk $ Array $ fmap String $ Seq.fromList $ Map.keys m
keysUnsorted (Array items)  = resultOk $ Array $ Number . fromIntegral <$> Seq.iterateN (Seq.length items) (+ 1) (0::Int)
keysUnsorted any = resultErr $ jsonShowType any <> " has no keys"

startswith :: Json -> Json -> FilterRun (FilterRet Json)
startswith (String b) (String a) = retOk $ Bool $ T.isPrefixOf b a
startswith _ _ = retErr "startswith() requires string inputs"

endswith :: Json -> Json -> FilterRun (FilterRet Json)
endswith (String b) (String a) = retOk $ Bool $ T.isSuffixOf b a
endswith _ _ = retErr "endswith() requires string inputs"

ltrimstr :: Json -> Json -> FilterRun (FilterRet Json)
ltrimstr (String b) (String a) = retOk $ String $ fromMaybe a $ T.stripPrefix b a
ltrimstr _ a = retOk a

rtrimstr :: Json -> Json -> FilterRun (FilterRet Json)
rtrimstr (String b) (String a) = retOk $ String $ fromMaybe a $ T.stripSuffix b a
rtrimstr _ a = retOk a

split :: Json -> Json -> FilterRun (FilterRet Json)
split separator@(String _) input@(String _) = divide input separator
split _ _ = retErr "split input and separator must be strings"

explode :: Json -> FilterRun (FilterResult Json)
explode (String s) = resultOk $ Array $ Seq.fromList $ map (Number . fromIntegral . fromEnum) $ T.unpack s
explode _ = resultErr "explode input must be a string"

implode :: Json -> FilterRun (FilterResult Json)
implode (Array codes) = return $ (:[]) $ String . T.pack <$> mapM reqCodepoint (toList codes)
  where
    reqCodepoint j@(Number (Num n)) = let n' = truncate $ toFloatNum n in
      if n' >= 0 && n' <= 0x10FFFF
      then Ok $ toEnum n'
      else nonCodepointErr j
    reqCodepoint any = nonCodepointErr any

    nonCodepointErr json = Err $ "implode() requires all elements of input array to be valid unicode codepoint numbers, not " <> jsonShowError json
implode _ = resultErr "implode input must be a string"

strindices :: Json -> Json -> FilterRun (FilterRet Json)
strindices (String needle) (String haystack)
  | T.null needle = retOk $ Array Seq.empty
  | otherwise     = retOk $ Array $ Seq.fromList $ map (Number . fromIntegral . T.length . fst) $ T.breakOnAll needle haystack
strindices anyl anyr = retErr $ "Needle and haystack must be both strings, not " <> jsonShowError anyl <> " and " <> jsonShowError anyr

setpath :: Json -> Json -> Json -> FilterRun (FilterRet Json)
setpath (Array paths) value json = return $ foldr modify (const $ Ok value) paths json
  where
    modify :: Json -> (Json -> FilterRet Json) -> Json -> FilterRet Json
    modify (Object rng) fret j@(Array items) = do
      let len   = fromIntegral $ Seq.length items
      start     <- getIndex len $ Left   $ Map.lookup "start" rng
      end       <- getIndex len $ Right  $ Map.lookup "end" rng
      oldSlice  <- slice (j, Nothing) (Number $ fromIntegral <$> start) (Number $ fromIntegral <$> end)
      newSlice  <- fret $ fst oldSlice
      case newSlice of
        Array s -> Ok $ Array $ Seq.take (intNumToInt $ fromJsonNum start) items <> s <> Seq.drop (intNumToInt $ fromJsonNum end) items
        _       -> Err "A slice of an array can only be assigned another array"
    modify (String k) fret (Object m) = do
      prop <- fret $ fromMaybe Null $ Map.lookup k m
      Ok $ Object $ Map.insert k prop m
    modify (Number (Num n)) fret (Array items) = do
      let len = Seq.length items
      let i   = intNumToInt $ cycleIdx (fromIntegral len) $ sciTruncate n
      elem    <- fret $ fromMaybe Null $ Seq.lookup i items
      if i < 0
      then Err "Out of bounds negative array index"
      else Ok $
        if i < len then Array $ Seq.update i elem items
        else if i == len then Array $ items :|> elem
        else Array $ (items <> Seq.replicate (i - len) Null) :|> elem
    modify (Number _) _ (Array _) = Err "Out of bounds NaN array index"
    modify s@(String _) fret Null = modify s fret $ Object Map.empty
    modify n@(Number _) fret Null = modify n fret $ Array Seq.empty
    modify r@(Object _) fret Null = modify r (const $ fret Null) $ Array Seq.empty
    modify p _ j = Err ("Cannot index " <> jsonShowType j <> " with " <> jsonShowError p)
setpath _ _ _ = retErr "Path must be specified as an array"

-- This function is an exception to the rule, it IS an exp path (jq being jq)
getpath :: Filter -> Json -> FilterRun (FilterResult (Json, Maybe PathExp))
getpath filter json = do
  pathStatus <- filterRunGetPathExp
  pathExps <- runFilterNoPath filter json
  return $ mapRet (getpath' $ pathStatus /= PathOff) pathExps
  where
    getpath' :: Bool -> Json -> FilterRet (Json, Maybe PathExp)
    getpath' withPath (Array paths) = foldM run (json, if withPath then Just Seq.empty else Nothing) paths
      where
        run :: (Json, Maybe PathExp) -> Json -> FilterRet (Json, Maybe PathExp)
        run (j@(Array items), p) (Object rng) = do
            let len   = fromIntegral $ Seq.length items
            start <- getIndex len $ Left   $ Map.lookup "start" rng
            end   <- getIndex len $ Right  $ Map.lookup "end" rng
            slice (j, p) (Number $ fromIntegral <$> start) (Number $ fromIntegral <$> end)
        run (Null, p) j@(Object _) = Ok (Null, (:|> j) <$> p)
        run jp p = project jp p
    getpath' _ _ = Err "Path must be specified as an array"

-- Stores deletion information made to each array in json
-- Then we can reference it to make the id information of path expressions
--  not depend on the previous ones
-- PathExp -> (deletionCount, idxMapper)
type ArrIdxMapper = HashMap PathExp (Int, IntNum -> IntNum)

delpaths :: Json -> Json -> FilterRun (FilterRet Json)
delpaths (Array paths) json = return $ snd <$> foldM delpath (Map.empty, json) paths
  where
    delpath :: (ArrIdxMapper, Json) -> Json -> FilterRet (ArrIdxMapper, Json)
    delpath (idxMap, _) (Array Seq.Empty) = Ok (idxMap, Null)
    delpath (idxMap, json') (Array (ps :|> delP)) = foldr modify (delete delP) ps json'
      where
        (arrDeleted, idxMapper) = fromMaybe (0, id) $ Map.lookup ps idxMap

        modify :: Json -> (Json -> FilterRet (ArrIdxMapper, Json)) -> Json -> FilterRet (ArrIdxMapper, Json)
        modify (Object rng) fret j@(Array items) = do
          let len   = fromIntegral $ Seq.length items + arrDeleted
          start     <- fmap idxMapper <$> getIndex len (Left  $ Map.lookup "start" rng)
          end       <- fmap idxMapper <$> getIndex len (Right $ Map.lookup "end" rng)
          oldSlice  <- slice (j, Nothing) (Number $ fromIntegral <$> start) (Number $ fromIntegral <$> end)
          (newMap, newSlice)  <- fret $ fst oldSlice
          case newSlice of
            Array s -> Ok $ (newMap,) $ Array $ Seq.take (intNumToInt $ fromJsonNum start) items <> s <> Seq.drop (intNumToInt $ fromJsonNum end) items
            _       -> Err "A slice of an array can only be assigned another array"
        modify (String k) fret j@(Object m) = do
          mRet <- maybe (Ok Nothing) (fmap Just . fret) $ Map.lookup k m
          Ok $ maybe (idxMap, j) (second $ Object . flip (Map.insert k) m) mRet
        modify (Number (Num n)) fret j@(Array items) = do
          let len   = Seq.length items + arrDeleted
          let i     = intNumToInt $ idxMapper $ cycleIdx (fromIntegral len) $ sciTruncate n
          mElem     <- maybe (Ok Nothing) (fmap Just . fret) $ Seq.lookup i items
          Ok $ case mElem of
            Nothing -> (idxMap, j)
            Just (newMap, elem) -> (newMap,) $ Array $ Seq.update i elem items
        modify (Number _nan) _ j@(Array _) = Ok (idxMap, j)
        modify _ _ Null = Ok (idxMap, Null)
        modify p _ j = Err ("Cannot index " <> jsonShowType j <> " with " <> jsonShowError p)

        delete :: Json -> Json -> FilterRet (ArrIdxMapper, Json)
        delete (Object rng) j@(Array items) = do
          let len = fromIntegral $ Seq.length items + arrDeleted
          start'  <- fmap idxMapper <$> getIndex len (Left   $ Map.lookup "start" rng)
          end'    <- fmap idxMapper <$> getIndex len (Right  $ Map.lookup "end" rng)
          if start' == NaN || end' == NaN
          then Ok (Map.insert ps (arrDeleted, idxMapper) idxMap, j)
          else do
            let start   = fromJsonNum start'
            let end     = fromJsonNum end'
            let delLen  = end - start
            let newMapper i
                  | i < start = idxMapper i
                  | i >= end  = idxMapper (i - delLen)
                  | otherwise = -1
            let newMap  = Map.insert ps (arrDeleted + fromIntegral delLen, newMapper) idxMap
            return $ (newMap,) $ Array $ Seq.take (intNumToInt start) items <> Seq.drop (intNumToInt end) items
        delete (String k) (Object m)    = Ok $ (idxMap,) $ Object $ Map.delete k m
        delete (Number (Num n)) (Array items) = let
            idx     = idxMapper $ cycleIdx (fromIntegral $ Seq.length items + arrDeleted) $ sciTruncate n
            newMapper i
              | i < idx   = idxMapper i
              | i > idx   = idxMapper (i - 1)
              | otherwise = -1
            newMap  = Map.insert ps (arrDeleted + 1, newMapper) idxMap
          in Ok $ (newMap,) $ Array $ Seq.deleteAt (intNumToInt idx) items
        delete (Number _nan) j@(Array _) = Ok (idxMap, j)
        delete _ Null         = Ok (idxMap, Null)
        delete any (Object _) = Err $ "Cannot delete " <> jsonShowType any <> " fields of object"
        delete any (Array _)  = Err $ "Cannot delete " <> jsonShowType any <> " element of array"
        delete _ any          = Err $ "Cannot delete fields from " <> jsonShowType any
    delpath _ _ = Err "Path must be specified as an array"
delpaths _ _ = retErr "Paths must be specified as an array"

has :: Json -> Json -> FilterRun (FilterRet Json)
has (String key)  (Object m)    = retOk $ Bool $ Map.member key m
has (Number (Num n))    (Array items) = let n' = fromIntegral $ sciTruncate n in retOk $ Bool $ n' >= 0 && n' < Seq.length items
has (Number NaN)        (Array _)     = retOk $ Bool False
has json key = retErr $ "Cannot check whether " <> jsonShowType json <> " has a " <> jsonShowType key <> " key"

contains :: Json -> Json -> FilterRun (FilterRet Json)
contains elem json = retOk $ Bool $ run elem json
  where
    run (String  b)  (String  a)   = T.isInfixOf b a
    run (Array   bs) (Array   as)  = all (\b -> F.any (run b) as) bs
    run (Object  mb) (Object  ma)  = all (\(bk, bv) -> maybe False (run bv) $ Map.lookup bk ma) $ Map.toList mb
    run          b             a   = b == a

length0 :: Json -> FilterRun (FilterResult Json)
length0 (String s)    = resultOk $ Number $ fromIntegral $ T.length s
length0 (Array items) = resultOk $ Number $ fromIntegral $ Seq.length items
length0 (Object m)    = resultOk $ Number $ fromIntegral $ Map.size m
length0 Null          = resultOk $ Number 0
length0 any           = resultErr $ jsonShowError any <> " has no length"

utf8bytelength :: Json -> FilterRun (FilterResult Json)
utf8bytelength (String s) = resultOk $ Number $ fromIntegral $ BSS.length $ encodeUtf8 s
utf8bytelength any = resultErr $ jsonShowError any <> " only strings have UTF-8 byte length"

isinfinite :: Json -> FilterRun (FilterResult Json)
isinfinite (Number (Num n)) = resultOk $ Bool $ isInfinite $ toFloatNum n
isinfinite _                = resultOk $ Bool False

isnan :: Json -> FilterRun (FilterResult Json)
isnan (Number NaN)  = resultOk $ Bool True
isnan _             = resultOk $ Bool False

isnormal :: Json -> FilterRun (FilterResult Json)
isnormal (Number (Num n)) = let n' = toFloatNum n in resultOk $ Bool $ not $ isInfinite n'
isnormal _                = resultOk $ Bool False

sortBy :: Filter -> Json -> FilterRun (FilterResult Json)
sortBy proj (Array items) = do
  jAndProj <- mapM (addArrProjection proj) items
  return $ (:[]) (Array . fmap snd . Seq.sortBy (comparing fst) <$> sequence jAndProj)
sortBy _ any = resultErr $ jsonShowError any <> " cannot be sorted, as it is not an array"

groupBy :: Filter -> Json -> FilterRun (FilterResult Json)
groupBy proj (Array items) = do
  jAndProj <- mapM (addArrProjection proj) items
  return $ (:[]) (Array . fmap (Array . snd) . Seq.sortBy (comparing fst) . Seq.fromList . Map.toList . toMultiMap <$> sequence jAndProj)
  where
    toMultiMap = foldr (\(k, v) -> Map.alter (Just . maybe (Seq.singleton v) (v :<|)) k) Map.empty
groupBy _ any = resultErr $ jsonShowError any <> " cannot be grouped, as it is not an array"

min0 :: Seq Json -> Json
min0 Seq.Empty = Null
min0 items = minimum items

max0 :: Seq Json -> Json
max0 Seq.Empty = Null
max0 items = maximum items

minBy :: Filter -> Json -> FilterRun (FilterResult Json)
minBy _     (Array Seq.Empty) = resultOk Null
minBy proj  (Array items) = do
  jAndProj <- mapM (addArrProjection proj) items
  return $ (:[]) (snd . minimumBy (comparing fst) <$> sequence jAndProj)
minBy _ any = resultErr $ jsonShowError any <> " cannot be sorted, as it is not an array"

maxBy :: Filter -> Json -> FilterRun (FilterResult Json)
maxBy _     (Array Seq.Empty) = resultOk Null
maxBy proj  (Array items) = do
  jAndProj <- mapM (addArrProjection proj) items
  return $ (:[]) (snd . maximumBy (comparing fst) <$> sequence jAndProj)
maxBy _ any = resultErr $ jsonShowError any <> " cannot be sorted, as it is not an array"

error0 :: Json -> FilterRun (FilterResult Json)
error0 (String msg) = resultErr msg
error0 Null         = return []
error0 _            = resultErr "(not a string)"

format :: Json -> Json -> FilterRun (FilterRet Json)
format (String "text")    json          = retOk $ String $ textFormat json
format (String "json")    json          = retOk $ String $ jsonFormat json
format (String "html")    json          = retOk $ String $ htmlFormat json
format (String "uri")     json          = retOk $ String $ uriFormat json
format (String "csv")     (Array items) = return $ String <$> csvFormat items
format (String "csv")     any           = retErr $ jsonShowType any <> " cannot be csv-formatted, only array"
format (String "tsv")     (Array items) = return $ String <$> tsvFormat items
format (String "tsv")     any           = retErr $ jsonShowType any <> " cannot be tsv-formatted, only array"
format (String "sh")      (Array items) = return $ String <$> shFormat items
format (String "sh")      json          = return $ String <$> shFormat (Seq.singleton json)
format (String "base64")  json          = retOk $ String $ base64EncodeFormat json
format (String "base64d") json          = return $ String <$> base64DecodeFormat json
format (String other)     _             = retErr $ other <> " is not a valid format"
format any                _             = retErr $ jsonShowError any <> " is not a valid format"

haltError :: Json -> Json -> FilterRun (FilterRet Json)
haltError (Number n)  json = return $ Halt (fromInteger $ sciTruncate $ fromMaybe 0 $ jsonNumMaybe n) $ Just json
haltError any         _    = retErr $ jsonShowError any <> " halt_error/1: number required"

matchImpl :: Json -> Json -> Json -> Json -> FilterRun (FilterRet Json)
matchImpl (String regex) (String optStr) testFlag (String str) = return $ join $ flip fmap (strToRegexOpt optStr) $ \(global, opts) ->
  let
    justTest = case testFlag of Bool True -> True; _ -> False -- We are flexible. Only true is true
    compiled = compile (encodeUtf8 regex) opts
  in if justTest
    then either Err (Ok . Bool) $ flip test (encodeUtf8 str) =<< compiled
    else
      either Err Ok $ Array . foldr ((:<|) . matchToJson) Seq.empty <$> ((\reg -> match reg global $ encodeUtf8 str) =<< compiled)
    where
      strToRegexOpt :: Text -> FilterRet (Bool, RegexOpt)
      strToRegexOpt = foldM (\(global, opts) c -> if c == 'g' then Ok (True, opts) else (global,) . (opts <>) <$> charToOpt c) (False, mempty) . T.unpack
        where
          charToOpt c = case c of
            'i' -> Ok optIgnoreCase
            'x' -> Ok optExtend
            'm' -> Ok optMultiLine
            's' -> Ok optSingleLine
            'p' -> Ok $ optMultiLine <> optSingleLine
            'l' -> Ok optFindLongest
            'n' -> Ok optFindNotEmpty
            _   -> Err $ str <> " is not a valid modifier string"

      matchToJson :: Match -> Json
      matchToJson (offset, length, string, captures) = Object $ Map.fromList
        [ ("offset",    Number $ fromIntegral offset)
        , ("length",    Number $ fromIntegral length)
        , ("string",    String $ decodeUtf8With lenientDecode string)
        , ("captures",  Array $ foldr ((:<|) . captureToJson) Seq.empty captures)
        ]

      captureToJson :: MatchCapture -> Json
      captureToJson (Nothing, name) = Object $ Map.fromList
        [ ("offset",  Number (-1))
        , ("length",  Number 0)
        , ("string",  Null)
        , ("name",    maybe Null (String . decodeUtf8With lenientDecode) name)
        ]
      captureToJson (Just (offset, length, string), name) = Object $ Map.fromList
        [ ("offset",  Number $ fromIntegral offset)
        , ("length",  Number $ fromIntegral length)
        , ("string",  String $ decodeUtf8With lenientDecode string)
        , ("name",    maybe Null (String . decodeUtf8With lenientDecode) name)
        ]

matchImpl r@(String _) Null testFlag s@(String _) = matchImpl r (String "") testFlag s
matchImpl (String _)  (String _)  _ any = retErr $ jsonShowError any <> " cannot be matched, as it is not a string"
matchImpl any         (String _)  _ _   = retErr $ jsonShowError any <> " is not a string"
matchImpl _           any         _ _   = retErr $ jsonShowError any <> " is not a string"

builtins0 :: [(Text, Int)] -> FilterRun (FilterResult Json)
builtins0 = resultOk . Array . foldl' sigToNames Seq.empty
  where
    sigToNames ret (name, argc) =
      if T.null name || T.head name == '_'
      then ret
      else ret :|> String (name <> "/" <> T.pack (show argc))

------------------------ Aux --------------------------

addArrProjection :: Filter -> Json -> FilterRun (FilterRet (Json, Json))
addArrProjection proj i = do
  projection <- runFilterNoPath proj i
  return $ (,i) . Array . Seq.fromList . take maxBound <$> sequence projection

-- For slice operations
getIndex :: IntNum -> Either (Maybe Json) (Maybe Json) -> FilterRet (JsonNum IntNum)
getIndex _    (Left  (Just Null))        = Ok 0
getIndex len  (Right (Just Null))        = Ok $ Num len
getIndex len  (Left  (Just (Number n)))  = Ok $ cycleIdx len . sciFloor <$> n
getIndex len  (Right (Just (Number n)))  = Ok $ cycleIdx len . sciCeiling <$> n
getIndex _    (Left  Nothing)            = Err "'start' and 'end' properties must be included in rng object"
getIndex _    (Right Nothing)            = Err "'start' and 'end' properties must be included in range object"
getIndex _    (Left  (Just any))         = Err $ "Start and end indices of an array slice must be numbers not " <> jsonShowType any <> " (start)"
getIndex _    (Right (Just any))         = Err $ "Start and end indices of an array slice must be numbers not " <> jsonShowType any <> " (end)"

reqSortable :: (Seq Json -> Json) -> Json -> FilterRun (FilterResult Json)
reqSortable f (Array items) = resultOk $ f items
reqSortable _ any = resultErr $ jsonShowError any <> " cannot be sorted, as it is not an array"
