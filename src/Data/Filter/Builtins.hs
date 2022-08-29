module Data.Filter.Builtins (builtins) where

import Prelude hiding (filter, any, exp)

import Data.Filter.Internal.Run
  ( FilterRun
  , runFilter
  , FilterFunc

  , jsonShowError
  )

import Data.Filter.Internal.Result
  (FilterRet (..)
  , retOk
  , retErr

  , FilterResult
  , resultErr
  , mapMRet
  , concatMapMRet
  )

import Data.Filter (Filter)

import Data.Json (Json (..))

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

hsBuiltins :: [((Text, Int), FilterFunc)]
hsBuiltins =
  [ (("_plus",      2),   binary    plus)
  , (("_negate",    1),   unary     neg)
  , (("_minus",     2),   binary    minus)
  , (("_multiply",  2),   binary    multiply)
  , (("_divide",    2),   binary    divide)
  , (("_mod",       2),   binary    modulus)
  , (("_equal",     2),   comp      (==))
  , (("_notequal",  2),   comp      (/=))
  , (("_notequal",  2),   comp      (/=))
  , (("_less",      2),   comp      (<))
  , (("_greater",   2),   comp      (>))
  , (("_lesseq",    2),   comp      (<=))
  , (("_greatereq", 2),   comp      (>=))
  , (("error",      0),   nullary'  error0)
  ]

builtins :: IO (HashMap (Text, Int) FilterFunc)
builtins = return $ Map.fromList hsBuiltins

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

error0 :: Json -> FilterRun (FilterResult Json)
error0 (String msg)  = resultErr msg
error0 _             = resultErr "(not a string)"
