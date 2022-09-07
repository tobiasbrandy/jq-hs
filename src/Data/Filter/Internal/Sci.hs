module Data.Filter.Internal.Sci
( FloatNum
, IntNum

, infinity
, nan
, maxFloatNum
, toFloatNum
, fromFloat
, sciTruncate
, sciCeiling
, sciFloor
, sciBinOp
, intNumToInt
) where

import Data.Scientific (Scientific, toBoundedRealFloat, fromFloatDigits, toRealFloat)

-- RealFloat representation to use for scientific numbers during operations
type FloatNum = Double

-- Integral representation to use for scientific numbers during operations
type IntNum = Integer

infinity :: FloatNum
infinity = 1/0

nan :: FloatNum
nan = infinity/infinity

maxFloatNum :: FloatNum
maxFloatNum = encodeFloat m n
  where
    cnst = 0 :: FloatNum
    b = floatRadix cnst
    e = floatDigits cnst
    (_, e') = floatRange cnst
    m = b ^ e - 1
    n = e' - e

-- Infinity is replaced for FloatNum upper bound
toFloatNum :: Scientific -> FloatNum
toFloatNum n = case toBoundedRealFloat n of
  Left e -> if e == 0 then 0 else if e > 0 then maxFloatNum else -maxFloatNum
  Right n' -> n'

fromFloat :: RealFloat a => a -> Scientific
fromFloat = fromFloatDigits

sciTruncate :: Scientific -> IntNum
sciTruncate = truncate . toFloatNum

sciCeiling :: Scientific -> IntNum
sciCeiling = ceiling . toFloatNum

sciFloor :: Scientific -> IntNum
sciFloor = floor . toFloatNum

maxBoundInt :: IntNum
maxBoundInt = fromIntegral (maxBound :: Int)

minBoundInt :: IntNum
minBoundInt = fromIntegral (minBound :: Int)

-- Handle overflow safely returning Int bounds
intNumToInt :: IntNum -> Int
intNumToInt n
  | n > maxBoundInt = maxBound
  | n < minBoundInt = minBound
  | otherwise = fromIntegral n

-- op handles infinity, so we use unsafe 'toRealFloat'
sciBinOp :: RealFloat a => (a -> a -> FloatNum) -> Scientific -> Scientific -> Scientific
sciBinOp op l r  = fromFloat $ toRealFloat l `op` toRealFloat r
