module Data.Filter.Internal.Sci 
( FloatNum
, IntNum

, maxFloatNum
, toFloatNum
, sciTruncate
, sciCeiling
, sciFloor
, sciBinOp
) where

import Data.Scientific (Scientific, toBoundedRealFloat, fromFloatDigits, toRealFloat)

-- RealFloat representation to use for scientific numbers during operations
type FloatNum = Double

-- Integral representation to use for scientific numbers during operations
type IntNum = Integer

maxFloatNum :: FloatNum
maxFloatNum = encodeFloat m n
  where
    cnst :: FloatNum
    cnst = 0
    b = floatRadix cnst
    e = floatDigits cnst
    (_, e') = floatRange cnst
    m = b ^ e - 1
    n = e' - e

-- Infinity is replaced for the maximum FloatNum available
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

-- op handles infinity, so we use unsafe 'toRealFloat'
sciBinOp :: RealFloat a => (a -> a -> FloatNum) -> Scientific -> Scientific -> Scientific
sciBinOp op l r  = fromFloat $ toRealFloat l `op` toRealFloat r
