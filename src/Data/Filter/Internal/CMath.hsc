{-# LANGUAGE CPP, ForeignFunctionInterface, CApiFFI #-}

--------------------------------------------------------------------
-- |
-- Module    : Foreign.C.Math.Double
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability: FFI
--
--------------------------------------------------------------------
-- |
-- Modifications where performed for compatibility and extension
--
--------------------------------------------------------------------
-- A binding to C's math.h double functions.
--

module Data.Filter.Internal.CMath
( frexp
, modf
, acos
, acosh
, asin
, asinh
, atan
, atan2
, atanh
, cbrt
, cos
, cosh
, exp
, exp2
, floor
, hypot
, log
, log10
, log2
, pow
, remainder
, sin
, sinh
, sqrt
, tan
, tanh
, tgamma
, ceil
, copysign
, erf
, erfc
, expm1
, fabs
, fdim
, fma
, fmax
, fmin
, fmod
, lgamma
, log1p
, logb
, nearbyint
, nextafter
, nexttoward
, rint
, round
, scalbln
, trunc
, ldexp
) where

import Prelude (Double, realToFrac, fromIntegral, ($), return, IO)

import System.IO.Unsafe (unsafePerformIO)

import Foreign
import Foreign.C.Types

-- | The acos function computes the principal value of the arc cosine of x
-- in the range [0, pi]
--
acos :: Double -> Double
acos x = realToFrac (c_acos (realToFrac x))
{-# INLINE acos #-}

foreign import capi unsafe "math.h acos" c_acos     :: CDouble -> CDouble

-- | The asin function computes the principal value of the arc sine of x in
-- the range [-pi/2, +pi/2].
--
asin :: Double -> Double
asin x = realToFrac (c_asin (realToFrac x))
{-# INLINE asin #-}

foreign import capi unsafe "math.h asin" c_asin     :: CDouble -> CDouble

-- | The atan function computes the principal value of the arc tangent of x
-- in the range [-pi/2, +pi/2].
--
atan :: Double -> Double
atan x = realToFrac (c_atan (realToFrac x))
{-# INLINE atan #-}

foreign import capi unsafe "math.h atan" c_atan     :: CDouble -> CDouble

-- | The atan2 function computes the principal value of the arc tangent of
-- y/x, using the signs of both arguments to determine the quadrant of the
-- return value.
--
atan2 :: Double -> Double -> Double
atan2 x y = realToFrac (c_atan2 (realToFrac x) (realToFrac y))
{-# INLINE atan2 #-}

foreign import capi unsafe "math.h atan2" c_atan2    :: CDouble -> CDouble -> CDouble

-- | The cos function computes the cosine of x (measured in radians).
-- A large magnitude argument may yield a result with little or no significance.  For a
-- discussion of error due to roundoff, see math(3).
--
cos :: Double -> Double
cos x = realToFrac (c_cos (realToFrac x))
{-# INLINE cos #-}

foreign import capi unsafe "math.h cos" c_cos      :: CDouble -> CDouble

-- | The sin function computes the sine of x (measured in radians). 
-- A large magnitude argument may yield a result with little or no
-- significance.  For a discussion of error due to roundoff, see math(3).
--
sin :: Double -> Double
sin x = realToFrac (c_sin (realToFrac x))
{-# INLINE sin #-}

foreign import capi unsafe "math.h sin" c_sin      :: CDouble -> CDouble

-- | The tan function computes the tangent of x (measured in radians). 
-- A large magnitude argument may yield a result with little or no
-- significance.  For a discussion of error due to roundoff, see math(3).
--
tan :: Double -> Double
tan x = realToFrac (c_tan (realToFrac x))
{-# INLINE tan #-}

foreign import capi unsafe "math.h tan" c_tan      :: CDouble -> CDouble

-- | The cosh function computes the hyperbolic cosine of x.
--
cosh :: Double -> Double
cosh x = realToFrac (c_cosh (realToFrac x))
{-# INLINE cosh #-}

foreign import capi unsafe "math.h cosh" c_cosh     :: CDouble -> CDouble

-- | The sinh function computes the hyperbolic sine of x.
--
sinh :: Double -> Double
sinh x = realToFrac (c_sinh (realToFrac x))
{-# INLINE sinh #-}

foreign import capi unsafe "math.h sinh" c_sinh     :: CDouble -> CDouble

-- | The tanh function computes the hyperbolic tangent of x.
--
tanh :: Double -> Double
tanh x = realToFrac (c_tanh (realToFrac x))
{-# INLINE tanh #-}

foreign import capi unsafe "math.h tanh" c_tanh     :: CDouble -> CDouble

------------------------------------------------------------------------

-- | The exp() function computes the exponential value of the given argument x. 
--
exp :: Double -> Double
exp x = realToFrac (c_exp (realToFrac x))
{-# INLINE exp  #-}

foreign import capi unsafe "math.h exp" c_exp      :: CDouble -> CDouble

-- | The exp2() function computes 2 raised to the power of the given argument x. 
-- exp2 is not defined in the Haskell 98 report.
--
exp2 :: Double -> Double
exp2 x = realToFrac (c_exp2 (realToFrac x))
{-# INLINE exp2  #-}

foreign import capi unsafe "math.h exp2" c_exp2      :: CDouble -> CDouble

-- | The expm1() function computes the exponential value of the given argument x, minus 1. 
-- expm1 is not defined in the Haskell 98 report.
--
expm1 :: Double -> Double
expm1 x = realToFrac (c_expm1 (realToFrac x))
{-# INLINE expm1  #-}

foreign import capi unsafe "math.h expm1" c_expm1      :: CDouble -> CDouble

-- | frexp convert floating-point number to fractional and integral components
-- frexp is not defined in the Haskell 98 report.
--
frexp :: Double -> (Double,Int)
frexp x = unsafePerformIO $
    alloca $ \p -> do
        d <- c_frexp (realToFrac x) p
        i <- peek p
        return (realToFrac d, fromIntegral i)

foreign import capi unsafe "math.h frexp" c_frexp    :: CDouble -> Ptr CInt -> IO Double

-- | The ldexp function multiplies a floating-point number by an integral power of 2.
-- ldexp is not defined in the Haskell 98 report.
--
ldexp :: Double -> Int -> Double
ldexp x i = realToFrac (c_ldexp (realToFrac x) (fromIntegral i))
{-# INLINE ldexp #-}

foreign import capi unsafe "math.h ldexp" c_ldexp    :: CDouble -> CInt -> Double

-- | The log() function computes the value of the natural logarithm of argument x.
--
log :: Double -> Double
log x = realToFrac (c_log (realToFrac x))
{-# INLINE log  #-}

foreign import capi unsafe "math.h log" c_log      :: CDouble -> CDouble

-- | The log10 function computes the value of the logarithm of argument x to base 10.
-- log10 is not defined in the Haskell 98 report.
--
log10 :: Double -> Double
log10 x = realToFrac (c_log10 (realToFrac x))
{-# INLINE log10 #-}

foreign import capi unsafe "math.h log10" c_log10    :: CDouble -> CDouble

-- | The log2 function computes the value of the logarithm of argument x to base 2.
-- log2 is not defined in the Haskell 98 report.
--
log2 :: Double -> Double
log2 x = realToFrac (c_log2 (realToFrac x))
{-# INLINE log2 #-}

foreign import capi unsafe "math.h log2" c_log2    :: CDouble -> CDouble

-- | The log1p() function computes the value of the natural logarithm of argument 1 plus x.
-- log1p is not defined in the Haskell 98 report.
--
log1p :: Double -> Double
log1p x = realToFrac (c_log1p (realToFrac x))
{-# INLINE log1p #-}

foreign import capi unsafe "math.h log1p" c_log1p    :: CDouble -> CDouble

-- | The modf function breaks the argument value into integral and fractional
-- parts, each of which has the same sign as the argument.
-- modf is not defined in the Haskell 98 report.
--
modf :: Double -> (Double,Double)
modf x = unsafePerformIO $
    alloca $ \p -> do
        d <- c_modf (realToFrac x) p
        i <- peek p
        return (realToFrac d, realToFrac i)

foreign import capi unsafe "math.h modf" c_modf     :: CDouble -> Ptr CDouble -> IO CDouble

-- | The pow function computes the value of x to the exponent y.
--
pow :: Double -> Double -> Double
pow x y = realToFrac (c_pow (realToFrac x) (realToFrac y))
{-# INLINE pow #-}

foreign import capi unsafe "math.h pow" c_pow      :: CDouble -> CDouble -> CDouble

-- | The sqrt function computes the non-negative square root of x.
--
sqrt :: Double -> Double
sqrt x = realToFrac (c_sqrt (realToFrac x))
{-# INLINE sqrt #-}

foreign import capi unsafe "math.h sqrt" c_sqrt     :: CDouble -> CDouble

-- | The ceil function returns the smallest integral value greater than or equal to x.
--
ceil :: Double -> Double
ceil x = realToFrac (c_ceil (realToFrac x))
{-# INLINE ceil #-}

foreign import capi unsafe "math.h ceil" c_ceil     :: CDouble -> CDouble

-- | The fdim function computes the positive difference between floating-point numbers x and y.
-- fdim is not defined in the Haskell 98 report.
--
fdim :: Double -> Double -> Double
fdim x y = realToFrac (c_fdim (realToFrac x) (realToFrac y))
{-# INLINE fdim #-}

foreign import capi unsafe "math.h fdim" c_fdim     :: CDouble -> CDouble -> CDouble

-- | The fmax function computes the maximum between floating-point numbers x and y.
-- fmax is not defined in the Haskell 98 report.
--
fmax :: Double -> Double -> Double
fmax x y = realToFrac (c_fmax (realToFrac x) (realToFrac y))
{-# INLINE fmax #-}

foreign import capi unsafe "math.h fmax" c_fmax     :: CDouble -> CDouble -> CDouble

-- | The fmin function computes the minimum between floating-point numbers x and y.
-- fdim is not defined in the Haskell 98 report.
--
fmin :: Double -> Double -> Double
fmin x y = realToFrac (c_fmin (realToFrac x) (realToFrac y))
{-# INLINE fmin #-}

foreign import capi unsafe "math.h fmin" c_fmin     :: CDouble -> CDouble -> CDouble

-- | The fabs function computes the absolute value of a floating-point number x.
--
fabs :: Double -> Double
fabs x = realToFrac (c_fabs (realToFrac x))
{-# INLINE fabs #-}

foreign import capi unsafe "math.h fabs" c_fabs     :: CDouble -> CDouble

-- | The fma function computes the product between floating-point numbers x and y,
-- and the addition of floating-point number z.
-- fma is not defined in the Haskell 98 report.
--
fma :: Double -> Double -> Double -> Double
fma x y z = realToFrac (c_fma (realToFrac x) (realToFrac y) (realToFrac z))
{-# INLINE fma #-}

foreign import capi unsafe "math.h fma" c_fma     :: CDouble -> CDouble -> CDouble -> CDouble

-- | The fabs function computes the absolute value of a floating-point number x.

-- | The floor function returns the largest integral value less than or equal to x.
--
floor :: Double -> Double
floor x = realToFrac (c_floor (realToFrac x))
{-# INLINE floor #-}

foreign import capi unsafe "math.h floor" c_floor    :: CDouble -> CDouble

-- | The fmod function computes the floating-point remainder of x \/ y.
--
fmod :: Double -> Double -> Double
fmod x y = realToFrac (c_fmod (realToFrac x) (realToFrac y))
{-# INLINE fmod #-}

foreign import capi unsafe "math.h fmod" c_fmod     :: CDouble -> CDouble -> CDouble

-- | The round function returns the nearest integral value to x; if x lies
-- halfway between two integral values, then these functions return the integral
-- value with the larger absolute value (i.e., it rounds away from zero).
-- 
round :: Double -> Double
round x = realToFrac (c_round (realToFrac x))
{-# INLINE round #-}

foreign import capi unsafe "math.h round" c_round    :: CDouble -> CDouble

-- | The fmod function computes the floating-point remainder of x \/ y.
--
trunc :: Double -> Double
trunc x = realToFrac (c_trunc (realToFrac x))
{-# INLINE trunc #-}

foreign import capi unsafe "math.h trunc" c_trunc    :: CDouble -> CDouble

-- | The erf calculates the error function of x. The error function is defined as:
--
-- > erf(x) = 2/sqrt(pi)*integral from 0 to x of exp(-t*t) dt.
--
erf :: Double -> Double
erf x = realToFrac (c_erf (realToFrac x))
{-# INLINE erf #-}

foreign import capi unsafe "math.h erf" c_erf      :: CDouble -> CDouble

-- | The erfc function calculates the complementary error function of x;
-- that is erfc() subtracts the result of the error function erf(x) from
-- 1.0.  This is useful, since for large x places disappear.
--
erfc :: Double -> Double
erfc x = realToFrac (c_erfc (realToFrac x))
{-# INLINE erfc #-}

foreign import capi unsafe "math.h erfc" c_erfc     :: CDouble -> CDouble

-- | The gamma function.
--
tgamma :: Double -> Double
tgamma x = realToFrac (c_tgamma (realToFrac x))
{-# INLINE tgamma #-}

foreign import capi unsafe "math.h tgamma" c_tgamma    :: CDouble -> CDouble

-- | The hypot function function computes the sqrt(x*x+y*y) in such a way that
-- underflow will not happen, and overflow occurs only if the final result
-- deserves it.  
-- 
-- > hypot(Infinity, v) = hypot(v, Infinity) = +Infinity for all v, including NaN.
--
hypot :: Double -> Double -> Double
hypot x y = realToFrac (c_hypot (realToFrac x) (realToFrac y))
{-# INLINE hypot #-}

foreign import capi unsafe "math.h hypot" c_hypot    :: CDouble -> CDouble -> CDouble

-- | lgamma(x) returns ln|| (x)|.
--
lgamma :: Double -> Double
lgamma x = realToFrac (c_lgamma (realToFrac x))
{-# INLINE lgamma #-}

foreign import capi unsafe "math.h lgamma" c_lgamma    :: CDouble -> CDouble


-- | The acosh function computes the inverse hyperbolic cosine of the real argument x. 
--
acosh :: Double -> Double
acosh x = realToFrac (c_acosh (realToFrac x))
{-# INLINE acosh #-}

foreign import capi unsafe "math.h acosh" c_acosh    :: CDouble -> CDouble

-- | The asinh function computes the inverse hyperbolic sine of the real argument. 
--
asinh :: Double -> Double
asinh x = realToFrac (c_asinh (realToFrac x))
{-# INLINE asinh #-}

foreign import capi unsafe "math.h asinh" c_asinh    :: CDouble -> CDouble

-- | The atanh function computes the inverse hyperbolic tangent of the real argument x.
--
atanh :: Double -> Double
atanh x = realToFrac (c_atanh (realToFrac x))
{-# INLINE atanh #-}

foreign import capi unsafe "math.h atanh" c_atanh    :: CDouble -> CDouble

-- | The cbrt function computes the cube root of x.
--
cbrt :: Double -> Double
cbrt x = realToFrac (c_cbrt (realToFrac x))
{-# INLINE cbrt #-}

foreign import capi unsafe "math.h cbrt" c_cbrt    :: CDouble -> CDouble

-- | logb x returns x's exponent n, a signed integer converted to
-- double-precision floating-point.  
-- 
-- > logb(+-Infinity) = +Infinity;
-- > logb(0) = -Infinity with a division by zero exception.
--
logb :: Double -> Double
logb x = realToFrac (c_logb (realToFrac x))
{-# INLINE logb #-}

foreign import capi unsafe "math.h logb" c_logb    :: CDouble -> CDouble

-- | nearbyint x returns the value of x rounded to a nearby integral (as a floating-point value).
--
nearbyint :: Double -> Double
nearbyint x = realToFrac (c_nearbyint (realToFrac x))
{-# INLINE nearbyint #-}

foreign import capi unsafe "math.h nearbyint" c_nearbyint    :: CDouble -> CDouble

-- | nextafter returns the next machine representable number from x in direction y.
--
nextafter :: Double -> Double -> Double
nextafter x y = realToFrac (c_nextafter (realToFrac x) (realToFrac y))
{-# INLINE nextafter #-}

foreign import capi unsafe "math.h nextafter" c_nextafter    :: CDouble -> CDouble -> CDouble

-- | nexttoward returns the next machine representable number from x in direction y.
-- This function behaves as nextafter, but with a potentially more precise y.
--
nexttoward :: Double -> Double -> Double
nexttoward x y = realToFrac (c_nexttoward (realToFrac x) (realToFrac y))
{-# INLINE nexttoward #-}

foreign import capi unsafe "math.h nexttoward" c_nexttoward    :: CDouble -> CDouble -> CDouble

-- | remainder returns the remainder r := x - n*y where n is the integer
-- nearest the exact value of x/y; moreover if |n - x/y| = 1/2 then n is even.
-- Consequently, the remainder is computed exactly and |r| <= |y|/2.  But
-- remainder(x, 0) and remainder(Infinity, 0) are invalid operations that produce
-- a NaN.  --
remainder :: Double -> Double -> Double
remainder x y = realToFrac (c_remainder (realToFrac x) (realToFrac y))
{-# INLINE remainder #-}

foreign import capi unsafe "math.h remainder" c_remainder    :: CDouble -> CDouble -> CDouble

-- | scalbln(x, n) returns x*(2**n) computed by exponent manipulation. n is a long.
scalbln :: Double -> Int64 -> Double
scalbln x y = realToFrac (c_scalbln (realToFrac x) (fromIntegral y))
{-# INLINE scalbln #-}

foreign import capi unsafe "math.h scalbln" c_scalbln    :: CDouble -> CLong -> CDouble

-- |  copysign x y returns x with its sign changed to y's.
copysign :: Double -> Double -> Double
copysign x y = realToFrac (c_copysign (realToFrac x) (realToFrac y))
{-# INLINE copysign #-}

foreign import capi unsafe "math.h copysign" c_copysign    :: CDouble -> CDouble -> CDouble

-- | The rint() function returns the integral value (represented as a
-- double precision number) nearest to x according to the prevailing
-- rounding mode.
--
rint :: Double -> Double
rint x = realToFrac (c_rint (realToFrac x))
{-# INLINE rint #-}

foreign import capi unsafe "math.h rint" c_rint    :: CDouble -> CDouble
