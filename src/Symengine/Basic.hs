{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Symengine.Basic
    (
     zero,
     one,
     im,
     Symengine.Basic.pi,
     e,
     minusOne,
     rational,
     complex,
     symbol,
     subs,
     diff,
     expand
    ) where

import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Control.Applicative
import System.IO.Unsafe
import Control.Monad
import GHC.Real

import           Symengine.Foreign.Internal
import qualified Symengine.Foreign.Basic as B

newtype Basic = Basic { getBasic :: B.Basic }

-- | constructor for 0
zero :: Basic
zero = Basic (B.construct B.basic_const_zero_ffi)

-- | constructor for 1
one :: Basic
one = Basic (B.construct B.basic_const_one_ffi)

-- | constructor for -1
minusOne :: Basic
minusOne = Basic (B.construct B.basic_const_minus_one_ffi)

-- | constructor for i = sqrt(-1)
im :: Basic
im = Basic (B.construct B.basic_const_I_ffi)

-- | the ratio of the circumference of a circle to its radius
pi :: Basic
pi = Basic (B.construct B.basic_const_pi_ffi)

-- | The base of the natural logarithm
e :: Basic
e = Basic (B.construct B.basic_const_E_ffi)

expand :: Basic -> Basic
expand (Basic a) = Basic (B.lift_unaryop B.basic_expand_ffi a)

subs :: Basic -> Basic -> Basic -> Basic
subs (Basic expr) (Basic x) (Basic x') = Basic (B.lift_ternaryop B.basic_subs_ffi expr x x')

eulerGamma :: Basic
eulerGamma = Basic (B.construct B.basic_const_EulerGamma_ffi)

fromSignedInt :: Int -> Basic
fromSignedInt i = Basic (unsafePerformIO $ do
    iptr <- B.new
    with iptr (\iptr -> B.integer_set_si_ffi iptr (intToCLong i) )
    return iptr)

basicFromInteger :: Integer -> Basic
basicFromInteger i = Basic (unsafePerformIO $ do
  s <- B.new
  with s (\s -> B.integer_set_si_ffi s (fromInteger i))
  return s)

pow :: Basic -> Basic -> Basic
pow (Basic a) (Basic b) = Basic (B.lift_binaryop B.basic_pow_ffi a b)

-- |Create a rational number with numerator and denominator
rational :: Basic -> Basic -> Basic
rational (Basic num) (Basic denom) = Basic (B.lift_binaryop B.rational_set_ffi num denom)

-- |Create a complex number a + b * im
complex :: Basic -> Basic -> Basic
complex (Basic a) (Basic b) = Basic (B.lift_binaryop B.complex_set_ffi a b)

rationalFromInteger :: Integer -> Integer -> Basic
rationalFromInteger i j = Basic (unsafePerformIO $ do
    s <- B.new
    with s (\s -> B.rational_set_si_ffi s (integerToCLong i) (integerToCLong j))
    return s)

-- |Create a symbol with the given name
symbol :: String -> Basic
symbol name = Basic (unsafePerformIO $ do
    s <- B.new
    cname <- newCString name
    with s (`B.symbol_set_ffi` cname)
    free cname
    return s)

-- |Differentiate an expression with respect to a symbol
diff :: Basic -> Basic -> Basic
diff (Basic expr) (Basic x) = Basic (B.lift_binaryop B.basic_diff_ffi expr x)

instance Show Basic where
    show (Basic a) = unsafePerformIO (with a (B.basic_str_ffi >=> peekCString))

instance Eq Basic where
    (==) (Basic a) (Basic b) = 
        unsafePerformIO $ do i <- with2 a b B.basic_eq_ffi
                             return $ i == 1

instance Num Basic where
    (Basic a) + (Basic b) = Basic (B.lift_binaryop B.basic_add_ffi a b)
    (Basic a) - (Basic b) = Basic (B.lift_binaryop B.basic_sub_ffi a b)
    (Basic a) * (Basic b) = Basic (B.lift_binaryop B.basic_mul_ffi a b)
    negate (Basic a)      = Basic (B.lift_unaryop  B.basic_neg_ffi a)
    abs    (Basic a)      = Basic (B.lift_unaryop  B.basic_abs_ffi a)
    signum (Basic a)      = undefined
    fromInteger           = basicFromInteger

instance Fractional Basic where
    (Basic a) / (Basic b) = Basic (B.lift_binaryop B.basic_div_ffi a b)
    fromRational (num :% denom) = rationalFromInteger num denom
    recip r = one / r

instance Floating Basic where
    pi = Symengine.Basic.pi
    exp x = e ** x
    log = undefined
    sqrt x = x  ** 1/2
    (**) = pow
    logBase = undefined
    sin   (Basic a) = Basic (B.lift_unaryop B.basic_sin_ffi   a)
    cos   (Basic a) = Basic (B.lift_unaryop B.basic_cos_ffi   a)
    tan   (Basic a) = Basic (B.lift_unaryop B.basic_tan_ffi   a)
    asin  (Basic a) = Basic (B.lift_unaryop B.basic_asin_ffi  a)
    acos  (Basic a) = Basic (B.lift_unaryop B.basic_acos_ffi  a)
    atan  (Basic a) = Basic (B.lift_unaryop B.basic_atan_ffi  a)
    sinh  (Basic a) = Basic (B.lift_unaryop B.basic_sinh_ffi  a)
    cosh  (Basic a) = Basic (B.lift_unaryop B.basic_cosh_ffi  a)
    tanh  (Basic a) = Basic (B.lift_unaryop B.basic_tanh_ffi  a)
    asinh (Basic a) = Basic (B.lift_unaryop B.basic_asinh_ffi a)
    acosh (Basic a) = Basic (B.lift_unaryop B.basic_acosh_ffi a)
    atanh (Basic a) = Basic (B.lift_unaryop B.basic_atanh_ffi a)

