{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Symengine.Foreign.Basic where

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

import Symengine.Foreign.Internal

data CBasic
data Basic = Basic !(ForeignPtr CBasic)

instance Wrapped Basic CBasic where
    with (Basic (p)) f = withForeignPtr p f

construct :: (Ptr CBasic -> IO ()) -> Basic
construct init_fn = unsafePerformIO $ do
    basic_ptr <- new
    with basic_ptr init_fn
    return basic_ptr

-- |Create a basic object that represents all other objects through
-- the FFI
new :: IO Basic
new = do
    basic_ptr <- basic_new_heap_ffi
    finalized_ptr <- newForeignPtr ptr_basic_free_heap_ffi  basic_ptr

    return $ Basic finalized_ptr
    
liftB3 :: (Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt) -> 
    Basic -> Basic -> Basic -> Basic
liftB3 f a b c = unsafePerformIO $ do
    s <- new
    with4 s a b c f >>= throwOnSymIntException

    return s

-- NOTE: throws exception
liftB2 :: (Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt) -> 
    Basic -> Basic -> Basic
liftB2 f a b = unsafePerformIO $ do
    s <- new
    with3 s a b f >>= throwOnSymIntException

    return s

liftB :: (Ptr CBasic -> Ptr CBasic -> IO CInt) -> Basic -> Basic
liftB f a = unsafePerformIO $ do
    s <- new 
    with2 s a f >>= throwOnSymIntException
    return $ s

foreign import ccall "symengine/cwrapper.h basic_new_heap" basic_new_heap_ffi :: IO (Ptr CBasic)
foreign import ccall "symengine/cwrapper.h &basic_free_heap" ptr_basic_free_heap_ffi :: FunPtr(Ptr CBasic -> IO ())

-- constants
foreign import ccall "symengine/cwrapper.h basic_const_zero" basic_const_zero_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_one" basic_const_one_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_minus_one" basic_const_minus_one_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_I" basic_const_I_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_pi" basic_const_pi_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_E" basic_const_E_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_const_EulerGamma" basic_const_EulerGamma_ffi :: Ptr CBasic -> IO ()
foreign import ccall "symengine/cwrapper.h basic_str" basic_str_ffi :: Ptr CBasic -> IO CString
foreign import ccall "symengine/cwrapper.h basic_eq" basic_eq_ffi :: Ptr CBasic -> Ptr CBasic -> IO Int

foreign import ccall "symengine/cwrapper.h symbol_set" symbol_set_ffi :: Ptr CBasic -> CString -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_diff" basic_diff_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h integer_set_si" integer_set_si_ffi :: Ptr CBasic -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h rational_set" rational_set_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h rational_set_si" rational_set_si_ffi :: Ptr CBasic -> CLong -> CLong -> IO ()

foreign import ccall "symengine/cwrapper.h complex_set" complex_set_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_expand" basic_expand_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_subs2" basic_subs_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_add" basic_add_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_sub" basic_sub_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_mul" basic_mul_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_div" basic_div_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_pow" basic_pow_ffi :: Ptr CBasic -> Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_neg" basic_neg_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_abs" basic_abs_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sin" basic_sin_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cos" basic_cos_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tan" basic_tan_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asin" basic_asin_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acos" basic_acos_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atan" basic_atan_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_sinh" basic_sinh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_cosh" basic_cosh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_tanh" basic_tanh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h basic_asinh" basic_asinh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_acosh" basic_acosh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h basic_atanh" basic_atanh_ffi :: Ptr CBasic -> Ptr CBasic -> IO CInt

