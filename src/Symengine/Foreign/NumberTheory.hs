module Symengine.Foreign.NumberTheory where

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

foreign import ccall "symengine/cwrapper.h ntheory_gcd" ntheory_gcd_ffi :: 
  Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_lcm" ntheory_lcm_ffi :: 
  Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_gcd_ext" ntheory_gcd_ext_ffi 
  :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> 
      Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_nextprime"
  ntheory_nextprime_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_mod"
  ntheory_mod_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_quotient"
  ntheory_quotient_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_quotient_mod"
  ntheory_quotient_mod_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> 
                          Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_mod_f"
  ntheory_mod_f_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_quotient_f"
  ntheory_quotient_f_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_quotient_mod_f"
  ntheory_quotient_mod_f_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> 
                          Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_mod_inverse"
  ntheory_mod_inverse_ffi :: Ptr B.CBasic -> Ptr B.CBasic -> Ptr B.CBasic -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_fibonacci"
  ntheory_fibonacci_ffi :: Ptr B.CBasic -> 
    CULong -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_fibonacci2"
  ntheory_fibonacci2_ffi :: Ptr B.CBasic -> Ptr B.CBasic ->  
    CULong -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_lucas"
  ntheory_lucas_ffi :: Ptr B.CBasic ->
    CULong -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_lucas2"
  ntheory_lucas2_ffi :: Ptr B.CBasic -> Ptr B.CBasic ->  
     CULong -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_binomial"
  ntheory_binomial_ffi :: Ptr B.CBasic -> Ptr B.CBasic ->  
    CULong -> IO CInt

foreign import ccall "symengine/cwrapper.h ntheory_factorial"
  ntheory_factorial_ffi :: Ptr B.CBasic -> 
    CULong -> IO CInt
