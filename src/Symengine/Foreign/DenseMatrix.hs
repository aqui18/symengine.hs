{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- to write things like KnownNat(r * c) => ...
{-# LANGUAGE FlexibleContexts #-}
-- @
{-# LANGUAGE TypeApplications #-}
-- to bring stuff like (r, c) into scope
{-# LANGUAGE ScopedTypeVariables #-}

-- allow non injective type functions (+)
{-# LANGUAGE AllowAmbiguousTypes #-}

-- data declarations that are empty
{-# LANGUAGE EmptyDataDecls #-}
module Symengine.Foreign.DenseMatrix where

import Prelude
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr
import Control.Applicative
import Control.Monad -- for foldM
import System.IO.Unsafe
import Control.Monad
import GHC.Real
import Data.Proxy

import GHC.TypeLits -- type level programming
import qualified Data.Vector.Sized as V -- sized vectors
import Data.Finite -- types to represent numbers

import Symengine.Internal

foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new" cdensematrix_new_ffi :: IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h &dense_matrix_free" cdensematrix_free_ffi :: FunPtr ((Ptr CDenseMatrix) -> IO ())
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new_rows_cols" cdensematrix_new_rows_cols_ffi :: CUInt -> CUInt -> IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_new_vec" cdensematrix_new_vec_ffi :: CUInt -> CUInt -> Ptr CVecBasic -> IO (Ptr CDenseMatrix)
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_zeros" cdensematrix_zeros_ffi :: Ptr CDenseMatrix -> CULong -> CULong -> IO CInt
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_eye" cdensematrix_eye_ffi :: Ptr CDenseMatrix -> CULong -> CULong  -> CULong -> IO CInt
foreign import ccall unsafe "symengine/cwrapper.h dense_matrix_diag" cdensematrix_diag_ffi :: Ptr CDenseMatrix -> Ptr CVecBasic -> CULong  -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_eq" cdensematrix_eq_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_set" cdensematrix_set_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_str" cdensematrix_str_ffi :: Ptr CDenseMatrix -> IO CString

foreign import ccall "symengine/cwrapper.h dense_matrix_get_basic" cdensematrix_get_basic_ffi :: Ptr (CBasicSym)  -> Ptr CDenseMatrix -> CUInt -> CUInt -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_set_basic" cdensematrix_set_basic_ffi :: Ptr CDenseMatrix -> CUInt -> CUInt -> Ptr (CBasicSym)  -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_rows" cdensematrix_rows_ffi :: Ptr CDenseMatrix -> IO CULong
foreign import ccall "symengine/cwrapper.h dense_matrix_cols" cdensematrix_cols_ffi :: Ptr CDenseMatrix -> IO CULong

foreign import ccall "symengine/cwrapper.h dense_matrix_add_matrix"
  cdensematrix_add_matrix_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_mul_matrix"
  cdensematrix_mul_matrix_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_mul_scalar"
  cdensematrix_mul_scalar_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CBasicSym -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_det"
  cdensematrix_det_ffi :: Ptr CBasicSym -> Ptr CDenseMatrix -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_inv"
  cdensematrix_inv_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt


foreign import ccall "symengine/cwrapper.h dense_matrix_transpose"
  cdensematrix_transpose_ffi :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

foreign import ccall "symengine/cwrapper.h dense_matrix_LU" cdensematrix_lu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_LDL" cdensematrix_ldl :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLU" cdensematrix_fflu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_FFLDU" cdensematrix_ffldu :: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt
foreign import ccall "symengine/cwrapper.h dense_matrix_LU_solve" cdensematrix_lu_solve:: Ptr CDenseMatrix -> Ptr CDenseMatrix -> Ptr CDenseMatrix -> IO CInt

