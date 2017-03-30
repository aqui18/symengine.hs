{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
-- @
{-# LANGUAGE TypeApplications #-}
-- to bring stuff like (r, c) into scope
{-# LANGUAGE ScopedTypeVariables #-}
module Symengine.Foreign.Set where

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

import GHC.TypeLits -- type level programming

import           Symengine.Foreign.Internal
import qualified Symengine.Foreign.Basic as B

data CSet
data Set = Set !(ForeignPtr CSet)

instance Wrapped Set CSet where
    with (Set p) = withForeignPtr p

-- | get the i'th element out of a Set
get :: Set -> Int -> Either SymengineException B.Basic
get set i 
  | i >= 0 && (i < size set) = 
      unsafePerformIO $ do
          sym <- B.new
          with2 set sym (\v s -> setbasic_get_ffi v (intToCInt i) s)
          return (Right sym)
  | otherwise =  Left RuntimeError

-- | Create a new Set
new :: IO Set
new = do
    ptr <- setbasic_new_ffi
    finalized <- newForeignPtr setbasic_free_ffi ptr
    return (Set finalized)

size :: Set -> Int
size set = unsafePerformIO $
  fromIntegral <$> with set setbasic_size_ffi

foreign import ccall "symengine/cwrapper.h basic_free_symbols" basic_free_symbols_ffi :: Ptr B.CBasic -> Ptr CSet -> IO CInt
foreign import ccall "symengine/cwrapper.h setbasic_new" setbasic_new_ffi :: IO (Ptr CSet)
foreign import ccall "symengine/cwrapper.h setbasic_get" setbasic_get_ffi :: Ptr CSet -> CInt -> Ptr B.CBasic -> IO CInt
foreign import ccall "symengine/cwrapper.h setbasic_size" setbasic_size_ffi :: Ptr CSet -> IO CSize
foreign import ccall "symengine/cwrapper.h &setbasic_free" setbasic_free_ffi :: FunPtr (Ptr CSet -> IO ())

