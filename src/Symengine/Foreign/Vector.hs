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
module Symengine.Foreign.Vector (
    Vector,
    new,
    pushBack,
    get,
    size,
    toVector,
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
import Symengine

import GHC.TypeLits 
import qualified Data.Vector.Sized as V

import           Symengine.Foreign.Internal
import qualified Symengine.Foreign.Basic

-- |represents a symbol exported by SymEngine. create this using the functions
-- 'zero', 'one', 'minus_one', 'e', 'im', 'rational', 'complex', and also by
-- constructing a number and converting it to a Symbol
-- 
-- >>> 3.5 :: B.Basic
-- 7/2
--
-- >>> rational 2 10
-- 1 /5
--
-- >>> complex 1 2
-- 1 + 2*I
-- vectors binding

-- | Represents a Vector of B.Basic
-- | usually, end-users are not expected to interact directly with Vector
-- | this should at some point be moved to Symengine.Internal

data CVector
data Vector = Vector !(ForeignPtr CVector)

instance Wrapped Vector CVector where
    with (Vector p) = withForeignPtr p

-- | push back an element into a vector
pushBack :: Vector -> B.Basic -> IO ()
pushBack vec sym =  with2 vec sym vecbasic_push_back_ffi v p

-- | get the i'th element out of a vecbasic
get :: Vector -> Int -> Either SymengineException B.Basic
get vec i 
  | i >= 0 && i < vecbasic_size vec = 
      unsafePerformIO $ do
          sym <- basicsym_new
          exception <- cIntToEnum <$> with2 vec sym (\v s -> vecbasic_get_ffi v i s)
          case exception of
            NoException -> return (Right sym)
            _ -> return (Left exception)

  | otherwise = Left RuntimeError

-- | Create a new Vector
new :: IO Vector
new = do
    ptr <- vecbasic_new_ffi
    finalized <- newForeignPtr vecbasic_free_ffi ptr
    return $ Vector finalized

toVector :: forall n. KnownNat n => V.Vector n B.Basic -> IO Vector
toVector syms = do
  ptr <- vecbasic_new_ffi
  forM_ syms (\sym -> with sym vecbasic_push_back_ffi ptr)
  finalized <- newForeignPtr vecbasic_free_ffi ptr
  return $ Vector finalized

size :: Vector -> Int
size vec = unsafePerformIO $
  fromIntegral <$> with vec vecbasic_size_ffi

foreign import ccall "symengine/cwrapper.h vecbasic_new" vecbasic_new_ffi :: IO (Ptr CVector)
foreign import ccall "symengine/cwrapper.h vecbasic_push_back" vecbasic_push_back_ffi :: Ptr CVector -> Ptr CB.Basic -> IO ()
foreign import ccall "symengine/cwrapper.h vecbasic_get" vecbasic_get_ffi :: Ptr CVector -> Int -> Ptr CB.Basic -> IO CInt
foreign import ccall "symengine/cwrapper.h vecbasic_size" vecbasic_size_ffi :: Ptr CVector -> IO CSize
foreign import ccall "symengine/cwrapper.h &vecbasic_free" vecbasic_free_ffi :: FunPtr (Ptr CVector -> IO ())

