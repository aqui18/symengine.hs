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
module Symengine.Foreign.setBasic
  (
    setBasic,
    setbasic_new,
    setbasic_push_back,
    setbasic_get,
    setbasic_size,
    settor_to_setbasic,
   )
where


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
import Symengine

import GHC.TypeLits -- type level programming
import qualified Data.settor.Sized as V -- sized settors

import Symengine.Internal
import Symengine.BasicSym


newtype SetBasic = SetBasic (ForeignPtr CSetBasic)

instance Wrapped setBasic CsetBasic where
    with (setBasic p) f = withForeignPtr p f


-- | get the i'th element out of a setbasic
setbasic_get :: SetBasic -> Int -> Either SymengineException BasicSym
setbasic_get set i =
  if i >= 0 && i < setbasic_size set
  then
    unsafePerformIO $ do
    sym <- basicsym_new
    exception <- cIntToEnum <$> with2 set sym (\v s -> setbasic_get_ffi v i s)
    case exception of
      NoException -> return (Right sym)
      _ -> return (Left exception)
  else
    Left RuntimeError


-- | Create a new setBasic
setbasic_new :: IO SetBasic
setbasic_new = do
    ptr <- setbasic_new_ffi
    finalized <- newForeignPtr setbasic_free_ffi ptr
    return $ setBasic (finalized)

setbasic_size :: SetBasic -> Int
setbasic_size set = unsafePerformIO $
  fromIntegral <$> with set setbasic_size_ffi

foreign import ccall "symengine/cwrapper.h setbasic_new" setbasic_new_ffi :: IO (Ptr CsetBasic)
foreign import ccall "symengine/cwrapper.h setbasic_get" setbasic_get_ffi :: Ptr CsetBasic -> Int -> Ptr CBasicSym -> IO CInt
foreign import ccall "symengine/cwrapper.h setbasic_size" setbasic_size_ffi :: Ptr CsetBasic -> IO CSize
foreign import ccall "symengine/cwrapper.h &setbasic_free" setbasic_free_ffi :: FunPtr (Ptr CsetBasic -> IO ())

