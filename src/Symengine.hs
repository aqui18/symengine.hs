{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Symengine
Description : Symengine bindings to Haskell
-}
module Symengine
  (
  module Symengine.BasicSym,
  module Symengine.DenseMatrix,
  module Symengine.NumberTheory
  ) where

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
import GHC.Real --TODO clean imports

import Symengine.BasicSym
import Symengine.DenseMatrix
import Symengine.NumberTheory

ascii_art_str :: IO String
ascii_art_str = ascii_art_str_ffi >>= peekCString

foreign import ccall "symengine/cwrapper.h ascii_art_str" ascii_art_str_ffi :: IO CString

