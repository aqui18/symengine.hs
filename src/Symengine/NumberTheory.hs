
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Symengine.NumberTheory(
  Symengine.NumberTheory.gcd,
  Symengine.NumberTheory.lcm,
  gcdExtended,
  next_prime,
  Symengine.NumberTheory.mod,
  quotient,
  quotient_and_mod,
  mod_f,
  quotient_f,
  quotient_and_mod_f,
  mod_inverse,
  fibonacci,
  fibonacci2,
  lucas,
  binomial,
  factorial
)
where

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
import qualified Symengine.Foreign.NumberTheory as N
import           Symengine.Basic


gcd :: Basic -> Basic -> Basic
gcd = Basic (liftB2 N.ntheory_gcd_ffi)

lcm :: Basic -> Basic -> Basic
lcm = Basic (liftB2 N.ntheory_lcm_ffi)

gcdExtended :: Basic -> Basic -> (Basic, Basic, Basic)
gcdExtended (Basic a) (Basic b) = unsafePerformIO $ do
  g <- B.new
  s <- B.new
  t <- B.new

  with4 g s t a (\g s t a ->
                    with b (\b ->
                              N.ntheory_gcd_ext_ffi g s t a b))

  return (Basic g, Basic s, Basic t)
  
next_prime :: Basic -> Basic
next_prime = Basic (liftB N.ntheory_nextprime_ffi)

type Quotient = Basic
type Modulo = Basic

mod :: Basic -> Basic -> Quotient
mod = Basic (liftB2 N.ntheory_mod_ffi)

quotient :: Basic -> Basic -> Basic
quotient = liftB2 N.ntheory_quotient_ffi

quotient_and_mod :: Basic -> Basic -> (Quotient, Modulo)
quotient_and_mod a b = unsafePerformIO $ do
  quotient <- B.new
  modulo <- B.new
  with4 quotient modulo a b N.ntheory_quotient_mod_ffi
  return $  (quotient, modulo)


mod_f :: Basic -> Basic -> Quotient
mod_f = liftB2 N.ntheory_mod_f_ffi

quotient_f :: Basic -> Basic -> Basic
quotient_f = liftB2 N.ntheory_quotient_f_ffi

quotient_and_mod_f :: Basic -> Basic -> (Quotient, Modulo)
quotient_and_mod_f a b = unsafePerformIO $ do
  quotient <- B.new
  modulo <- B.new
  with4 quotient modulo a b N.ntheory_quotient_mod_f_ffi
  return $  (quotient, modulo)


mod_inverse :: Basic -> Basic -> Quotient
mod_inverse = liftB2 N.ntheory_mod_inverse_ffi


fibonacci ::  Int -> Basic
fibonacci i = Basic (unsafePerformIO $  do
  fib <- B.new
  with fib (\fib -> N.N.ntheory_fibonacci_ffi fib (fromIntegral i))
  return fib)

fibonacci2 :: Int -> (Basic, Basic)
fibonacci2 n = Basic (unsafePerformIO $ do
  g <- B.new
  s <- B.new
  
  with2 g s (\g s -> N.ntheory_fibonacci2_ffi g s (fromIntegral n))
  
  return (g, s))


lucas :: Int -> Basic
lucas n = unsafePerformIO $ do
  l <- B.new
  with l (\l -> N.ntheory_lucas_ffi l (fromIntegral n))
  return l

binomial :: Basic -> Int -> Basic
binomial n r = unsafePerformIO $ do
  ncr <- B.new
  with2 ncr n (\ncr n -> N.ntheory_binomial_ffi ncr n (fromIntegral r))
  return ncr


factorial :: Int -> Basic
factorial n = unsafePerformIO $ do
  fact <- B.new
  with fact (\fact -> N.ntheory_factorial_ffi fact (fromIntegral n))
  return fact

