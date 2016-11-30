import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Data.List
import Data.Ord
import Data.Monoid

import Symengine as Sym
import Prelude hiding (pi)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, vectorTests, denseMatrixTests]


-- These are used to check invariants that can be tested by creating
-- random members of the type and then checking invariants on them

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

unitTests = testGroup "Unit tests"
  [ HU.testCase "FFI Sanity Check - ASCII Art should be non-empty" $
    do
      ascii_art <- Sym.ascii_art_str
      HU.assertBool "ASCII art from ascii_art_str is empty" (not . null $ ascii_art)
    , HU.testCase "Basic Constructors" $
    do
      "0" @?= (show zero)
      "1" @?= (show one)
      "-1" @?= (show minus_one)
    , HU.testCase "Basic Trignometric Functions" $
    do
      let pi_over_3 = pi / 3 :: BasicSym
      let pi_over_2 = pi / 2 :: BasicSym

      sin zero @?= zero
      cos zero @?= one

      sin (pi / 6) @?= 1 / 2
      sin (pi / 3) @?= (3 ** (1/2)) / 2

      cos (pi / 6) @?= (3 ** (1/2)) / 2
      cos (pi / 3) @?= 1 / 2

      sin pi_over_2 @?= one
      cos pi_over_2 @?= zero

  ]
-- tests for vectors
vectorTests = testGroup "Vector"
    [ HU.testCase "Vector - create, push_back, get out value" $
      do
        v <- vecbasic_new
        vecbasic_push_back v (11 :: BasicSym)
        vecbasic_push_back v (12 :: BasicSym)

        vecbasic_get v 0 @?= Right (11 :: BasicSym)
        vecbasic_get v 1 @?= Right (12 :: BasicSym)
        vecbasic_get v 101 @?= Left RuntimeError
    ]


-- tests for dense matrices
denseMatrixTests = testGroup "Dense Matrix"
  [ HU.testCase "Create matrix and display" $
    do
      let syms = [one, one, one, zero]
      mat <- densematrix_new_vec 2 2 syms
      show mat @?= "[1, 1]\n[1, 0]\n"
    , HU.testCase "test get for matrix" $
        do
          let syms = [1, 2, 3, 4]
          mat <- densematrix_new_vec 2 2 syms
          densematrix_get mat 0 0  @?= 1
          densematrix_get mat 0 1  @?= 2
          densematrix_get mat 1 0  @?= 3
          densematrix_get mat 1 1  @?= 4
  ]
