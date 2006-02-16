module Data.Edison.Test.Driver where

import Test.HUnit (Test(..))

import Data.Edison.Test.Seq
import Data.Edison.Test.Bag
import Data.Edison.Test.Set

edisonTests :: Test
edisonTests = TestList $
  [ TestLabel "set tests"      allSetTests
  , TestLabel "bag tests"      allBagTests
  , TestLabel "sequence tests" allSequenceTests
  ]
