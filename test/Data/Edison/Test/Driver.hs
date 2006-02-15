module Data.Edison.Test.Driver where

import Test.HUnit (Test(..))

import Data.Edison.Test.Seq
import Data.Edison.Test.Bag

edisonTests :: Test
edisonTests = TestList $
  [ TestLabel "bag tests"      allBagTests
  , TestLabel "sequence tests" allSequenceTests
  ]
