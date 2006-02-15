module Data.Edison.Test.Driver where

import Test.HUnit (Test(..))

import Data.Edison.Test.Seq

edisonTests :: Test
edisonTests = TestList $
  [ TestLabel "sequence tests" allSequenceTests
  ]
