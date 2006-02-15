module Data.Edison.Test.Driver where

import Test.HUnit (runTestTT, Test(..),assertFailure,Counts)

import Data.Edison.Test.Seq

edisonTests :: Test
edisonTests = TestList $
  [ TestLabel "sequence tests" allSequenceTests
  ]

runEdisonTests :: IO Counts
runEdisonTests = runTestTT edisonTests
