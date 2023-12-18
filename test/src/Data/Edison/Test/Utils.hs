-- Copyright (c) 2006 Robert Dockins
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.Utils where

import Data.List (intersperse)

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.HUnit (runTestTT, Test(..),assertFailure)

-- | Turn a QuickCheck 'Testable' into an HUnit 'Test'
qcTest :: Testable a => a -> Test
qcTest x = TestCase $ do
   let args =
        stdArgs
        { maxSuccess = 100
        , maxSize = 20
        }

   res <- quickCheckWithResult args x

   case res of
     Success{} -> return ()

     GaveUp{ numTests = i, output = msg } ->
        assertFailure . concat $ ["Test time exhausted: ",msg," ",show i]

     Failure{ numTests = i, reason = r, output = msg } ->
        assertFailure . concat $ [r, " ", msg, " ", show i]

     NoExpectedFailure{ output = msg } ->
        assertFailure msg
