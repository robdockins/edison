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
     Success _ _ _ -> return ()

     GaveUp i _ msg -> 
	assertFailure . concat $ ["Test time exausted: ",msg," ",show i]

     Failure i _ _ _ r _ msg -> 
        assertFailure . concat $ [r, " ", msg, " ", show i]

     NoExpectedFailure _ _ msg ->
        assertFailure msg
