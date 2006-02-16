module Data.Edison.Test.Utils where

import Data.List (intersperse)

import Test.QuickCheck
import Test.QuickCheck.Batch
import Test.HUnit (runTestTT, Test(..),assertFailure)

-- | Turn a QuickCheck 'Testable' into an HUnit 'Test'
qcTest :: Testable a => a -> Test
qcTest x = TestCase $ do
   let testOpts = 
        TestOptions 
        { no_of_tests = 100
        , length_of_tests = 20
        , debug_tests = False
        }

   res <- run x testOpts

   case res of
     TestOk _ _ _ -> return ()

     TestExausted msg i msgs -> 
	assertFailure . concat $ ["Test time exausted: ",msg," ",show i
                                 ," ",concat (intersperse " " (concat msgs))]

     TestFailed msgs i -> 
        assertFailure . concat $ ["Falsifiable: (| ",concat (intersperse ", " msgs)," |) ",show i]

     TestAborted ex    ->
        assertFailure (show ex)
