{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
-- Copyright (c) 2006 Robert Dockins
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.Utils where

import Data.List (intersperse)

import Test.QuickCheck
import Test.QuickCheck.Test
import Test.HUnit (runTestTT, Test(..),assertFailure)
import GHC.Stack (HasCallStack)
import Data.Edison.Test (InternalTest(..))

-- Inserting InternalTest in the right places in all properties is a lot
-- of work. So we do this hack instead, which amounts to inserting
-- InternalTest everywhere and then dispatch the types that don't need it
-- with some overlappable instances.
class ITestable a where
  iproperty :: a -> Property

instance ITestable Bool where
  iproperty = property

instance ITestable Property where
  iproperty = id

instance (Arbitrary (InternalTest a), Show (InternalTest a), ITestable b) => ITestable (a -> b) where
  iproperty f = forAll arbitrary (\(InternalTest x) -> iproperty (f x))

instance {-# OVERLAPPABLE #-} Arbitrary a => Arbitrary (InternalTest a) where
  arbitrary = InternalTest <$> arbitrary
  shrink (InternalTest x) = InternalTest <$> shrink x

instance {-# OVERLAPPABLE #-} Show a => Show (InternalTest a) where
  showsPrec n (InternalTest x) = showsPrec n x

-- | Turn a QuickCheck 'Testable' into an HUnit 'Test'
qcTest :: (ITestable a, HasCallStack) => a -> Test
qcTest x = TestCase $ do
   let args =
        stdArgs
        { maxSuccess = 1000
        , maxSize = 20
        , chatty = False
        }

   res <- quickCheckWithResult args (iproperty x)

   case res of
     Success{} -> return ()

     GaveUp{ numTests = i, output = msg } ->
        assertFailure . concat $ ["Test time exausted: ",msg," ",show i]

     Failure{ numTests = i, reason = r, output = msg } ->
        assertFailure . concat $ [r, " ", msg, " ", show i]

     NoExpectedFailure{ output = msg } ->
        assertFailure msg
