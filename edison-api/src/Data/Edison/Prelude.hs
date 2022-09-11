-- |
--   Module      :  Data.Edison.Prelude
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   This module is a central depository of common definitions
--   used throughout Edison.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Edison.Prelude (
-- * Hashing classes
  Hash (..)
, UniqueHash
, ReversibleHash (..)
, Measured (..)
-- * Pure MonadFail
,  runFail_
) where

import Control.Monad.Fail
import Data.Monoid

-- | This class represents hashable objects. If obeys the 
--   following invariant:
--
-- @forall x,y :: a. (x == y) implies (hash x == hash y)@

class Eq a => Hash a where
  hash :: a -> Int


-- | This class represents hashable objects where the hash function
--   is /unique/ (injective).  There are no new methods, just a 
--   stronger invariant:
--
-- @forall x,y :: a. (x == y) iff (hash x == hash y)@

class Hash a => UniqueHash a


-- | This class represents hashable objects where the hash is
--   reversible.
--
-- @forall x :: a. unhash (hash x) == x@
--
--  Note that:
--
-- @hash (unhash i) == i@
--
-- does not necessarily hold because 'unhash' is not necessarily
-- defined for all @i@, only for all @i@ in the range of hash.

class UniqueHash a => ReversibleHash a where
  unhash :: Int -> a


-- | This class represents a quantity that can be measured.  It is
--   calculated by an associative function with a unit (hence the
--   @Monoid@ superclass, and by a function which gives the measurement
--   for an individual item.  Some datastructures are able to speed up
--   the calculation of a measure by caching intermediate values of
--   the computation.
class (Monoid v) => Measured v a | a -> v where
  measure :: a -> v

-- From Agda source code: src/full/Agda/Utils/Fail.hs
-- | A pure MonadFail.
newtype Fail a = Fail { runFail :: Either String a }
  deriving (Functor, Applicative, Monad)

instance MonadFail Fail where
  fail = Fail . Left

runFail_ :: Fail a -> a
runFail_ = either error id . runFail
