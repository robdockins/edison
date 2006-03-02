-- |
--   Module      :  Data.Edison.Prelude
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   This module is a central depository of common definitions
--   used throughout Edison.

module Data.Edison.Prelude (
-- * Hashing classes
  Hash (..)
, UniqueHash
, ReversibleHash (..)
) where


-- | This class represents hashable objects. If obeys the 
--   following invariant:
--
-- @forall x,y :: a. (x == y) implies (hash x == hash y)@

class Eq a => Hash a where
  hash :: a -> Int


-- | This class represents hashable objects where the hash funcion
--   is /unique/ (injective).  There are no new methods, just a 
--   stronger invariant:
--
-- @forall x,y :: a. (x == y) iff (hash x == hash y)@

class Hash a => UniqueHash a


-- | This class represents hashable objects where the hash is
--   reversable.
--
-- @forall x :: a. unhash (hash x) == x@
--
--  Note that:
--
-- @hash (unhash i) == i@
--
-- does not necessarily hold because unhash is not necessarily
-- defined for all @i@, only for all @i@ in the range of hash.

class UniqueHash a => ReversibleHash a where
  unhash :: Int -> a
