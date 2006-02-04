-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module EdisonPrelude where


class Eq a => Hash a where
  hash :: a -> Int
  -- forall x,y :: a. (x == y) implies (hash x == hash y)

class Hash a => UniqueHash a
  -- no new methods, just a stronger invariant
  -- forall x,y :: a. (x == y) iff (hash x == hash y)

class UniqueHash a => ReversibleHash a where
  unhash :: Int -> a
  -- forall x :: a. unhash (hash x) == x

  -- Note that 
  --   hash (unhash i) == i
  -- does not necessarily hold because unhash is not necessarily
  -- defined for all i, only for all i in the range of hash.


-- Enum types
--

instance (Eq a, Enum a) => Hash a where
  hash = fromEnum

instance (Eq a, Enum a) => UniqueHash a where

instance (Eq a, Enum a) => ReversibleHash a where
  unhash = toEnum

-- vi: set ts=8 sw=4 ai:
-- vim: set ts=8 sw=4 ai:
