-- |
--   Module      :  Data.Edison.Coll.Utils
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   This module provides implementations of several useful operations
--   that are not included in the collection classes themselves.  This is
--   usually because the operation involves transforming a collection into a
--   different type of collection; such operations cannot be typed using
--   the collection classes without significantly complicating them.
--
--   Be aware that these functions are defined using the external class
--   interfaces and may be less efficient than corresponding, but more
--   restrictively typed, functions in the collection classes.

module Data.Edison.Coll.Utils where

import Prelude hiding (map,null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Coll


-- | Apply a function across all the elements in a collection and transform
--   the collection type.
map :: (Coll cin a, CollX cout b) => (a -> b) -> (cin -> cout)
map f xs = fold (\x ys -> insert (f x) ys) empty xs


-- | Map a partial function across all elements of a collection and transform
--   the collection type.
mapPartial :: (Coll cin a, CollX cout b) => (a -> Maybe b) -> (cin -> cout)
mapPartial f xs = fold (\ x ys -> case f x of
                                    Just y -> insert y ys
                                    Nothing -> ys)
                       empty xs


-- | Map a monotonic function across all the elements of a collection and
--   transform the collection type.   The function is required to satisfy
--   the following precondition:
--
-- > forall x y. x < y ==> f x < f y
unsafeMapMonotonic :: (OrdColl cin a, OrdCollX cout b) => (a -> b) -> (cin -> cout)
unsafeMapMonotonic f xs = foldr (unsafeInsertMin . f) empty xs


-- | Map a collection-producing function across all elements of a collection
--   and collect the results together using 'union'.
unionMap :: (Coll cin a, CollX cout b) => (a -> cout) -> (cin -> cout)
unionMap f xs = fold (\x ys -> union (f x) ys) empty xs
