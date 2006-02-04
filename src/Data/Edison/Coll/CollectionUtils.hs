-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Coll.CollectionUtils where

import Prelude hiding (map,null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Coll.Collection

map :: (Coll cin a, CollX cout b) => (a -> b) -> (cin -> cout)
map f xs = fold (\x ys -> insert (f x) ys) empty xs

mapPartial :: (Coll cin a, CollX cout b) => (a -> Maybe b) -> (cin -> cout)
mapPartial f xs = fold (\ x ys -> case f x of
                                    Just y -> insert y ys
                                    Nothing -> ys)
                       empty xs

unsafeMapMonotonic :: (OrdColl cin a, OrdCollX cout b) => (a -> b) -> (cin -> cout)
unsafeMapMonotonic f xs = foldr (unsafeInsertMin . f) empty xs

unionMap :: (Coll cin a, CollX cout b) => (a -> cout) -> (cin -> cout)
unionMap f xs = fold (\x ys -> union (f x) ys) empty xs

