-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Coll (
{-
    -- non-observable classes
    CollX(..),
    OrdCollX(..),
    SetX(..),
    OrdSetX(..),

    -- observable classes
    Coll(..),
    OrdColl(..),
    Set(..),
    OrdSet(..),

    -- specialize all the sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    unsafeFromOrdList,
    toList,
    lookupList,
    toOrdList,
    fromListWith,
    insertListWith,
    unionListWith,
-}
    module Data.Edison.Coll
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Prelude
import Data.Edison.Seq(Sequence)
import Data.Edison.Seq.ListSeq()

class Eq a => CollX c a | c -> a where
  empty          :: c

    -- the empty collection

  single         :: a -> c

    -- create a singleton collection

  fromSeq        :: Sequence seq => seq a -> c

    -- convert a sequence to a collection.  For sets, it is unspecified
    -- which element is kept in case of duplicates.

  insert         :: a -> c -> c
  insertSeq      :: Sequence seq => seq a -> c -> c

    -- insert an element or a sequence of elements into a collection.  For
    -- sets, insert keeps the new element in case of duplicates, but
    -- insertSeq keeps an unspecified element.

  union          :: c -> c -> c
  unionSeq       :: Sequence seq => seq c -> c

    -- merge two collections or a sequence of collections.  For sets, it
    -- is unspecified which element is kept in case of duplicates.

  delete         :: a -> c -> c
  deleteAll      :: a -> c -> c

    -- delete a single occurrence or all occurrences of the given
    -- element from a collection.  For sets, these will be the same,
    -- but for bags they may be different.  For delete on bags, it
    -- is unspecified which of several duplicate elements is deleted.

  deleteSeq      :: Sequence seq => seq a -> c -> c

    -- delete a single occurrence of each of the given elements from
    -- a collection.  For bags, there may be multiple occurrences of a
    -- given element in the collection, in which case it is unspecified
    -- which is deleted.

  null           :: c -> Bool
  size           :: c -> Int

    -- test whether the collection is empty, or return the number of
    -- elements in the collection.

  member         :: c -> a -> Bool
  count          :: c -> a -> Int

    -- test whether the given element is in the collection, or how many
    -- duplicates are in the collection.  (For sets, count will always
    -- return 0 or 1.)

  instanceName   :: c -> String

    -- the name of the module implementing c

class (CollX c a, Ord a) => OrdCollX c a | c -> a where

  deleteMin          :: c -> c
  deleteMax          :: c -> c

    -- delete the minimum or maximum element from the collection.
    -- If there is more than one minimum or maximum, it is unspecified which
    -- is deleted.

  unsafeInsertMin    :: a -> c -> c
  unsafeInsertMax    :: c -> a -> c

    -- insert an element that is guaranteed to be <= or >= any existing
    -- elements in the collection.  (For sets, this precondition is
    -- strengthened to < or >.)

  unsafeFromOrdSeq   :: Sequence seq => seq a -> c

    -- convert a sequence in non-decreasing order into a collection.
    -- (For sets, the sequence must be in increasing order.)

  unsafeAppend       :: c -> c -> c

    -- union two collections where every element in the first
    -- collection is <= every element in the second collection.
    -- (For sets, this precondition is strengthened to <.)

  filterLT           :: a -> c -> c
  filterLE           :: a -> c -> c
  filterGT           :: a -> c -> c
  filterGE           :: a -> c -> c

    -- filterLT x xs = filter (< x) xs
    -- filterLE x xs = filter (<= x) xs
    -- filterGT x xs = filter (> x) xs
    -- filterGE x xs = filter (>= x) xs

  partitionLT_GE     :: a -> c -> (c, c)
  partitionLE_GT     :: a -> c -> (c, c)
  partitionLT_GT     :: a -> c -> (c, c)

    -- partitionLT_GE x xs = partition (< x) xs
    -- partitionLE_GT x xs = partition (<= x) xs
    -- partitionLT_GT x xs = (filterLT x xs, filterGT x xs)

class CollX c a => SetX c a | c -> a where

  intersect   :: c -> c -> c
  difference  :: c -> c -> c

    -- return the intersection or difference of two sets.  For intersect,
    -- it is unspecified which of the two elements is kept.

  subset      :: c -> c -> Bool    
  subsetEq    :: c -> c -> Bool

    -- test whether the first set is a proper subset of the second,
    -- or whether it is a (possibly improper) subset.

class (OrdCollX c a, SetX c a) => OrdSetX c a | c -> a
  -- no methods


class CollX c a => Coll c a | c -> a where
  toSeq      :: Sequence seq => c -> seq a

    -- list the elements of the collection in an unspecified order

  lookup     :: c -> a -> a
  lookupM    :: (Monad m) => c -> a -> m a
  lookupAll  :: Sequence seq => c -> a -> seq a
  lookupWithDefault  :: a -> c -> a -> a

    -- lookup one or more elements equal to the given element.
    -- if there is none, then lookup signals an error, lookupM returns 
    -- Nothing, lookupAll returns empty, and lookupWithDefault d returns d.
    -- if there are mulitiple copies, then lookup/lookupM/lookupWithDefault
    -- return an unspecified one, and lookupAll returns them all, but
    -- in an unspecified order.

  fold       :: (a -> b -> b) -> b -> c -> b
  fold1      :: (a -> a -> a) -> c -> a

    -- fold over all the elements in a collection in unspecified order.
    -- (fold1 signals an error if the collection is empty.)

  filter     :: (a -> Bool) -> c -> c
  partition  :: (a -> Bool) -> c -> (c, c)

    -- filter removes all elements not satisfying the predicate.
    -- partition returns two collections, one containing all the
    -- elements satisfying the predicate, and one containing all the
    -- elements not satisfying the predicate.

class (Coll c a, OrdCollX c a) => OrdColl c a | c -> a where

  minView    :: (Monad m) => c -> m (a, c)
  minElem    :: c -> a

    -- return the minimum element in the collection, together with
    -- the collection without that element in the case of minView.
    -- If there are multiple copies of the minimum element, it is
    -- unspecified which is chosen.  Note that minView, minElem, and
    -- deleteMin may make different choices!

  maxView    :: (Monad m) => c -> m (c, a)
  maxElem    :: c -> a

    -- return the maximum element in the collection, together with
    -- the collection without that element in the case of maxView.
    -- If there are multiple copies of the maximum element, it is
    -- unspecified which is chosen.  Note that maxView, maxElem, and
    -- deleteMax may make different choices!

  foldr      :: (a -> b -> b) -> b -> c -> b
  foldl      :: (b -> a -> b) -> b -> c -> b

    -- fold across the elements in non-decreasing order.
    -- (For sets, this will always be increasing order.)

  foldr1     :: (a -> a -> a) -> c -> a
  foldl1     :: (a -> a -> a) -> c -> a

    -- fold across the elements in non-decreasing order, or signal an
    -- error if the collection is empty.  (For sets, this will always be 
    -- increasing order.)

  toOrdSeq   :: Sequence seq => c -> seq a

    -- list the elements in non-decreasing order.

class (Coll c a, SetX c a) => Set c a | c -> a where

  -- WARNING: Each of the following "With" functions is unsafe.  The combining
  -- functions are required to satisfy the precondition that, given two
  -- equal elements, they return a third element equal to the other two.

  fromSeqWith     :: Sequence seq => (a -> a -> a) -> seq a -> c

    -- same as fromSeq but with a combining function to resolve duplicates.
    -- Usually, the combining function should be associative.  If not,
    -- the elements will be combined left-to-right, but with an
    -- unspecified associativity.  For example, if x == y == z,
    -- then fromSeqWith (+) [x,y,z] equals either
    --     single (x + (y + z))
    -- or
    --     single ((x + y) + z)

  insertWith      :: (a -> a -> a) -> a -> c -> c
  insertSeqWith   :: Sequence seq => (a -> a -> a) -> seq a -> c -> c

    -- same as insert/insertSeq but with a combining function to resolve 
    -- duplicates.  The comments about associativity apply to insertSeqWith.

  unionl          :: c -> c -> c
  unionr          :: c -> c -> c

    -- unionl = unionWith (\x y -> x)
    -- unionr = unionWith (\x y -> y)
    
  unionWith       :: (a -> a -> a) -> c -> c -> c
  unionSeqWith    :: Sequence seq => (a -> a -> a) -> seq (c) -> c

    -- same as union/unionSeq but with a combining function to resolve
    -- duplicates.  The comments about associativity apply to unionSeqWith.

  intersectWith   :: (a -> a -> a) -> c -> c -> c

    -- same as intersect but with a combining function to resolve duplicates.

class (OrdColl c a, Set c a) => OrdSet c a | c -> a
  -- no methods


-- specialize all the sequence operations to lists

fromList          :: CollX c a => [a] -> c
insertList        :: CollX c a => [a] -> c -> c
unionList         :: CollX c a => [c] -> c
deleteList        :: CollX c a => [a] -> c -> c
unsafeFromOrdList :: OrdCollX c a => [a] -> c
toList            :: Coll c a => c -> [a]
lookupList        :: Coll c a => c -> a -> [a]
toOrdList         :: OrdColl c a => c -> [a]
fromListWith      :: Set c a => (a -> a -> a) -> [a] -> c
insertListWith    :: Set c a => (a -> a -> a) -> [a] -> c -> c
unionListWith     :: Set c a => (a -> a -> a) -> [c] -> c

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
unsafeFromOrdList = unsafeFromOrdSeq
toList = toSeq
lookupList = lookupAll
toOrdList = toOrdSeq
fromListWith = fromSeqWith
insertListWith = insertSeqWith
unionListWith = unionSeqWith

