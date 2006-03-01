-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | This module provides default implementations of many of the associative
--   collection operations.  These function are used to fill in collection
--   implementations and are not intended to be used directly by end users.
module Data.Edison.Assoc.Defaults 
where

import Data.Maybe (fromJust)

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import Data.Edison.Assoc
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L

singletonUsingInsert :: (Assoc m k) => k -> a -> m a
singletonUsingInsert k v = insert k v empty

fromSeqUsingInsertSeq :: (AssocX m k,S.Sequence seq) => seq (k,a) -> m a
fromSeqUsingInsertSeq kvs = insertSeq kvs empty

insertSeqUsingFoldr :: 
    (AssocX m k,S.Sequence seq) => seq (k,a) -> m a -> m a
insertSeqUsingFoldr kvs m = S.foldr (uncurry insert) m kvs

unionSeqUsingReduce :: (AssocX m k,S.Sequence seq) => seq (m a) -> m a
unionSeqUsingReduce ms = S.reducel union empty ms

deleteSeqUsingFoldr :: (AssocX m k,S.Sequence seq) => seq k -> m a -> m a
deleteSeqUsingFoldr ks m = S.foldr delete m ks

memberUsingLookupM :: (AssocX m k) => k -> m a -> Bool
memberUsingLookupM k m
  = case lookupM k m of
  	Just _  -> True
	Nothing -> False

sizeUsingElements :: (AssocX m k) => m a -> Int
sizeUsingElements m = length (elements m)

countUsingMember :: AssocX m k => k -> m a -> Int
countUsingMember k m = if member k m then 1 else 0

lookupAllUsingLookupM :: (AssocX m k,S.Sequence seq) => k -> m a -> seq a
lookupAllUsingLookupM k m = case lookupM k m of
                              Just x -> S.single x
                              Nothing -> S.empty

lookupWithDefaultUsingLookupM :: AssocX m k => a -> k -> m a -> a
lookupWithDefaultUsingLookupM d k m = case lookupM k m of
                                        Just x -> x
                                        Nothing -> d

partitionUsingFilter :: AssocX m k => (a -> Bool) -> m a -> (m a,m a)
partitionUsingFilter f m = (filter f m, filter (not . f) m)

fold1UsingElements :: (AssocX m k) => (a -> a -> a) -> m a -> a
fold1UsingElements op m = L.foldr1 op (elements m)

elementsUsingFold :: (AssocX m k,S.Sequence seq) => m a -> seq a
elementsUsingFold = fold S.lcons S.empty

nullUsingElements :: (AssocX m k) => m a -> Bool
nullUsingElements m
  = case elements m of
  	[] -> True
  	_  -> False

insertWithUsingLookupM :: 
    FiniteMapX m k => (a -> a -> a) -> k -> a -> m a -> m a
insertWithUsingLookupM f k x m =
    case lookupM k m of
      Nothing -> insert k x m
      Just y  -> insert k (f x y) m

fromSeqWithUsingInsertSeqWith ::
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (k,a) -> m a
fromSeqWithUsingInsertSeqWith f kvs = insertSeqWith f kvs empty

fromSeqWithKeyUsingInsertSeqWithKey :: 
    (FiniteMapX m k,S.Sequence seq) => (k -> a -> a -> a) -> seq (k,a) -> m a
fromSeqWithKeyUsingInsertSeqWithKey f kvs = insertSeqWithKey f kvs empty

insertWithKeyUsingInsertWith :: 
    FiniteMapX m k => (k -> a -> a -> a) -> k -> a -> m a -> m a
insertWithKeyUsingInsertWith f k = insertWith (f k) k

insertSeqWithUsingInsertWith :: 
    (FiniteMapX m k,S.Sequence seq) => 
      (a -> a -> a) -> seq (k,a) -> m a -> m a
insertSeqWithUsingInsertWith f kvs m =
    S.foldr (uncurry (insertWith f)) m kvs

insertSeqWithKeyUsingInsertWithKey ::
    (FiniteMapX m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (k,a) -> m a -> m a
insertSeqWithKeyUsingInsertWithKey f kvs m =
    S.foldr (uncurry (insertWithKey f)) m kvs

unionSeqWithUsingReduce :: 
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (m a) -> m a
unionSeqWithUsingReduce f ms = S.reducel (unionWith f) empty ms

unionSeqWithUsingFoldr :: 
    (FiniteMapX m k,S.Sequence seq) => (a -> a -> a) -> seq (m a) -> m a
unionSeqWithUsingFoldr f ms = S.foldr (unionWith f) empty ms

toSeqUsingFoldWithKey :: (Assoc m k,S.Sequence seq) => m a -> seq (k,a)
toSeqUsingFoldWithKey = foldWithKey conspair S.empty
  where conspair k v kvs = S.lcons (k,v) kvs

keysUsingFoldWithKey :: (Assoc m k,S.Sequence seq) => m a -> seq k
keysUsingFoldWithKey = foldWithKey conskey S.empty
  where conskey k v ks = S.lcons k ks

unionWithUsingInsertWith :: 
    FiniteMap m k => (a -> a -> a) -> m a -> m a -> m a
unionWithUsingInsertWith f m1 m2 = foldWithKey (insertWith f) m2 m1

unionWithKeyUsingInsertWithKey :: 
    FiniteMap m k => (k -> a -> a -> a) -> m a -> m a -> m a
unionWithKeyUsingInsertWithKey f m1 m2 = foldWithKey (insertWithKey f) m2 m1

unionSeqWithKeyUsingReduce :: 
    (FiniteMap m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (m a) -> m a
unionSeqWithKeyUsingReduce f ms = S.reducel (unionWithKey f) empty ms

unionSeqWithKeyUsingFoldr :: 
    (FiniteMap m k,S.Sequence seq) => 
      (k -> a -> a -> a) -> seq (m a) -> m a
unionSeqWithKeyUsingFoldr f ms = S.foldr (unionWithKey f) empty ms

intersectWithUsingLookupM :: 
    FiniteMap m k => (a -> b -> c) -> m a -> m b -> m c
intersectWithUsingLookupM f m1 m2 = foldWithKey ins empty m1
  where ins k x m = case lookupM k m2 of
                      Nothing -> m
                      Just y  -> insert k (f x y) m

intersectWithKeyUsingLookupM :: 
    FiniteMap m k => (k -> a -> b -> c) -> m a -> m b -> m c
intersectWithKeyUsingLookupM f m1 m2 = foldWithKey ins empty m1
  where ins k x m = case lookupM k m2 of
                      Nothing -> m
                      Just y  -> insert k (f k x y) m

differenceUsingDelete :: FiniteMap m k => m a -> m b -> m a
differenceUsingDelete m1 m2 = foldWithKey del m1 m2
  where del k _ m = delete k m

subsetUsingSubsetEq :: FiniteMapX m k => m a -> m b -> Bool
subsetUsingSubsetEq m1 m2 = subsetEq m1 m2 && size m1 < size m2

subsetEqUsingMember :: FiniteMap m k => m a -> m b -> Bool
subsetEqUsingMember m1 m2 = foldWithKey mem True m1
  where mem k _ b = member k m2 && b

adjustOrInsertUsingMember :: AssocX m k => (Maybe a -> a) -> k -> m a -> m a
adjustOrInsertUsingMember f k m =
  if member k m
     then adjust (f . Just) k m
     else insert k (f Nothing) m
