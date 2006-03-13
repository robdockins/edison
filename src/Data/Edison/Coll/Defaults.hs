-- |
--   Module      :  Data.Edison.Coll.Defaults
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  internal (unstable)
--   Portability :  non-portable (MPTC and FD)
--
--   This module provides default implementations of many of the collection methods.  The functions
--   in this module are used to fill out collection implementations and are not intended to be
--   used directly by end users.

module Data.Edison.Coll.Defaults where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Control.Monad.Identity

import Data.Edison.Coll
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L

insertSeqUsingUnion :: (CollX c a,S.Sequence seq) => seq a -> c -> c
insertSeqUsingUnion xs c = union (fromSeq xs) c

insertSeqUsingFoldr :: (CollX c a,S.Sequence seq) => seq a -> c -> c
insertSeqUsingFoldr xs c = S.foldr insert c xs

memberUsingFold :: Coll c a => c -> a -> Bool
memberUsingFold h x = fold (\y ans -> (x == y) || ans) False h

countUsingMember :: SetX c a => a -> c -> Int
countUsingMember x xs = if member x xs then 1 else 0

lookupAllUsingLookupM :: (Set c a,S.Sequence seq) => a -> c -> seq a
lookupAllUsingLookupM x xs =
  case lookupM x xs of
    Nothing -> S.empty
    Just y  -> S.singleton y

deleteSeqUsingDelete :: (CollX c a,S.Sequence seq) => seq a -> c -> c
deleteSeqUsingDelete xs c = S.foldr delete c xs

unionSeqUsingFoldl :: (CollX c a,S.Sequence seq) => seq c -> c
unionSeqUsingFoldl = S.foldl union empty

unionSeqUsingReduce :: (CollX c a,S.Sequence seq) => seq c -> c
unionSeqUsingReduce = S.reducel union empty

fromSeqUsingFoldr :: (CollX c a,S.Sequence seq) => seq a -> c
fromSeqUsingFoldr = S.foldr insert empty

fromSeqUsingUnionSeq :: (CollX c a,S.Sequence seq) => seq a -> c
fromSeqUsingUnionSeq = unionList . S.foldl singleCons []
  where singleCons xs x = S.lcons (singleton x) xs

toSeqUsingFold :: (Coll c a,S.Sequence seq) => c -> seq a
toSeqUsingFold = fold S.lcons S.empty

unsafeInsertMaxUsingUnsafeAppend :: OrdCollX c a => a -> c -> c
unsafeInsertMaxUsingUnsafeAppend x c = unsafeAppend c (singleton x)

toOrdSeqUsingFoldr :: (OrdColl c a,S.Sequence seq) => c -> seq a
toOrdSeqUsingFoldr = foldr S.lcons S.empty

unsafeFromOrdSeqUsingUnsafeInsertMin :: 
    (OrdCollX c a,S.Sequence seq) => seq a -> c
unsafeFromOrdSeqUsingUnsafeInsertMin = S.foldr unsafeInsertMin empty

disjointUsingToOrdList :: OrdColl c a => c -> c -> Bool
disjointUsingToOrdList xs ys = disj (toOrdList xs) (toOrdList ys)
  where disj a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> disj xs b
            EQ -> False
            GT -> disj a ys
        disj _ _ = True

intersectWitnessUsingToOrdList ::
	(OrdColl c a, Monad m) => c -> c -> m (a,a)
intersectWitnessUsingToOrdList xs ys = witness (toOrdList xs) (toOrdList ys)
  where witness a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> witness xs b
            EQ -> return (x, y)
            GT -> witness a ys
	-- XXX
        witness _ _ = fail $ instanceName xs ++ ".intersect: failed"

lookupUsingLookupM :: Coll c a => a -> c -> a
lookupUsingLookupM x ys = runIdentity (lookupM x ys)

lookupUsingLookupAll :: Coll c a => a -> c -> a
lookupUsingLookupAll x ys =
  case lookupAll x ys of
    (y:_) -> y
    [] -> error $ instanceName ys ++ ".lookup: lookup failed"

lookupMUsingLookupAll :: (Coll c a, Monad m) => a -> c -> m a
lookupMUsingLookupAll x ys =
  case lookupAll x ys of
    (y:_) -> return y
    []    -> fail $ instanceName ys ++ ".lookupM: lookup failed"

lookupWithDefaultUsingLookupAll :: Coll c a => a -> a -> c -> a
lookupWithDefaultUsingLookupAll dflt x ys =
  case lookupAll x ys of
    (y:_) -> y
    [] -> dflt

lookupWithDefaultUsingLookupM :: Coll c a => a -> a -> c -> a
lookupWithDefaultUsingLookupM dflt x ys =
  case lookupM x ys of
    Just y  -> y
    Nothing -> dflt

deleteMaxUsingMaxView :: OrdColl c a => c -> c
deleteMaxUsingMaxView c =
  case maxView c of
    Just (_,c') -> c'
    Nothing     -> c

fromSeqWithUsingInsertWith :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq a -> c
fromSeqWithUsingInsertWith c = S.foldr (insertWith c) empty

insertUsingInsertWith :: Set c a => a -> c -> c
insertUsingInsertWith = insertWith (\x y -> x)

unionUsingUnionWith :: Set c a => c -> c -> c
unionUsingUnionWith = unionWith (\x y -> x)

filterUsingOrdLists :: OrdColl c a => (a -> Bool) -> c -> c
filterUsingOrdLists p = unsafeFromOrdList . L.filter p . toOrdList

partitionUsingOrdLists :: OrdColl c a => (a -> Bool) -> c -> (c,c)
partitionUsingOrdLists p xs = (unsafeFromOrdList ys,unsafeFromOrdList zs)
  where (ys,zs) = L.partition p (toOrdList xs)

intersectionUsingIntersectionWith :: Set c a => c -> c -> c
intersectionUsingIntersectionWith = intersectionWith (\x y -> x)

differenceUsingOrdLists :: OrdSet c a => c -> c -> c
differenceUsingOrdLists xs ys = unsafeFromOrdList (diff (toOrdList xs) (toOrdList ys))
  where diff a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : diff xs b
            EQ -> diff xs ys
            GT -> diff a ys
        diff a _ = a

properSubsetUsingOrdLists :: OrdSet c a => c -> c -> Bool
properSubsetUsingOrdLists xs ys = properSubsetOnOrdLists (toOrdList xs) (toOrdList ys)

subsetUsingOrdLists :: OrdSet c a => c -> c -> Bool
subsetUsingOrdLists xs ys = subsetOnOrdLists (toOrdList xs) (toOrdList ys)

properSubsetOnOrdLists [] [] = False
properSubsetOnOrdLists [] (_:_) = True
properSubsetOnOrdLists (_:_) [] = False
properSubsetOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> properSubsetOnOrdLists xs ys
    GT -> subsetOnOrdLists a ys

subsetOnOrdLists [] _ = True
subsetOnOrdLists (_:_) [] = False
subsetOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> subsetOnOrdLists xs ys
    GT -> subsetOnOrdLists a ys

insertSeqWithUsingInsertWith :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq a -> c -> c
insertSeqWithUsingInsertWith c xs s = S.foldr (insertWith c) s xs

unionlUsingUnionWith :: Set c a => c -> c -> c
unionlUsingUnionWith xs ys = unionWith (\x y -> x) xs ys

unionrUsingUnionWith :: Set c a => c -> c -> c
unionrUsingUnionWith xs ys = unionWith (\x y -> y) xs ys

unionWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c -> c -> c
unionWithUsingOrdLists c xs ys = unsafeFromOrdList (merge (toOrdList xs) (toOrdList ys))
  where merge a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : merge xs b
            EQ -> c x y : merge xs ys
            GT -> y : merge a ys
        merge a@(x:xs) [] = a
        merge [] b = b

unionSeqWithUsingReducer :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq c -> c
unionSeqWithUsingReducer c = S.reducer (unionWith c) empty

intersectionWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c -> c -> c
intersectionWithUsingOrdLists c xs ys = unsafeFromOrdList (inter (toOrdList xs) (toOrdList ys))
  where inter a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> inter xs b
            EQ -> c x y : inter xs ys
            GT -> inter a ys
        inter _ _ = []


unsafeMapMonotonicUsingFoldr :: (OrdColl cin a, OrdCollX cout b) => (a -> b) -> (cin -> cout)
unsafeMapMonotonicUsingFoldr f xs = foldr (unsafeInsertMin . f) empty xs




