-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | This module provides default implementations of many of the collection methods.  The functions
--   in this module are used to fill out collection implementations and are not intended to be
--   used directly by end users.

module Data.Edison.Coll.Defaults
where

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

countUsingMember :: SetX c a => c -> a -> Int
countUsingMember xs x = if member xs x then 1 else 0

lookupAllUsingLookupM :: (Set c a,S.Sequence seq) => c -> a -> seq a
lookupAllUsingLookupM xs x =
  case lookupM xs x of
    Nothing -> S.empty
    Just y  -> S.single y

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
  where singleCons xs x = S.lcons (single x) xs

toSeqUsingFold :: (Coll c a,S.Sequence seq) => c -> seq a
toSeqUsingFold = fold S.lcons S.empty

unsafeInsertMaxUsingUnsafeAppend :: OrdCollX c a => c -> a -> c
unsafeInsertMaxUsingUnsafeAppend c x = unsafeAppend c (single x)

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

lookupUsingLookupM :: Coll c a => c -> a -> a
lookupUsingLookupM ys x = runIdentity (lookupM ys x)


lookupUsingLookupAll :: Coll c a => c -> a -> a
lookupUsingLookupAll ys x =
  case lookupAll ys x of
    (y:_) -> y
    [] -> error $ instanceName ys ++ ".lookup: lookup failed"

lookupMUsingLookupAll :: (Coll c a, Monad m) => c -> a -> m a
lookupMUsingLookupAll ys x =
  case lookupAll ys x of
    (y:_) -> return y
    []    -> fail $ instanceName ys ++ ".lookup: lookup failed"

lookupWithDefaultUsingLookupAll :: Coll c a => a -> c -> a -> a
lookupWithDefaultUsingLookupAll dflt ys x =
  case lookupAll ys x of
    (y:_) -> y
    [] -> dflt

lookupWithDefaultUsingLookupM :: Coll c a => a -> c -> a -> a
lookupWithDefaultUsingLookupM dflt ys x =
  case lookupM ys x of
    Just y  -> y
    Nothing -> dflt

deleteMaxUsingMaxView :: OrdColl c a => c -> c
deleteMaxUsingMaxView c =
  case maxView c of
    Just (c',_) -> c'
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

intersectUsingIntersectWith :: Set c a => c -> c -> c
intersectUsingIntersectWith = intersectWith (\x y -> x)

differenceUsingOrdLists :: OrdSet c a => c -> c -> c
differenceUsingOrdLists xs ys = unsafeFromOrdList (diff (toOrdList xs) (toOrdList ys))
  where diff a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : diff xs b
            EQ -> diff xs ys
            GT -> diff a ys
        diff a _ = a

subsetUsingOrdLists :: OrdSet c a => c -> c -> Bool
subsetUsingOrdLists xs ys = subsetOnOrdLists (toOrdList xs) (toOrdList ys)

subsetEqUsingOrdLists :: OrdSet c a => c -> c -> Bool
subsetEqUsingOrdLists xs ys = subsetEqOnOrdLists (toOrdList xs) (toOrdList ys)

subsetOnOrdLists [] [] = False
subsetOnOrdLists [] (_:_) = True
subsetOnOrdLists (_:_) [] = False
subsetOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> subsetOnOrdLists xs ys
    GT -> subsetEqOnOrdLists a ys

subsetEqOnOrdLists [] _ = True
subsetEqOnOrdLists (_:_) [] = False
subsetEqOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> subsetEqOnOrdLists xs ys
    GT -> subsetEqOnOrdLists a ys

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

intersectWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c -> c -> c
intersectWithUsingOrdLists c xs ys = unsafeFromOrdList (inter (toOrdList xs) (toOrdList ys))
  where inter a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> inter xs b
            EQ -> c x y : inter xs ys
            GT -> inter a ys
        inter _ _ = []




