-- |
--   Module      :  Data.Edison.Coll.Defaults
--   Copyright   :  Copyright (c) 1998, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  internal (unstable)
--   Portability :  GHC / Hugs (MPTC and FD)
--
--   This module provides default implementations of many of the collection methods.  The functions
--   in this module are used to fill out collection implementations and are not intended to be
--   used directly by end users.

module Data.Edison.Coll.Defaults where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import qualified Control.Monad.Fail as MF
import Control.Monad.Identity

import Data.Edison.Coll
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Seq.Defaults (tokenMatch,maybeParens)

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

unionSeqUsingFoldl' :: (CollX c a,S.Sequence seq) => seq c -> c
unionSeqUsingFoldl' = S.foldl' union empty

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
  where disj a@(c:cs) b@(d:ds) =
          case compare c d of
            LT -> disj cs b
            EQ -> False
            GT -> disj a ds
        disj _ _ = True

intersectWitnessUsingToOrdList ::
        (OrdColl c a, MF.MonadFail m) => c -> c -> m (a,a)
intersectWitnessUsingToOrdList as bs = witness (toOrdList as) (toOrdList bs)
  where witness a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> witness xs b
            EQ -> return (x, y)
            GT -> witness a ys
        -- XXX
        witness _ _ = fail $ instanceName as ++ ".intersect: failed"

lookupUsingLookupM :: Coll c a => a -> c -> a
lookupUsingLookupM x ys = runIdentity (lookupM x ys)

lookupUsingLookupAll :: Coll c a => a -> c -> a
lookupUsingLookupAll x ys =
  case lookupAll x ys of
    (y:_) -> y
    [] -> error $ instanceName ys ++ ".lookup: lookup failed"

lookupMUsingLookupAll :: (Coll c a, MF.MonadFail m) => a -> c -> m a
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
insertUsingInsertWith = insertWith (\x _ -> x)

unionUsingUnionWith :: Set c a => c -> c -> c
unionUsingUnionWith = unionWith (\x _ -> x)

filterUsingOrdLists :: OrdColl c a => (a -> Bool) -> c -> c
filterUsingOrdLists p = unsafeFromOrdList . L.filter p . toOrdList

partitionUsingOrdLists :: OrdColl c a => (a -> Bool) -> c -> (c,c)
partitionUsingOrdLists p xs = (unsafeFromOrdList ys,unsafeFromOrdList zs)
  where (ys,zs) = L.partition p (toOrdList xs)

intersectionUsingIntersectionWith :: Set c a => c -> c -> c
intersectionUsingIntersectionWith = intersectionWith (\x _ -> x)

differenceUsingOrdLists :: OrdSet c a => c -> c -> c
differenceUsingOrdLists as bs = unsafeFromOrdList $ diff (toOrdList as) (toOrdList bs)
  where diff a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : diff xs b
            EQ -> diff xs ys
            GT -> diff a ys
        diff a _ = a

symmetricDifferenceUsingDifference :: SetX c a => c -> c -> c
symmetricDifferenceUsingDifference xs ys = union (difference xs ys) (difference ys xs)

properSubsetUsingOrdLists :: OrdSet c a => c -> c -> Bool
properSubsetUsingOrdLists xs ys = properSubsetOnOrdLists (toOrdList xs) (toOrdList ys)

subsetUsingOrdLists :: OrdSet c a => c -> c -> Bool
subsetUsingOrdLists xs ys = subsetOnOrdLists (toOrdList xs) (toOrdList ys)

properSubsetOnOrdLists :: (Ord t) => [t] -> [t] -> Bool
properSubsetOnOrdLists [] [] = False
properSubsetOnOrdLists [] (_:_) = True
properSubsetOnOrdLists (_:_) [] = False
properSubsetOnOrdLists a@(x:xs) (y:ys) =
  case compare x y of
    LT -> False
    EQ -> properSubsetOnOrdLists xs ys
    GT -> subsetOnOrdLists a ys

subsetOnOrdLists :: (Ord t) => [t] -> [t] -> Bool
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
unionlUsingUnionWith xs ys = unionWith (\x _ -> x) xs ys

unionrUsingUnionWith :: Set c a => c -> c -> c
unionrUsingUnionWith xs ys = unionWith (\_ y -> y) xs ys

unionWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c -> c -> c
unionWithUsingOrdLists c as bs = unsafeFromOrdList $ merge (toOrdList as) (toOrdList bs)
  where merge a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> x : merge xs b
            EQ -> c x y : merge xs ys
            GT -> y : merge a ys
        merge a [] = a
        merge [] b = b

unionSeqWithUsingReducer :: (Set c a,S.Sequence seq) => (a -> a -> a) -> seq c -> c
unionSeqWithUsingReducer c = S.reducer (unionWith c) empty

intersectionWithUsingOrdLists :: OrdSet c a => (a -> a -> a) -> c -> c -> c
intersectionWithUsingOrdLists c as bs = unsafeFromOrdList $ inter (toOrdList as) (toOrdList bs)
  where inter a@(x:xs) b@(y:ys) =
          case compare x y of
            LT -> inter xs b
            EQ -> c x y : inter xs ys
            GT -> inter a ys
        inter _ _ = []


unsafeMapMonotonicUsingFoldr :: (OrdColl cin a, OrdCollX cout b) => (a -> b) -> (cin -> cout)
unsafeMapMonotonicUsingFoldr f xs = foldr (unsafeInsertMin . f) empty xs

showsPrecUsingToList :: (Coll c a,Show a) => Int -> c -> ShowS
showsPrecUsingToList i xs rest
  | i == 0    = concat [    instanceName xs,".fromSeq ",showsPrec 10 (toList xs) rest]
  | otherwise = concat ["(",instanceName xs,".fromSeq ",showsPrec 10 (toList xs) (')':rest)]

readsPrecUsingFromList :: (Coll c a, Read a) => Int -> ReadS c
readsPrecUsingFromList _ xs =
    let result = maybeParens p xs
        p ys = tokenMatch ((instanceName x) ++ ".fromSeq") ys
                 >>= readsPrec 10
                 >>= \(l,rest) -> return (fromList l,rest)

        -- play games with the typechecker so we don't have to use
        -- extensions for scoped type variables
        ~[(x,_)] = result

    in result

compareUsingToOrdList :: OrdColl c a => c -> c -> Ordering
compareUsingToOrdList as bs = cmp (toOrdList as) (toOrdList bs)
 where
  cmp [] [] = EQ
  cmp [] _  = LT
  cmp _  [] = GT
  cmp (x:xs) (y:ys) =
      case compare x y of
         EQ -> cmp xs ys
         c -> c

