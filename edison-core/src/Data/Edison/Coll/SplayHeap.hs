-- |
--   Module      :  Data.Edison.Coll.SplayHeap
--   Copyright   :  Copyright (c) 1999 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Splay heaps.
--
--   If 'minElem' is called frequently, then SplayHeap should
--   be used in conjunction with "Data.Edison.Coll.MinHeap".
--
--   /References:/
--
-- * Chris Okasaki. /Purely Functional Data Structures/. 1998.
--   Section 5.4.

module Data.Edison.Coll.SplayHeap (
    -- * Type of splay heaps
    Heap, -- instance of Coll/CollX, OrdColl/OrdCollX

    -- * CollX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,strict,structuralInvariant,

    -- * Coll operations
    toSeq, lookup, lookupM, lookupAll, lookupWithDefault, fold, fold',
    fold1, fold1', filter, partition, strictWith,

    -- * OrdCollX operations
    deleteMin,deleteMax,unsafeInsertMin,unsafeInsertMax,unsafeFromOrdSeq,
    unsafeAppend,filterLT,filterLE,filterGT,filterGE,partitionLT_GE,
    partitionLE_GT,partitionLT_GT,

    -- * OrdColl operations
    minView,minElem,maxView,maxElem,foldr,foldr',foldl,foldl',
    foldr1,foldr1',foldl1,foldl1',toOrdSeq,
    unsafeMapMonotonic,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Prelude
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq as S
import Data.Edison.Coll.Defaults
import Data.Monoid
import Control.Monad
import Test.QuickCheck

moduleName = "Data.Edison.Coll.SplayHeap"

data Heap a = E | T (Heap a) a (Heap a)

-- invariants:
--    * Binary Search Tree order (allowing duplicates)

structuralInvariant :: Ord a => Heap a -> Bool
structuralInvariant t = bounded Nothing Nothing t
   where bounded _ _ E = True
         bounded lo hi (T l x r)  = cmp_l lo x 
                                 && cmp_r x hi
                                 && bounded lo (Just x) l
                                 && bounded (Just x) hi r

         cmp_l Nothing  _ = True
         cmp_l (Just x) y = x <= y

         cmp_r _ Nothing  = True
         cmp_r x (Just y) = x <= y


empty     :: Heap a
singleton :: a -> Heap a
fromSeq   :: (Ord a,S.Sequence s) => s a -> Heap a
insert    :: Ord a => a -> Heap a -> Heap a
insertSeq :: (Ord a,S.Sequence s) => s a -> Heap a -> Heap a
union     :: Ord a => Heap a -> Heap a -> Heap a
unionSeq  :: (Ord a,S.Sequence s) => s (Heap a) -> Heap a
delete    :: Ord a => a -> Heap a -> Heap a
deleteAll :: Ord a => a -> Heap a -> Heap a
deleteSeq :: (Ord a,S.Sequence s) => s a -> Heap a -> Heap a
null      :: Heap a -> Bool
size      :: Heap a -> Int
member    :: Ord a => a -> Heap a -> Bool
count     :: Ord a => a -> Heap a -> Int
strict    :: Heap a -> Heap a

toSeq     :: (Ord a, S.Sequence s) => Heap a -> s a
lookup    :: Ord a => a -> Heap a -> a
lookupM   :: (Ord a,Monad m) => a -> Heap a -> m a
lookupAll :: (Ord a,S.Sequence s) => a -> Heap a -> s a
lookupWithDefault :: Ord a => a -> a -> Heap a -> a
fold      :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold1     :: Ord a => (a -> a -> a) -> Heap a -> a
fold'     :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold1'    :: Ord a => (a -> a -> a) -> Heap a -> a
filter    :: Ord a => (a -> Bool) -> Heap a -> Heap a
partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
strictWith :: (a -> b) -> Heap a -> Heap a

deleteMin        :: Ord a => Heap a -> Heap a
deleteMax        :: Ord a => Heap a -> Heap a
unsafeInsertMin  :: Ord a => a -> Heap a -> Heap a
unsafeInsertMax  :: Ord a => a -> Heap a -> Heap a
unsafeFromOrdSeq :: (Ord a,S.Sequence s) => s a -> Heap a
unsafeAppend     :: Ord a => Heap a -> Heap a -> Heap a
filterLT         :: Ord a => a -> Heap a -> Heap a
filterLE         :: Ord a => a -> Heap a -> Heap a
filterGT         :: Ord a => a -> Heap a -> Heap a
filterGE         :: Ord a => a -> Heap a -> Heap a
partitionLT_GE   :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLE_GT   :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GT   :: Ord a => a -> Heap a -> (Heap a, Heap a)

minView  :: (Ord a,Monad m) => Heap a -> m (a, Heap a)
minElem  :: Ord a => Heap a -> a
maxView  :: (Ord a,Monad m) => Heap a -> m (a, Heap a)
maxElem  :: Ord a => Heap a -> a
foldr    :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldl    :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldr1   :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1   :: Ord a => (a -> a -> a) -> Heap a -> a
foldr'   :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldl'   :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldr1'  :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1'  :: Ord a => (a -> a -> a) -> Heap a -> a
toOrdSeq :: (Ord a,S.Sequence s) => Heap a -> s a

unsafeMapMonotonic :: (a -> b) -> Heap a -> Heap b

empty = E
singleton x = T E x E

insert x xs = T a x b
  where (a,b) = partitionLE_GT x xs

union E ys = ys
union (T a x b) ys = T (union c a) x (union d b)
  where (c,d) = partitionLE_GT x ys

delete x xs =
  let (a,b) = partitionLE_GT x xs
  in case maxView a of
       Nothing -> b
       Just (y, a')
         | x > y -> T a' y b
         | otherwise -> unsafeAppend a' b

deleteAll x xs = unsafeAppend a b
  where (a,b) = partitionLT_GT x xs

null E = True
null (T a x b) = False

size = sz 0
  where sz n E = n
        sz n (T a x b) = sz (sz (1+n) a) b
  
member x E = False
member x (T a y b) = if x < y then member x a else x==y || member x b

count = cnt 0
  where cnt n x E = n
        cnt n x (T a y b)
          | x < y = cnt n x a
          | x > y = cnt n x b
          | otherwise = cnt (cnt (1+n) x a) x b

toSeq xs = tos xs S.empty
  where tos E rest = rest
        tos (T a x b) rest = S.lcons x (tos a (tos b rest))

lookup x E = error "SplayHeap.lookup: empty heap"
lookup x (T a y b)
  | x < y     = lookup x a
  | x > y     = lookup x b
  | otherwise = y

lookupM x E = fail "SplayHeap.lookup: empty heap"
lookupM x (T a y b)
  | x < y     = lookupM x a
  | x > y     = lookupM x b
  | otherwise = return y

lookupWithDefault d x E = d
lookupWithDefault d x (T a y b)
  | x < y     = lookupWithDefault d x a
  | x > y     = lookupWithDefault d x b
  | otherwise = y

lookupAll x xs = look xs x S.empty
  where look E x rest = rest
        look (T a y b) x rest
          | x < y     = look a x rest
          | x > y     = look b x rest
          | otherwise = look a x (S.lcons y (look b x rest))

fold f e E = e
fold f e (T a x b) = f x (fold f (fold f e b) a)

fold' f e E = e
fold' f e (T a x b) = e `seq` f x $! (fold' f (fold' f e b) a)

fold1 f E = error "SplayHeap.fold1: empty heap"
fold1 f (T a x b) = fold f (fold f x b) a

fold1' f E = error "SplayHeap.fold1': empty heap"
fold1' f (T a x b) = fold' f (fold' f x b) a

filter p E = E
filter p (T a x b) 
  | p x       = T (filter p a) x (filter p b)
  | otherwise = unsafeAppend (filter p a) (filter p b)

partition p E = (E, E)
partition p (T a x b)
    | p x       = (T a0 x b0, unsafeAppend a1 b1)
    | otherwise = (unsafeAppend a0 b0, T a1 x b1)
  where (a0,a1) = partition p a
        (b0,b1) = partition p b

deleteMin E = E
deleteMin (T a x b) = del a x b
  where del E x b = b
        del (T E x b) y c = T b y c
        del (T (T a x b) y c) z d = T (del a x b) y (T c z d)

deleteMax E = E
deleteMax (T a x b) = del a x b
  where del a x E = a
        del a x (T b y E) = T a x b
        del a x (T b y (T c z d)) = T (T a x b) y (del c z d)

unsafeInsertMin x xs = T E x xs
unsafeInsertMax x xs = T xs x E

unsafeAppend a b = case maxView a of
                       Nothing      -> b
                       Just (x, a') -> T a' x b

filterLT k E = E
filterLT k t@(T a x b) = 
  if x >= k then filterLT k a
  else case b of
         E -> t
         T ba y bb ->
           if y >= k then T a x (filterLT k ba) 
                     else T (T a x ba) y (filterLT k bb)

filterLE k E = E
filterLE k t@(T a x b) = 
  if x > k then filterLE k a
  else case b of
         E -> t
         T ba y bb ->
           if y > k then T a x (filterLE k ba) 
                    else T (T a x ba) y (filterLE k bb)

filterGT k E = E
filterGT k t@(T a x b) =
  if x <= k then filterGT k b
  else case a of
         E -> t
         T aa y ab ->
           if y <= k then T (filterGT k ab) x b
                     else T (filterGT k aa) y (T ab x b)

filterGE k E = E
filterGE k t@(T a x b) =
  if x < k then filterGE k b
  else case a of
         E -> t
         T aa y ab ->
           if y < k then T (filterGE k ab) x b
                    else T (filterGE k aa) y (T ab x b)

partitionLT_GE k E = (E,E)
partitionLT_GE k t@(T a x b) =
  if x >= k then
    case a of
      E -> (E,t)
      T aa y ab ->
        if y >= k then
          let (small,big) = partitionLT_GE k aa
          in (small, T big y (T ab x b))
        else
          let (small,big) = partitionLT_GE k ab
          in (T aa y small, T big x b)
  else
    case b of
      E -> (t,E)
      T ba y bb ->
        if y >= k then
          let (small,big) = partitionLT_GE k ba
          in (T a x small, T big y bb)
        else
          let (small,big) = partitionLT_GE k bb
          in (T (T a x ba) y small, big)

partitionLE_GT k E = (E,E)
partitionLE_GT k t@(T a x b) =
  if x > k then
    case a of
      E -> (E,t)
      T aa y ab ->
        if y > k then
          let (small,big) = partitionLE_GT k aa
          in (small, T big y (T ab x b))
        else
          let (small,big) = partitionLE_GT k ab
          in (T aa y small, T big x b)
  else
    case b of
      E -> (t,E)
      T ba y bb ->
        if y > k then
          let (small,big) = partitionLE_GT k ba
          in (T a x small, T big y bb)
        else
          let (small,big) = partitionLE_GT k bb
          in (T (T a x ba) y small, big)


-- could specialize calls to filterLT/filterGT
partitionLT_GT k E = (E,E)
partitionLT_GT k t@(T a x b) =
  if x > k then
    case a of
      E -> (E,t)
      T aa y ab ->
        if y > k then
          let (small,big) = partitionLT_GT k aa
          in (small, T big y (T ab x b))
        else if y < k then
          let (small,big) = partitionLT_GT k ab
          in (T aa y small, T big x b)
        else (filterLT k aa, T (filterGT k ab) x b)
  else if x < k then
    case b of
      E -> (t,E)
      T ba y bb ->
        if y > k then
          let (small,big) = partitionLT_GT k ba
          in (T a x small, T big y bb)
        else if y < k then
          let (small,big) = partitionLT_GT k bb
          in (T (T a x ba) y small, big)
        else (T a x (filterLT k ba), filterGT k bb)
  else (filterLT k a, filterGT k b)

minView E = fail "SplayHeap.minView: empty heap"
minView (T a x b) = return (y, ys)
  where (y,ys) = minv a x b
        minv E x b = (x,b)
        minv (T E x b) y c = (x,T b y c)
        minv (T (T a x b) y c) z d = (w,T ab y (T c z d))
          where (w,ab) = minv a x b

minElem E = error "SplayHeap.minElem: empty heap"
minElem (T a x b) = minel a x
  where minel E x = x
        minel (T a x b) _ = minel a x


maxView E = fail "SplayHeap.maxView: empty heap"
maxView (T a x b) = return (y,ys)
  where (ys,y) = maxv a x b
        maxv a x E = (a,x)
        maxv a x (T b y E) = (T a x b,y)
        maxv a x (T b y (T c z d)) = (T (T a x b) y cd,w)
          where (cd,w) = maxv c z d

maxElem E = error "SplayHeap.minElem: empty heap"
maxElem (T a x b) = maxel x b
  where maxel x E = x
        maxel _ (T a x b) = maxel x b

foldr f e E = e
foldr f e (T a x b) = foldr f (f x (foldr f e b)) a

foldr' f e E = e
foldr' f e (T a x b) = foldr' f (f x $! (foldr' f e b)) a

foldl f e E = e
foldl f e (T a x b) = foldl f (f (foldl f e a) x) b

foldl' f e E = e
foldl' f e (T a x b) = e `seq` foldl' f ((f $! (foldl' f e a)) x) b

foldr1 f E = error "SplayHeap.foldr1: empty heap"
foldr1 f (T a x b) = foldr f (myfold f x b) a
  where myfold f x E = x
        myfold f x (T a y b) = f x (foldr f (myfold f y b) a)

foldr1' f E = error "SplayHeap.foldr1': empty heap"
foldr1' f (T a x b) = foldr' f (myfold f x b) a
  where myfold f x E = x
        myfold f x (T a y b) = f x $! (foldr' f (myfold f y b) a)

foldl1 f E = error "SplayHeap.foldl1: empty heap"
foldl1 f (T a x b) = foldl f (myfold f a x) b
  where myfold f E x = x
        myfold f (T a x b) y = f (foldl f (myfold f a x) b) y

foldl1' f E = error "SplayHeap.foldl1': empty heap"
foldl1' f (T a x b) = foldl' f (myfold f a x) b
  where myfold f E x = x
        myfold f (T a x b) y = (f $! (foldl f (myfold f a x) b)) y

toOrdSeq xs = tos xs S.empty
  where tos E rest = rest
        tos (T a x b) rest = tos a (S.lcons x (tos b rest))

unsafeMapMonotonic f E = E
unsafeMapMonotonic f (T a x b) =
  T (unsafeMapMonotonic f a) (f x) (unsafeMapMonotonic f b)

strict h@E = h
strict h@(T l x r) = strict l `seq` strict r `seq` h

strictWith f h@E = h
strictWith f h@(T l x r) = f x `seq` strictWith f l `seq` strictWith f r `seq` h

-- the remaining functions all use defaults

fromSeq = fromSeqUsingFoldr
insertSeq = insertSeqUsingFoldr
unionSeq = unionSeqUsingReduce
deleteSeq = deleteSeqUsingDelete
unsafeFromOrdSeq = unsafeFromOrdSeqUsingUnsafeInsertMin

-- instance declarations

instance Ord a => C.CollX (Heap a) a where
  {singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   strict = strict;
   structuralInvariant = structuralInvariant; instanceName c = moduleName}

instance Ord a => C.OrdCollX (Heap a) a where
  {deleteMin = deleteMin; deleteMax = deleteMax; 
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax; 
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend; 
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT; 
   filterGE = filterGE; partitionLT_GE = partitionLT_GE; 
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Ord a => C.Coll (Heap a) a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM; 
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault; 
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   strictWith = strictWith;
   filter = filter; partition = partition}

instance Ord a => C.OrdColl (Heap a) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldr' = foldr'; foldl = foldl; 
   foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic}


instance Ord a => Eq (Heap a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Heap a) where
  showsPrec = showsPrecUsingToList

instance (Ord a, Read a) => Read (Heap a) where
  readsPrec = readsPrecUsingFromList

instance (Ord a,Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = do xs <- arbitrary
                 return (C.fromList xs)

  coarbitrary E = variant 0
  coarbitrary (T a x b) = 
    variant 1 . coarbitrary a . coarbitrary x . coarbitrary b

instance (Ord a) => Monoid (Heap a) where
    mempty  = empty
    mappend = union
    mconcat = unionSeq

instance (Ord a) => Ord (Heap a) where
    compare = compareUsingToOrdList
