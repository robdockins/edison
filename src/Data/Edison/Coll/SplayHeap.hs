-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Coll.SplayHeap (
    -- type of splay heaps
    Heap, -- instance of Coll/CollX, OrdColl/OrdCollX

    -- CollX operations
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,

    -- Coll operations
    toSeq, lookup, lookupM, lookupAll, lookupWithDefault, fold, fold1,
    filter, partition,

    -- OrdCollX operations
    deleteMin,deleteMax,unsafeInsertMin,unsafeInsertMax,unsafeFromOrdSeq,
    unsafeAppend,filterLT,filterLE,filterGT,filterGE,partitionLT_GE,
    partitionLE_GT,partitionLT_GT,

    -- OrdColl operations
    minView,minElem,maxView,maxElem,foldr,foldl,foldr1,foldl1,toOrdSeq,

    -- other supported operations
    unsafeMapMonotonic,

    -- documentation
    moduleName
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Prelude
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq as S
import Data.Edison.Coll.Defaults
import Control.Monad
import Test.QuickCheck

moduleName = "SplayHeap"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 5.4.
--
-- If minElem is called frequently, then SplayHeap should
-- be used in conjunction with MinHeap.

data Heap a = E | T (Heap a) a (Heap a)

empty     :: Heap a
single    :: a -> Heap a
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
member    :: Ord a => Heap a -> a -> Bool
count     :: Ord a => Heap a -> a -> Int

toSeq     :: (Ord a, S.Sequence s) => Heap a -> s a
lookup    :: Ord a => Heap a -> a -> a
lookupM   :: (Ord a,Monad m) => Heap a -> a -> m a
lookupAll :: (Ord a,S.Sequence s) => Heap a -> a -> s a
lookupWithDefault :: Ord a => a -> Heap a -> a -> a
fold      :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold1     :: Ord a => (a -> a -> a) -> Heap a -> a
filter    :: Ord a => (a -> Bool) -> Heap a -> Heap a
partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)

deleteMin        :: Ord a => Heap a -> Heap a
deleteMax        :: Ord a => Heap a -> Heap a
unsafeInsertMin  :: Ord a => a -> Heap a -> Heap a
unsafeInsertMax  :: Ord a => Heap a -> a -> Heap a
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
maxView  :: (Ord a,Monad m) => Heap a -> m (Heap a, a)
maxElem  :: Ord a => Heap a -> a
foldr    :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldl    :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldr1   :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1   :: Ord a => (a -> a -> a) -> Heap a -> a
toOrdSeq :: (Ord a,S.Sequence s) => Heap a -> s a

unsafeMapMonotonic :: (a -> b) -> Heap a -> Heap b

empty = E
single x = T E x E

insert x xs = T a x b
  where (a,b) = partitionLE_GT x xs

union E ys = ys
union (T a x b) ys = T (union c a) x (union d b)
  where (c,d) = partitionLE_GT x ys

delete x xs =
  let (a,b) = partitionLE_GT x xs
  in case maxView a of
       Nothing -> b
       Just (a',y)
         | x > y -> T a' y b
         | otherwise -> unsafeAppend a' b

deleteAll x xs = unsafeAppend a b
  where (a,b) = partitionLT_GT x xs

null E = True
null (T a x b) = False

size = sz 0
  where sz n E = n
        sz n (T a x b) = sz (sz (1+n) a) b
  
member E x = False
member (T a y b) x = if x < y then member a x else x==y || member b x

count = cnt 0
  where cnt n E x = n
        cnt n (T a y b) x
          | x < y = cnt n a x
          | x > y = cnt n b x
          | otherwise = cnt (cnt (1+n) a x) b x

toSeq xs = tos xs S.empty
  where tos E rest = rest
        tos (T a x b) rest = S.lcons x (tos a (tos b rest))

lookup E x = error "SplayHeap.lookup: empty heap"
lookup (T a y b) x
  | x < y     = lookup a x
  | x > y     = lookup b x
  | otherwise = y

lookupM E x = fail "SplayHeap.lookup: empty heap"
lookupM (T a y b) x
  | x < y     = lookupM a x
  | x > y     = lookupM b x
  | otherwise = return y

lookupWithDefault d E x = d
lookupWithDefault d (T a y b) x
  | x < y     = lookupWithDefault d a x
  | x > y     = lookupWithDefault d b x
  | otherwise = y

lookupAll xs x = look xs x S.empty
  where look E x rest = rest
        look (T a y b) x rest
          | x < y     = look a x rest
          | x > y     = look b x rest
          | otherwise = look a x (S.lcons y (look b x rest))

fold f e E = e
fold f e (T a x b) = f x (fold f (fold f e b) a)

fold1 f E = error "SplayHeap.fold1: empty heap"
fold1 f (T a x b) = fold f (fold f x b) a

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
unsafeInsertMax xs x = T xs x E

unsafeAppend a b = case maxView a of
                       Nothing     -> b
                       Just (a',x) -> T a' x b

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
maxView (T a x b) = return (ys,y)
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

foldl f e E = e
foldl f e (T a x b) = foldl f (f (foldl f e a) x) b

foldr1 f E = error "SplayHeap.foldr1: empty heap"
foldr1 f (T a x b) = foldr f (myfold f x b) a
  where myfold f x E = x
        myfold f x (T a y b) = f x (foldr f (myfold f y b) a)

foldl1 f E = error "SplayHeap.foldl1: empty heap"
foldl1 f (T a x b) = foldl f (myfold f a x) b
  where myfold f E x = x
        myfold f (T a x b) y = f (foldl f (myfold f a x) b) y

toOrdSeq xs = tos xs S.empty
  where tos E rest = rest
        tos (T a x b) rest = tos a (S.lcons x (tos b rest))

unsafeMapMonotonic f E = E
unsafeMapMonotonic f (T a x b) =
  T (unsafeMapMonotonic f a) (f x) (unsafeMapMonotonic f b)

-- the remaining functions all use defaults

fromSeq = fromSeqUsingFoldr
insertSeq = insertSeqUsingFoldr
unionSeq = unionSeqUsingReduce
deleteSeq = deleteSeqUsingDelete
unsafeFromOrdSeq = unsafeFromOrdSeqUsingUnsafeInsertMin

-- instance declarations

instance Ord a => C.CollX (Heap a) a where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   instanceName c = moduleName}

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
   fold = fold; fold1 = fold1; filter = filter; partition = partition}

instance Ord a => C.OrdColl (Heap a) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldl = foldl; foldr1 = foldr1; 
   foldl1 = foldl1; toOrdSeq = toOrdSeq}


instance Ord a => Eq (Heap a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Heap a) where
  show xs = show (C.toOrdList xs)

instance (Ord a,Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = do xs <- arbitrary
                 return (C.fromList xs)

  coarbitrary E = variant 0
  coarbitrary (T a x b) = 
    variant 1 . coarbitrary a . coarbitrary x . coarbitrary b

