-- |
--   Module      :  Data.Edison.Coll.SkewHeap
--   Copyright   :  Copyright (c) 1998-1999 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   Skew heaps.
--
--   /References:/
--
-- * Daniel Sleator and Robert Tarjan. \"Self-Adjusting Heaps\".
--   /SIAM Journal on Computing/, 15(1):52-69, February 1986.

module Data.Edison.Coll.SkewHeap (
    -- * Type of skew heaps
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

moduleName = "Data.Edison.Coll.SkewHeap"

data Heap a = E | T a (Heap a) (Heap a)

-- invariants:
--  * Heap order
structuralInvariant :: Ord a => Heap a -> Bool
structuralInvariant E = True
structuralInvariant t@(T x l r) = isMin x t
  where isMin x E = True
        isMin x (T y l r) = x <= y && isMin y l && isMin y r


{-
For delete,deleteAll,filter,partition: could compute fringe and reduce
rather that rebuilding with union at every deleted node
-}

empty :: Ord a => Heap a
empty = E

singleton :: Ord a => a -> Heap a
singleton x = T x E E

insert :: Ord a => a -> Heap a -> Heap a
insert x E = T x E E
insert x h@(T y a b)
  | x <= y    = T x h E
  | otherwise = T y (insert x b) a

union :: Ord a => Heap a -> Heap a -> Heap a
union E h = h
union h@(T x a b) h' = union' h x a b h'
  where union' h x a b E = h
        union' hx x a b hy@(T y c d)
          | x <= y    = T x (union' hy y c d b) a
          | otherwise = T y (union' hx x a b d) c

delete :: Ord a => a -> Heap a -> Heap a
delete x h = case del h of
               Just h' -> h'
               Nothing -> h
  where del (T y a b) =
          case compare x y of
            LT -> Nothing
            EQ -> Just (union a b)
            GT -> case del b of
                    Just b' -> Just (T y a b')
                    Nothing -> case del a  of
                                 Just a' -> Just (T y a' b)
                                 Nothing -> Nothing
        del E = Nothing

deleteAll :: Ord a => a -> Heap a -> Heap a
deleteAll x h@(T y a b) =
  case compare x y of
    LT -> h
    EQ -> union (deleteAll x a) (deleteAll x b)
    GT -> T y (deleteAll x a) (deleteAll x b)
deleteAll x E = E

null :: Ord a => Heap a -> Bool
null E = True
null _ = False

size :: Ord a => Heap a -> Int
size h = sz h 0
  where sz E i = i
        sz (T _ a b) i = sz a (sz b (i + 1))

member :: Ord a => a -> Heap a -> Bool
member x E = False
member x (T y a b) =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member x b || member x a

count :: Ord a => a -> Heap a -> Int
count x E = 0
count x (T y a b) =
  case compare x y of
    LT -> 0
    EQ -> 1 + count x b + count x a
    GT -> count x b + count x a

toSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toSeq h = tol h S.empty
  where tol E rest = rest
        tol (T x a b) rest = S.lcons x (tol b (tol a rest))

lookupM :: (Ord a, Monad m) => a -> Heap a -> m a
lookupM x E = fail "SkewHeap.lookupM: XXX"
lookupM x (T y a b) =
  case compare x y of
    LT -> fail "SkewHeap.lookupM: XXX"
    EQ -> return y
    GT -> case lookupM x b `mplus` lookupM x a of
    		Nothing -> fail "SkewHeap.lookupM: XXX"
		Just x  -> return x

lookupAll :: (Ord a,S.Sequence seq) => a -> Heap a -> seq a
lookupAll x h = look h S.empty
  where look E ys = ys
        look (T y a b) ys =
          case compare x y of
            LT -> ys
            EQ -> S.lcons y (look b (look a ys))
            GT -> look b (look a ys)

fold :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold f e E = e
fold f e (T x a b) = f x (fold f (fold f e a) b)

fold' :: Ord a => (a -> b -> b) -> b -> Heap a -> b
fold' f e E = e
fold' f e (T x a b) = e `seq` f x $! (fold' f (fold' f e a) b)

fold1 :: Ord a => (a -> a -> a) -> Heap a -> a
fold1 f E = error "SkewHeap.fold1: empty collection"
fold1 f (T x a b) = fold f (fold f x a) b

fold1' :: Ord a => (a -> a -> a) -> Heap a -> a
fold1' f E = error "SkewHeap.fold1': empty collection"
fold1' f (T x a b) = fold' f (fold' f x a) b

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter p E = E
filter p (T x a b)
    | p x = T x (filter p a) (filter p b)
    | otherwise = union (filter p a) (filter p b)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition p E = (E, E)
partition p (T x a b)
    | p x = (T x a' b', union a'' b'')
    | otherwise = (union a' b', T x a'' b'')
  where (a', a'') = partition p a
        (b', b'') = partition p b


deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (T x a b) = union a b

deleteMax :: Ord a => Heap a -> Heap a
deleteMax h = case maxView h of
                Nothing     -> E
                Just (x,h') -> h'

unsafeInsertMin :: Ord a => a -> Heap a -> Heap a
unsafeInsertMin x h = T x h E

unsafeAppend :: Ord a => Heap a -> Heap a -> Heap a
unsafeAppend E h = h
unsafeAppend (T x a b) h = T x (unsafeAppend b h) a

filterLT :: Ord a => a -> Heap a -> Heap a
filterLT y (T x a b) | x < y = T x (filterLT y a) (filterLT y b)
filterLT y _ = E

filterLE :: Ord a => a -> Heap a -> Heap a
filterLE y (T x a b) | x <= y = T x (filterLE y a) (filterLE y b)
filterLE y _ = E

filterGT :: Ord a => a -> Heap a -> Heap a
filterGT y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(T x a b) hs
          | x > y = h : hs
          | otherwise = collect a (collect b hs)

filterGE :: Ord a => a -> Heap a -> Heap a
filterGE y h = C.unionList (collect h [])
  where collect E hs = hs
        collect h@(T x a b) hs
          | x >= y = h : hs
          | otherwise = collect b (collect a hs)

partitionLT_GE :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GE y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs
          | x >= y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (T x a' b', hs'')

partitionLE_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLE_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs
          | x > y = (E, h:hs)
          | otherwise = let (a', hs') = collect a hs
                            (b', hs'') = collect b hs'
                        in (T x a' b', hs'')

partitionLT_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GT y h = (h', C.unionList hs)
  where (h', hs) = collect h []

        collect E hs = (E, hs)
        collect h@(T x a b) hs = 
          case compare x y of
            GT -> (E, h:hs)
            EQ -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (union a' b', hs'')
            LT -> let (a', hs') = collect a hs
                      (b', hs'') = collect b hs'
                  in (T x a' b', hs'')

minView :: (Ord a, Monad m) => Heap a -> m (a, Heap a)
minView E = fail "SkewHeap.minView: empty heap"
minView (T x a b) = return (x, union a b)

minElem :: Ord a => Heap a -> a
minElem E = error "SkewHeap.minElem: empty collection"
minElem (T x a b) = x

maxView :: (Ord a, Monad m) => Heap a -> m (a, Heap a)
maxView E = fail "SkewHeap.maxView: empty heap"
maxView (T x E E) = return (x, E)
maxView (T x a E) = return (y, T x a' E)
  where Just (y, a') = maxView a
maxView (T x E a) = return (y, T x a' E)
  where Just (y, a') = maxView a
maxView (T x a b)
    | y >= z    = return (y, T x a' b)
    | otherwise = return (z, T x a b')
  where Just (y, a') = maxView a
        Just (z, b') = maxView b

-- warning: maxView and maxElem may disagree if root is equal to max!

maxElem :: Ord a => Heap a -> a
maxElem E = error "SkewHeap.maxElem: empty collection"
maxElem (T x E E) = x
maxElem (T x a E) = maxElem a
maxElem (T x E a) = maxElem a
maxElem (T x a b) = findMax b (findLeaf a)
  where findMax E m = m
        findMax (T x E E) m
          | m >= x = m
          | otherwise = x
        findMax (T x a E) m = findMax a m
        findMax (T x E a) m = findMax a m
        findMax (T x a b) m = findMax a (findMax b m)

        findLeaf E = error "SkewHeap.maxElem: bug"
        findLeaf (T x E E) = x
        findLeaf (T x a E) = findLeaf a
        findLeaf (T x E a) = findLeaf a
        findLeaf (T x a b) = findMax b (findLeaf a)

foldr :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr f e E = e
foldr f e (T x a b) = f x (foldr f e (union a b))

foldr' :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr' f e E = e
foldr' f e (T x a b) = e `seq` f x $! (foldr' f e (union a b))

foldl :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl f e E = e
foldl f e (T x a b) = foldl f (f e x) (union a b)

foldl' :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl' f e E = e
foldl' f e (T x a b) = e `seq` foldl' f (f e x) (union a b)

foldr1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1 f E = error "SkewHeap.foldr1: empty collection"
foldr1 f (T x E E) = x
foldr1 f (T x a b) = f x (foldr1 f (union a b))

foldr1' :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1' f E = error "SkewHeap.foldr1': empty collection"
foldr1' f (T x E E) = x
foldr1' f (T x a b) = f x $! (foldr1' f (union a b))

foldl1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1 f E = error "SkewHeap.foldl1: empty collection"
foldl1 f (T x a b) = foldl f x (union a b)

foldl1' :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1' f E = error "SkewHeap.foldl1': empty collection"
foldl1' f (T x a b) = foldl' f x (union a b)

{- ???? -}
unsafeMapMonotonic :: Ord a => (a -> a) -> Heap a -> Heap a
unsafeMapMonotonic f E = E
unsafeMapMonotonic f (T x a b) =
  T (f x) (unsafeMapMonotonic f a) (unsafeMapMonotonic f b)


strict :: Heap a -> Heap a
strict h@E = h
strict h@(T x l r) = strict l `seq` strict r `seq` h

strictWith :: (a -> b) -> Heap a -> Heap a
strictWith f h@E = h
strictWith f h@(T x l r) = f x `seq` strictWith f l `seq` strictWith f r `seq` h

-- the remaining functions all use default definitions

fromSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
fromSeq = fromSeqUsingUnionSeq

insertSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
insertSeq = insertSeqUsingUnion

unionSeq :: (Ord a,S.Sequence seq) => seq (Heap a) -> Heap a
unionSeq = unionSeqUsingReduce

deleteSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
deleteSeq = deleteSeqUsingDelete

lookup :: Ord a => a -> Heap a -> a
lookup = lookupUsingLookupM

lookupWithDefault :: Ord a => a -> a -> Heap a -> a
lookupWithDefault = lookupWithDefaultUsingLookupM

unsafeInsertMax :: Ord a => a -> Heap a -> Heap a
unsafeInsertMax = unsafeInsertMaxUsingUnsafeAppend

unsafeFromOrdSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
unsafeFromOrdSeq = unsafeFromOrdSeqUsingUnsafeInsertMin

toOrdSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toOrdSeq = toOrdSeqUsingFoldr

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
   filter = filter; partition = partition; strictWith = strictWith}

instance Ord a => C.OrdColl (Heap a) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1  = foldl1; foldl1' = fold1'; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic}

instance Ord a => Eq (Heap a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Heap a) where
  showsPrec = showsPrecUsingToList

instance (Ord a, Read a) => Read (Heap a) where
  readsPrec = readsPrecUsingFromList


instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized (\n -> arbTree n)
    where arbTree 0 = return E
          arbTree n =
            frequency [(1, return E),
                       (4, liftM3 sift arbitrary (arbTree (n `div` 2))
                                                 (arbTree (n `div` 4)))]

          sift x s@(T y a b) E
            | y < x = T y (sift x a b) E
          sift x E s@(T y a b)
            | y < x = T y E (sift x a b)
          sift x s@(T y a b) t@(T z c d)
            | y < x && y <= z = T y (sift x a b) t
            | z < x           = T z s (sift x c d)
          sift x a b = T x a b

  coarbitrary E = variant 0
  coarbitrary (T x a b) = 
      variant 1 . coarbitrary x . coarbitrary a . coarbitrary b

instance (Ord a) => Monoid (Heap a) where
    mempty  = empty
    mappend = union
    mconcat = unionSeq

instance (Ord a) => Ord (Heap a) where
    compare = compareUsingToOrdList
