-- |
--   Module      :  Data.Edison.Coll.LazyPairingHeap
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Lazy Paring Heaps
--
--   /References:/
--
-- * Chris Okasaki. /Purely Functional Data Structures/. 1998.
--   Section 6.5.

module Data.Edison.Coll.LazyPairingHeap (
    -- * Type of pairing heaps
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

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,foldl',lookup,filter)
import qualified Data.Edison.Coll as C ( CollX(..), OrdCollX(..),
                                   Coll(..), OrdColl(..), toOrdList )
import qualified Data.Edison.Seq as S
import Data.Edison.Coll.Defaults
import Data.List (sort)
import Data.Monoid
import Data.Semigroup as SG
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Test.QuickCheck

moduleName :: String
moduleName = "Data.Edison.Coll.LazyPairingHeap"


data Heap a = E
            | H1 a (Heap a)
            | H2 a !(Heap a) (Heap a)


-- Invariants:
--   * left child of H2 not empty
structuralInvariant :: Heap a -> Bool
structuralInvariant E = True
structuralInvariant (H1 _ h) = structuralInvariant h
structuralInvariant (H2 _ E _) = False
structuralInvariant (H2 _ l r) = structuralInvariant l && structuralInvariant r

-- second arg is not empty
-- not used!
-- link E h = h
-- link (H1 x b) a = H2 x a b
-- link (H2 x a b) a' = H1 x (union (union a a') b)

makeH2 :: a -> Heap a -> Heap a -> Heap a
makeH2 x E xs = H1 x xs
makeH2 x h xs = H2 x h xs

empty :: Heap a
empty = E

singleton :: a -> Heap a
singleton x = H1 x E

insert :: Ord a => a -> Heap a -> Heap a
insert x E = H1 x E
insert x h@(H1 y b)
  | x <= y    = H1 x h
  | otherwise = H2 y (H1 x E) b
insert x h@(H2 y a b)
  | x <= y    = H1 x h
  | otherwise = H1 y (union (insert x a) b)

union :: Ord a => Heap a -> Heap a -> Heap a
union E h = h
union hx@(H1 _ _) E = hx
union hx@(H1 x xs) hy@(H1 y ys)
  | x <= y    = H2 x hy xs
  | otherwise = H2 y hx ys
union hx@(H1 x xs) hy@(H2 y a ys)
  | x <= y    = H2 x hy xs
  | otherwise = H1 y (union (union hx a) ys)
union hx@(H2 _ _ _) E = hx
union hx@(H2 x a xs) hy@(H1 y ys)
  | x <= y    = H1 x (union (union hy a) xs)
  | otherwise = H2 y hx ys
union hx@(H2 x a xs) hy@(H2 y b ys)
  | x <= y    = H1 x (union (union hy a) xs)
  | otherwise = H1 y (union (union hx b) ys)

delete :: Ord a => a -> Heap a -> Heap a
delete y h = case del h of Just h' -> h'
                           Nothing -> h
  where del E = Nothing
        del (H1 x xs) =
          case compare x y of
            LT -> case del xs of
                    Just ys -> Just (H1 x ys)
                    Nothing -> Nothing
            EQ -> Just xs
            GT -> Nothing
        del (H2 x a xs) =
          case compare x y of
            LT -> case del a of
                    Just a' -> Just (makeH2 x a' xs)
                    Nothing -> case del xs of
                                 Just xs' -> Just (H2 x a xs')
                                 Nothing -> Nothing
            EQ -> Just (union a xs)
            GT -> Nothing

deleteAll :: Ord a => a -> Heap a -> Heap a
deleteAll _ E = E
deleteAll y h@(H1 x xs) =
  case compare x y of
    LT -> H1 x (deleteAll y xs)
    EQ -> deleteAll y xs
    GT -> h
deleteAll y h@(H2 x a xs) =
  case compare x y of
    LT -> makeH2 x (deleteAll y a) (deleteAll y xs)
    EQ -> union (deleteAll y a) (deleteAll y xs)
    GT -> h

deleteSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
deleteSeq = delList . sort . S.toList
  where delList [] h = h
        delList (y:ys) h = del y ys h

        del _ _ E = E
        del y ys h@(H1 x xs) =
          case compare x y of
            LT -> H1 x (del y ys xs)
            EQ -> delList ys xs
            GT -> delList ys h
        del y ys h@(H2 x a xs) =
          case compare x y of
            LT -> H1 x (del y ys (union a xs))
            EQ -> delList ys (union a xs)
            GT -> delList ys h
        {-
           could write the two GT cases as
             delList (dropWhile (< x) ys) h
           but this is only a win if we expect many of the ys
           to be missing from the tree.  However, we expect most
           of the ys to be present.
        -}

null :: Heap a -> Bool
null E = True
null _ = False

size :: Heap a -> Int
size E = 0
size (H1 _ xs) = 1 + size xs
size (H2 _ h xs) = 1 + size h + size xs

member :: Ord a => a -> Heap a -> Bool
member _ E = False
member x (H1 y ys) =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member x ys
member x (H2 y h ys) =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member x h || member x ys

count :: Ord a => a -> Heap a -> Int
count _ E = 0
count x (H1 y ys) =
  case compare x y of
    LT -> 0
    EQ -> 1 + count x ys
    GT -> count x ys
count x (H2 y h ys) =
  case compare x y of
    LT -> 0
    EQ -> 1 + count x h + count x ys
    GT -> count x h + count x ys

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (H1 _ xs) = xs
deleteMin (H2 _ h xs) = union h xs

unsafeInsertMin :: Ord a => a -> Heap a -> Heap a
unsafeInsertMin = H1

unsafeInsertMax :: Ord a => a -> Heap a -> Heap a
unsafeInsertMax x E = H1 x E
unsafeInsertMax x (H1 y ys) = H2 y (H1 x E) ys
unsafeInsertMax x (H2 y h ys) = H1 y (union (unsafeInsertMax x h) ys)

unsafeAppend :: Ord a => Heap a -> Heap a -> Heap a
unsafeAppend h E = h
unsafeAppend E h = h
unsafeAppend (H1 x xs) h = H2 x h xs
unsafeAppend (H2 x a xs) h = H1 x (union (unsafeAppend a h) xs)

filterLT :: Ord a => a -> Heap a -> Heap a
filterLT _ E = E
filterLT y (H1 x xs)
  | x < y = H1 x (filterLT y xs)
  | otherwise = E
filterLT y (H2 x h xs)
  | x < y = makeH2 x (filterLT y h) (filterLT y xs)
  | otherwise = E

filterLE :: Ord a => a -> Heap a -> Heap a
filterLE _ E = E
filterLE y (H1 x xs)
  | x <= y = H1 x (filterLE y xs)
  | otherwise = E
filterLE y (H2 x h xs)
  | x <= y = makeH2 x (filterLE y h) (filterLE y xs)
  | otherwise = E

filterGT :: Ord a => a -> Heap a -> Heap a
filterGT y h = fgt h E
  where fgt E rest = rest
        fgt i@(H1 x xs) rest
          | x > y = union i rest
          | otherwise = fgt xs rest
        fgt i@(H2 x a xs) rest
          | x > y = union i rest
          | otherwise = fgt a (fgt xs rest)

filterGE :: Ord a => a -> Heap a -> Heap a
filterGE y h = fge h E
  where fge E rest = rest
        fge i@(H1 x xs) rest
          | x >= y = union i rest
          | otherwise = fge xs rest
        fge i@(H2 x a xs) rest
          | x >= y = union i rest
          | otherwise = fge a (fge xs rest)

partitionLT_GE :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GE _ E = (E,E)
partitionLT_GE y h@(H1 x xs)
  | x < y = let (xs',xs'') = partitionLT_GE y xs
            in (H1 x xs',xs'')
  | otherwise = (E, h)
partitionLT_GE y h@(H2 x a xs)
  | x < y = let (a',a'') = partitionLT_GE y a
                (xs',xs'') = partitionLT_GE y xs
            in (makeH2 x a' xs',union a'' xs'')
  | otherwise = (E, h)

partitionLE_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLE_GT _ E = (E,E)
partitionLE_GT y h@(H1 x xs)
  | x <= y = let (xs',xs'') = partitionLE_GT y xs
             in (H1 x xs',xs'')
  | otherwise = (E, h)
partitionLE_GT y h@(H2 x a xs)
  | x <= y = let (a',a'') = partitionLE_GT y a
                 (xs',xs'') = partitionLE_GT y xs
             in (makeH2 x a' xs',union a'' xs'')
  | otherwise = (E, h)

partitionLT_GT :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GT _ E = (E,E)
partitionLT_GT y h@(H1 x xs) =
  case compare x y of
    LT -> let (xs',xs'') = partitionLT_GT y xs
          in (H1 x xs',xs'')
    EQ -> (E, filterGT y xs)
    GT -> (E, h)
partitionLT_GT y h@(H2 x a xs) =
  case compare x y of
    LT -> let (a',a'') = partitionLT_GT y a
              (xs',xs'') = partitionLT_GT y xs
          in (makeH2 x a' xs',union a'' xs'')
    EQ -> (E, union (filterGT y a) (filterGT y xs))
    GT -> (E, h)

toSeq :: S.Sequence seq => Heap a -> seq a
toSeq h = tol h S.empty
  where tol E rest = rest
        tol (H1 x xs) rest = S.lcons x (tol xs rest)
        tol (H2 x i xs) rest = S.lcons x $ tol i $ tol xs rest

fold :: (a -> b -> b) -> b -> Heap a -> b
fold _ c E = c
fold f c (H1 x xs) = f x (fold f c xs)
fold f c (H2 x h xs) = f x (fold f (fold f c xs) h)

fold' :: (a -> b -> b) -> b -> Heap a -> b
fold' _ c E = c
fold' f c (H1 x xs)   = c `seq` f x $! (fold' f c xs)
fold' f c (H2 x h xs) = c `seq` f x $! (fold' f (fold' f c xs) h)


fold1 :: (a -> a -> a) -> Heap a -> a
fold1 _ E = error "LazyPairingHeap.fold1: empty heap"
fold1 f (H1 x xs) = fold f x xs
fold1 f (H2 x h xs) = fold f (fold f x xs) h

fold1' :: (a -> a -> a) -> Heap a -> a
fold1' _ E = error "LazyPairingHeap.fold1': empty heap"
fold1' f (H1 x xs)   = fold' f x xs
fold1' f (H2 x h xs) = fold' f (fold' f x xs) h


filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter _ E = E
filter p (H1 x xs) = if p x then H1 x (filter p xs) else filter p xs
filter p (H2 x h xs) =
  if p x then makeH2 x (filter p h) (filter p xs)
         else union (filter p h) (filter p xs)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition _ E = (E, E)
partition p (H1 x xs) = if p x then (H1 x xs',xs'') else (xs',H1 x xs'')
    where (xs',xs'') = partition p xs
partition p (H2 x h xs) =
  if p x then (makeH2 x h' xs', union h'' xs'')
         else (union h' xs', makeH2 x h'' xs'')
    where (h',h'') = partition p h
          (xs',xs'') = partition p xs

lookupAll :: (Ord a,S.Sequence seq) => a -> Heap a -> seq a
lookupAll y h = look h S.empty
  where look E rest = rest
        look (H1 x xs) rest =
          case compare x y of
            LT -> look xs rest
            EQ -> S.lcons x (look xs rest)
            GT -> rest
        look (H2 x i xs) rest =
          case compare x y of
            LT -> look i $ look xs rest
            EQ -> S.lcons x $ look i $ look xs rest
            GT -> rest

minView :: (Ord a, Fail.MonadFail m) => Heap a -> m (a, Heap a)
minView E = fail "LazyPairingHeap.minView: empty heap"
minView (H1 x xs) = return (x,xs)
minView (H2 x h xs) = return (x,union h xs)

minElem :: Heap a -> a
minElem E = error "LazyPairingHeap.minElem: empty heap"
minElem (H1 x _) = x
minElem (H2 x _ _) = x

maxView :: (Ord a, Fail.MonadFail m) => Heap a -> m (a, Heap a)
maxView E = fail "LazyPairingHeap.maxView: empty heap"
maxView xs = return (y,xs')
  where (xs', y) = maxView' xs

-- not exported
maxView' :: (Ord a) => Heap a -> (Heap a, a)
maxView' (H1 x E) = (E, x)
maxView' (H1 x xs) = (H1 x xs', y)
  where (xs', y) = maxView' xs
maxView' (H2 x a E) = (H1 x a', y)
  where (a', y) = maxView' a
maxView' (H2 x a xs) =
    if y > z then (makeH2 x a' xs, y) else (H2 x a xs', z)
  where (a', y) = maxView' a
        (xs', z) = maxView' xs
maxView' E = error "LazyPairingHeap.maxView': bug!"

maxElem :: Ord a => Heap a -> a
maxElem E = error "LazyPairingHeap.maxElem: empty heap"
maxElem (H1 x E) = x
maxElem (H1 _ xs) = maxElem xs
maxElem (H2 _ h E) = maxElem h
maxElem (H2 _ h xs) = max (maxElem h) (maxElem xs)

foldr :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr _ c E = c
foldr f c (H1 x xs) = f x (foldr f c xs)
foldr f c (H2 x h xs) = f x (foldr f c (union h xs))

foldr' :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr' _ c E = c
foldr' f c (H1 x xs)   = c `seq` f x $! (foldr' f c xs)
foldr' f c (H2 x h xs) = c `seq` f x $! (foldr' f c (union h xs))

foldl :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl _ c E = c
foldl f c (H1 x xs) = foldl f (f c x) xs
foldl f c (H2 x h xs) = foldl f (f c x) (union h xs)

foldl' :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl' _ c E = c
foldl' f c (H1 x xs)   = c `seq` foldl' f (f c x) xs
foldl' f c (H2 x h xs) = c `seq` foldl' f (f c x) (union h xs)

foldr1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1 _ E = error "LazyPairingHeap.foldr1: empty heap"
foldr1 _ (H1 x E) = x
foldr1 f (H1 x xs) = f x (foldr1 f xs)
foldr1 f (H2 x h xs) = f x (foldr1 f (union h xs))

foldr1' :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1' _ E = error "LazyPairingHeap.foldr1': empty heap"
foldr1' _ (H1 x E)    = x
foldr1' f (H1 x xs)   = f x $! (foldr1' f xs)
foldr1' f (H2 x h xs) = f x $! (foldr1' f (union h xs))

foldl1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1 _ E = error "LazyPairingHeap.foldl1: empty heap"
foldl1 f (H1 x xs) = foldl f x xs
foldl1 f (H2 x h xs) = foldl f x (union h xs)

foldl1' :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1' _ E = error "LazyPairingHeap.foldl1': empty heap"
foldl1' f (H1 x xs)   = foldl' f x xs
foldl1' f (H2 x h xs) = foldl' f x (union h xs)

unsafeMapMonotonic :: (Ord a,Ord b) => (a -> b) -> Heap a -> Heap b
unsafeMapMonotonic = mapm
  where mapm _ E = E
        mapm f (H1 x xs) = H1 (f x) (mapm f xs)
        mapm f (H2 x h xs) = H2 (f x) (mapm f h) (mapm f xs)


strict :: Heap a -> Heap a
strict h@E = h
strict h@(H1 _ xs) = strict xs `seq` h
strict h@(H2 _ h' xs) = strict h' `seq` strict xs `seq` h

strictWith :: (a -> b) -> Heap a -> Heap a
strictWith _ h@E = h
strictWith f h@(H1 x xs) = f x `seq` strictWith f xs `seq` h
strictWith f h@(H2 x h' xs) = f x `seq` strictWith f h' `seq` strictWith f xs `seq` h


-- the remaining functions all use default definitions

fromSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
fromSeq = fromSeqUsingFoldr

insertSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a -> Heap a
insertSeq = insertSeqUsingFoldr

unionSeq :: (Ord a,S.Sequence seq) => seq (Heap a) -> Heap a
unionSeq = unionSeqUsingFoldl

unsafeFromOrdSeq :: (Ord a,S.Sequence seq) => seq a -> Heap a
unsafeFromOrdSeq = unsafeFromOrdSeqUsingUnsafeInsertMin

deleteMax :: Ord a => Heap a -> Heap a
deleteMax = deleteMaxUsingMaxView

lookup :: Ord a => a -> Heap a -> a
lookup = lookupUsingLookupAll

lookupM :: (Ord a, Fail.MonadFail m) => a -> Heap a -> m a
lookupM = lookupMUsingLookupAll

lookupWithDefault :: Ord a => a -> a -> Heap a -> a
lookupWithDefault = lookupWithDefaultUsingLookupAll

toOrdSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toOrdSeq = toOrdSeqUsingFoldr

-- instance declarations

instance Ord a => C.CollX (Heap a) a where
  {singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; unionSeq = unionSeq;
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   strict = strict;
   structuralInvariant = structuralInvariant; instanceName _ = moduleName}

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
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1;
   foldr1' = foldr1'; foldl1 = foldl1; foldl1' = foldl1';
   toOrdSeq = toOrdSeq; unsafeMapMonotonic = unsafeMapMonotonic}

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
                       (2, liftM2 sift1 arbitrary (arbTree (n - 1))),
                       (3, liftM3 sift arbitrary (arbTree (n `div` 4))
                                                 (arbTree (n `div` 2)))]

          sift x E a = sift1 x a
          sift x a E = case sift1 x a of
            H1 x' a' -> H2 x' a' E
            _ -> undefined
          sift x a b
              | x <= ma && x <= mb = H2 x a b
              | ma < x && ma <= mb = H2 ma (siftInto x a) b
              | otherwise          = H2 mb a (siftInto x b)
            where ma = minElem a
                  mb = minElem b

          sift1 x E = H1 x E
          sift1 x a
              | x <= ma   = H1 x a
              | otherwise = H1 ma (siftInto x a)
            where ma = minElem a

          siftInto x (H1 _ a) = sift1 x a
          siftInto x (H2 _ a b) = sift x a b
          siftInto _ E = error "LazyPairingHeap.arbitrary: bug!"

instance (Ord a, CoArbitrary a) => CoArbitrary (Heap a) where
  coarbitrary E = variant (0 :: Int)
  coarbitrary (H1 x a) = variant (1 :: Int) . coarbitrary x . coarbitrary a
  coarbitrary (H2 x a b) =
      variant (2 :: Int) . coarbitrary x . coarbitrary a . coarbitrary b

instance (Ord a) => Semigroup (Heap a) where
    (<>) = union

instance (Ord a) => Monoid (Heap a) where
    mempty  = empty
    mappend = (SG.<>)
    mconcat = unionSeq

instance (Ord a) => Ord (Heap a) where
    compare = compareUsingToOrdList
