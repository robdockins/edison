-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Coll.LazyPairingHeap (
    -- type of pairing heaps
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
import Data.Edison.EdisonPrelude
import qualified Data.Edison.Coll.Collection as C ( CollX(..), OrdCollX(..),
				   Coll(..), OrdColl(..), toOrdList )
import qualified Data.Edison.Seq.Sequence as S
import Data.Edison.Coll.CollectionDefaults
import Data.List(sort)
import Control.Monad
import Test.QuickCheck

moduleName = "LazyPairingHeap"

-- Adapted from
--   Chris Okasaki. Purely Functional Data Structures. 1998.
--   Section 6.5.

data Heap a = E 
            | H1 a (Heap a)
            | H2 a !(Heap a) (Heap a)
  -- Invariant: left child of H2 not empty

-- second arg is not empty
-- not used!
link E h = h
link (H1 x b) a = H2 x a b
link (H2 x a b) a' = H1 x (union (union a a') b)

makeH2 x E xs = H1 x xs
makeH2 x h xs = H2 x h xs

empty :: Heap a
empty = E

single :: a -> Heap a
single x = H1 x E

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
union hx@(H1 x xs) E = hx
union hx@(H1 x xs) hy@(H1 y ys)
  | x <= y    = H2 x hy xs
  | otherwise = H2 y hx ys
union hx@(H1 x xs) hy@(H2 y a ys)
  | x <= y    = H2 x hy xs
  | otherwise = H1 y (union (union hx a) ys)
union hx@(H2 x a xs) E = hx
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
                    Just xs -> Just (H1 x xs)
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
deleteAll y E = E
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

        del y ys E = E
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
size (H1 x xs) = 1 + size xs
size (H2 x h xs) = 1 + size h + size xs

member :: Ord a => Heap a -> a -> Bool
member E x = False
member (H1 y ys) x =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member ys x
member (H2 y h ys) x =
  case compare x y of
    LT -> False
    EQ -> True
    GT -> member h x || member ys x

count :: Ord a => Heap a -> a -> Int
count E x = 0
count (H1 y ys) x =
  case compare x y of
    LT -> 0
    EQ -> 1 + count ys x
    GT -> count ys x
count (H2 y h ys) x =
  case compare x y of
    LT -> 0
    EQ -> 1 + count h x + count ys x
    GT -> count h x + count ys x

deleteMin :: Ord a => Heap a -> Heap a
deleteMin E = E
deleteMin (H1 x xs) = xs
deleteMin (H2 x h xs) = union h xs

unsafeInsertMin :: Ord a => a -> Heap a -> Heap a
unsafeInsertMin = H1

unsafeInsertMax :: Ord a => Heap a -> a -> Heap a
unsafeInsertMax E x = H1 x E
unsafeInsertMax (H1 y ys) x = H2 y (H1 x E) ys
unsafeInsertMax (H2 y h ys) x = H1 y (union (unsafeInsertMax h x) ys)

unsafeAppend :: Ord a => Heap a -> Heap a -> Heap a
unsafeAppend h E = h
unsafeAppend E h = h
unsafeAppend (H1 x xs) h = H2 x h xs
unsafeAppend (H2 x a xs) h = H1 x (union (unsafeAppend a h) xs)

filterLT :: Ord a => a -> Heap a -> Heap a
filterLT y E = E
filterLT y (H1 x xs)
  | x < y = H1 x (filterLT y xs)
  | otherwise = E
filterLT y (H2 x h xs)
  | x < y = makeH2 x (filterLT y h) (filterLT y xs)
  | otherwise = E

filterLE :: Ord a => a -> Heap a -> Heap a
filterLE y E = E
filterLE y (H1 x xs)
  | x <= y = H1 x (filterLE y xs)
  | otherwise = E
filterLE y (H2 x h xs)
  | x <= y = makeH2 x (filterLE y h) (filterLE y xs)
  | otherwise = E

filterGT :: Ord a => a -> Heap a -> Heap a
filterGT y h = fgt h E
  where fgt E rest = rest
        fgt h@(H1 x xs) rest
          | x > y = union h rest
          | otherwise = fgt xs rest
        fgt h@(H2 x a xs) rest
          | x > y = union h rest
          | otherwise = fgt a (fgt xs rest)

filterGE :: Ord a => a -> Heap a -> Heap a
filterGE y h = fge h E
  where fge E rest = rest
        fge h@(H1 x xs) rest
          | x >= y = union h rest
          | otherwise = fge xs rest
        fge h@(H2 x a xs) rest
          | x >= y = union h rest
          | otherwise = fge a (fge xs rest)

partitionLT_GE :: Ord a => a -> Heap a -> (Heap a, Heap a)
partitionLT_GE y E = (E,E)
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
partitionLE_GT y E = (E,E)
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
partitionLT_GT y E = (E,E)
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
        tol (H2 x h xs) rest = S.lcons x (tol h (tol xs rest))

fold :: (a -> b -> b) -> b -> Heap a -> b
fold f c E = c
fold f c (H1 x xs) = f x (fold f c xs)
fold f c (H2 x h xs) = f x (fold f (fold f c xs) h)

fold1 :: (a -> a -> a) -> Heap a -> a
fold1 f E = error "LazyPairingHeap.fold1: empty heap"
fold1 f (H1 x xs) = fold f x xs
fold1 f (H2 x h xs) = fold f (fold f x xs) h

filter :: Ord a => (a -> Bool) -> Heap a -> Heap a
filter p E = E
filter p (H1 x xs) = if p x then H1 x (filter p xs) else filter p xs
filter p (H2 x h xs) =
  if p x then makeH2 x (filter p h) (filter p xs)
         else union (filter p h) (filter p xs)

partition :: Ord a => (a -> Bool) -> Heap a -> (Heap a, Heap a)
partition p E = (E, E)
partition p (H1 x xs) = if p x then (H1 x xs',xs'') else (xs',H1 x xs'')
    where (xs',xs'') = partition p xs
partition p (H2 x h xs) =
  if p x then (makeH2 x h' xs', union h'' xs'')
         else (union h' xs', makeH2 x h'' xs'')
    where (h',h'') = partition p h
          (xs',xs'') = partition p xs

lookupAll :: (Ord a,S.Sequence seq) => Heap a -> a -> seq a
lookupAll h y = look h S.empty
  where look E rest = rest
        look (H1 x xs) rest =
          case compare x y of
            LT -> look xs rest
            EQ -> S.lcons x (look xs rest)
            GT -> rest
        look (H2 x h xs) rest =
          case compare x y of
            LT -> look h (look xs rest)
            EQ -> S.lcons x (look h (look xs rest))
            GT -> rest

minView :: (Ord a, Monad m) => Heap a -> m (a, Heap a)
minView E = fail "LazyPairingHeap.minView: empty heap"
minView (H1 x xs) = return (x,xs)
minView (H2 x h xs) = return (x,union h xs)

minElem :: Heap a -> a
minElem E = error "LazyPairingHeap.minElem: empty heap"
minElem (H1 x xs) = x
minElem (H2 x h xs) = x

maxView :: (Ord a, Monad m) => Heap a -> m (Heap a, a)
maxView E = fail "LazyPairingHeap.maxView: empty heap"
maxView xs = return (xs',y)
  where (xs', y) = maxView' xs

-- not exported
maxView' (H1 x E) = (E, x)
maxView' (H1 x xs) = (H1 x xs', y)
  where (xs', y) = maxView' xs
maxView' (H2 x a E) = (H1 x a', y)
  where (a', y) = maxView' a
maxView' (H2 x a xs) = 
    if y > z then (makeH2 x a' xs, y) else (H2 x a xs', z)
  where (a', y) = maxView' a
        (xs', z) = maxView' xs

maxElem :: Ord a => Heap a -> a
maxElem E = error "LazyPairingHeap.maxElem: empty heap"
maxElem (H1 x E) = x
maxElem (H1 x xs) = maxElem xs
maxElem (H2 x h E) = maxElem h
maxElem (H2 x h xs) = max (maxElem h) (maxElem xs)

foldr :: Ord a => (a -> b -> b) -> b -> Heap a -> b
foldr f c E = c
foldr f c (H1 x xs) = f x (foldr f c xs)
foldr f c (H2 x h xs) = f x (foldr f c (union h xs))

foldl :: Ord a => (b -> a -> b) -> b -> Heap a -> b
foldl f c E = c
foldl f c (H1 x xs) = foldl f (f c x) xs
foldl f c (H2 x h xs) = foldl f (f c x) (union h xs)

foldr1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldr1 f E = error "LazyPairingHeap.foldr1: empty heap"
foldr1 f (H1 x E) = x
foldr1 f (H1 x xs) = f x (foldr1 f xs)
foldr1 f (H2 x h xs) = f x (foldr1 f (union h xs))

foldl1 :: Ord a => (a -> a -> a) -> Heap a -> a
foldl1 f E = error "LazyPairingHeap.foldl1: empty heap"
foldl1 f (H1 x xs) = foldl f x xs
foldl1 f (H2 x h xs) = foldl f x (union h xs)

unsafeMapMonotonic :: (Ord a,Ord b) => (a -> b) -> Heap a -> Heap b
unsafeMapMonotonic = mapm
  where mapm f E = E
        mapm f (H1 x xs) = H1 (f x) (mapm f xs)
        mapm f (H2 x h xs) = H2 (f x) (mapm f h) (mapm f xs)

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

lookup :: Ord a => Heap a -> a -> a
lookup = lookupUsingLookupAll

lookupM :: (Ord a, Monad m) => Heap a -> a -> m a
lookupM = lookupMUsingLookupAll

lookupWithDefault :: Ord a => a -> Heap a -> a -> a
lookupWithDefault = lookupWithDefaultUsingLookupAll

toOrdSeq :: (Ord a,S.Sequence seq) => Heap a -> seq a
toOrdSeq = toOrdSeqUsingFoldr

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

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = sized (\n -> arbTree n)
    where arbTree 0 = return E
          arbTree n =
            frequency [(1, return E),
                       (2, liftM2 sift1 arbitrary (arbTree (n - 1))),
                       (3, liftM3 sift arbitrary (arbTree (n `div` 4))
                                                 (arbTree (n `div` 2)))]

          sift x E a = sift1 x a
          sift x a E = let H1 x' a' = sift1 x a in H2 x' a' E
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

  coarbitrary E = variant 0
  coarbitrary (H1 x a) = variant 1 . coarbitrary x . coarbitrary a
  coarbitrary (H2 x a b) =
      variant 2 . coarbitrary x . coarbitrary a . coarbitrary b
