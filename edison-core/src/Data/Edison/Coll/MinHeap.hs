-- |
--   Module      :  Data.Edison.Coll.MinHeap
--   Copyright   :  Copyright (c) 1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   A generic adaptor for bags to keep the minimum element separately.

module Data.Edison.Coll.MinHeap (
    -- * Min heap adaptor type
    Min, -- instance of Coll/CollX, OrdColl/OrdCollX

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

    -- * Other supported operations
    toColl,fromColl,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq as S
import Data.Edison.Coll.Defaults
import Data.Edison.Seq.Defaults (tokenMatch,maybeParens)
import Data.Monoid
import qualified Data.Semigroup as SG
import Control.Monad
import qualified Control.Monad.Fail as MF
import Test.QuickCheck

data Min h a = E | M a h  deriving (Eq)

moduleName :: String
moduleName = "Data.Edison.Coll.MinHeap"

structuralInvariant :: (Ord a,C.OrdColl h a) => Min h a -> Bool
structuralInvariant E = True
structuralInvariant (M x h) = if C.null h then True else x <= C.minElem h

empty     :: Min h a
singleton :: (C.CollX h a,Ord a) => a -> Min h a
fromSeq   :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a
insert    :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
insertSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a -> Min h a
union     :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a -> Min h a
unionSeq  :: (C.OrdColl h a,Ord a,S.Sequence s) => s (Min h a) -> Min h a
delete    :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
deleteAll :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
deleteSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => s a -> Min h a -> Min h a
null      :: Min h a -> Bool
size      :: C.CollX h a => Min h a -> Int
member    :: (C.CollX h a,Ord a) => a -> Min h a -> Bool
count     :: (C.CollX h a,Ord a) => a -> Min h a -> Int
strict    :: (C.CollX h a,Ord a) => Min h a -> Min h a

toSeq     :: (C.Coll h a,S.Sequence s) => Min h a -> s a
lookup    :: (C.Coll h a,Ord a) => a -> Min h a -> a
lookupM   :: (C.Coll h a, Ord a, MF.MonadFail m) => a -> Min h a -> m a
lookupAll :: (C.Coll h a,Ord a,S.Sequence s) => a -> Min h a -> s a
lookupWithDefault :: (C.Coll h a,Ord a) => a -> a -> Min h a -> a
fold      :: (C.Coll h a) => (a -> b -> b) -> b -> Min h a -> b
fold1     :: (C.Coll h a) => (a -> a -> a) -> Min h a -> a
fold'     :: (C.Coll h a) => (a -> b -> b) -> b -> Min h a -> b
fold1'    :: (C.Coll h a) => (a -> a -> a) -> Min h a -> a
filter    :: (C.OrdColl h a) => (a -> Bool) -> Min h a -> Min h a
partition :: (C.OrdColl h a) => (a -> Bool) -> Min h a -> (Min h a, Min h a)
strictWith :: (C.OrdColl h a) => (a -> b) -> Min h a -> Min h a

deleteMin :: (C.OrdColl h a,Ord a) => Min h a -> Min h a
deleteMax :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a
unsafeInsertMin :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
unsafeInsertMax :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
unsafeFromOrdSeq :: (C.OrdCollX h a,Ord a,S.Sequence s) => s a -> Min h a
unsafeAppend :: (C.OrdCollX h a,Ord a) => Min h a -> Min h a -> Min h a
filterLT :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
filterLE :: (C.OrdCollX h a,Ord a) => a -> Min h a -> Min h a
filterGT :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
filterGE :: (C.OrdColl h a,Ord a) => a -> Min h a -> Min h a
partitionLT_GE :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)
partitionLE_GT :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)
partitionLT_GT :: (C.OrdColl h a,Ord a) => a -> Min h a -> (Min h a, Min h a)

minView :: (C.OrdColl h a, Ord a, MF.MonadFail m) => Min h a -> m (a, Min h a)
minElem :: (C.OrdColl h a,Ord a) => Min h a -> a
maxView :: (C.OrdColl h a, Ord a, MF.MonadFail m) => Min h a -> m (a, Min h a)
maxElem :: (C.OrdColl h a,Ord a) => Min h a -> a
foldr :: (C.OrdColl h a,Ord a) => (a -> b -> b) -> b -> Min h a -> b
foldl :: (C.OrdColl h a,Ord a) => (b -> a -> b) -> b -> Min h a -> b
foldr1 :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
foldl1 :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
foldr' :: (C.OrdColl h a,Ord a) => (a -> b -> b) -> b -> Min h a -> b
foldl' :: (C.OrdColl h a,Ord a) => (b -> a -> b) -> b -> Min h a -> b
foldr1' :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
foldl1' :: (C.OrdColl h a,Ord a) => (a -> a -> a) -> Min h a -> a
toOrdSeq :: (C.OrdColl h a,Ord a,S.Sequence s) => Min h a -> s a
unsafeMapMonotonic :: (C.OrdColl h a,Ord a) =>
      (a -> a) -> Min h a -> Min h a

fromColl :: C.OrdColl h a => h -> Min h a
fromColl = fromPrim

toColl :: C.OrdColl h a => Min h a -> h
toColl = toPrim

fromPrim :: (C.OrdColl c a) => c -> Min c a
fromPrim xs = case C.minView xs of
                Nothing -> E
                Just (x, xs') -> M x xs'

toPrim :: (C.OrdCollX c a) => Min c a -> c
toPrim E = C.empty
toPrim (M x xs) = C.unsafeInsertMin x xs

empty = E
singleton x = M x C.empty

fromSeq = fromPrim . C.fromSeq

insert x E = M x C.empty
insert x (M y xs)
  | x <= y    = M x (C.unsafeInsertMin y xs)
  | otherwise = M y (C.insert x xs)

insertSeq xs E = fromSeq xs
insertSeq xs (M y ys) =
    case C.minView xs_ys of
      Nothing -> M y C.empty
      Just (x, rest)
          | x < y     -> M x (C.insert y rest)
          | otherwise -> M y xs_ys
  where xs_ys = C.insertSeq xs ys

union E ys = ys
union xs E = xs
union (M x xs) (M y ys)
  | x <= y    = M x (C.union xs (C.unsafeInsertMin y ys))
  | otherwise = M y (C.union (C.unsafeInsertMin x xs) ys)

unionSeq = unionSeqUsingReduce

delete _ E = E
delete x m@(M y ys)
  | x > y     = M y (C.delete x ys)
  | x == y    = fromPrim ys
  | otherwise = m

deleteAll _ E = E
deleteAll x m@(M y ys)
  | x > y     = M y (C.deleteAll x ys)
  | x == y    = fromPrim (C.deleteAll x ys)
  | otherwise = m

deleteSeq = deleteSeqUsingDelete

null E = True
null (M _ _) = False

size E = 0
size (M _ xs) = 1 + C.size xs


member _ E = False
member x (M y ys)
  | x > y     = C.member x ys
  | otherwise = (x == y)

count _ E = 0
count x (M y ys)
  | x > y     = C.count x ys
  | x == y    = 1 + C.count x ys
  | otherwise = 0

toSeq E = S.empty
toSeq (M x xs) = S.lcons x (C.toSeq xs)

lookup x (M y ys)
  | x > y  = C.lookup x ys
  | x == y = y
lookup _ _ = error "MinHeap.lookup: empty heap"

lookupM x (M y ys)
  | x > y  = C.lookupM x ys
  | x == y = return y
lookupM _ _ = fail "lookupM.lookup: XXX"

lookupAll x (M y ys)
  | x > y  = C.lookupAll x ys
  | x == y = S.lcons y (C.lookupAll x ys)
lookupAll _ _ = S.empty

lookupWithDefault d x (M y ys)
  | x > y  = C.lookupWithDefault d x ys
  | x == y = y
lookupWithDefault d _ _ = d

fold _ e E = e
fold f e (M x xs) = f x (C.fold f e xs)

fold' _ e E = e
fold' f e (M x xs) = f x $! (C.fold' f e xs)

fold1 _ E = error "MinHeap.fold1: empty heap"
fold1 f (M x xs) = C.fold f x xs

fold1' _ E = error "MinHeap.fold1': empty heap"
fold1' f (M x xs) = C.fold' f x xs

filter _ E = E
filter p (M x xs)
  | p x       = M x (C.filter p xs)
  | otherwise = fromPrim (C.filter p xs)

partition _ E = (E, E)
partition p (M x xs)
    | p x       = (M x ys, fromPrim zs)
    | otherwise = (fromPrim ys, M x zs)
  where (ys,zs) = C.partition p xs

deleteMin E = E
deleteMin (M _ xs) = fromPrim xs

deleteMax E = E
deleteMax (M x xs)
  | C.null xs   = E
  | otherwise = M x (C.deleteMax xs)

unsafeInsertMin x xs = M x (toPrim xs)

unsafeInsertMax x E = M x C.empty
unsafeInsertMax x (M y ys) = M y (C.unsafeInsertMax x ys)

unsafeFromOrdSeq xs =
  case S.lview xs of
    Nothing      -> E
    Just (x,xs') -> M x (C.unsafeFromOrdSeq xs')

unsafeAppend E ys = ys
unsafeAppend (M x xs) ys = M x (C.unsafeAppend xs (toPrim ys))

filterLT x (M y ys) | y < x  = M y (C.filterLT x ys)
filterLT _ _ = E

filterLE x (M y ys) | y <= x = M y (C.filterLE x ys)
filterLE _ _ = E

filterGT x (M y ys) | y <= x = fromPrim (C.filterGT x ys)
filterGT _ h = h

filterGE x (M y ys) | y < x  = fromPrim (C.filterGE x ys)
filterGE _ h = h

partitionLT_GE x (M y ys)
  | y < x = (M y lows, fromPrim highs)
  where (lows,highs) = C.partitionLT_GE x ys
partitionLT_GE _ h = (E, h)

partitionLE_GT x (M y ys)
  | y <= x = (M y lows, fromPrim highs)
  where (lows,highs) = C.partitionLE_GT x ys
partitionLE_GT _ h = (E, h)

partitionLT_GT x (M y ys)
  | y < x  = let (lows,highs) = C.partitionLT_GT x ys
             in (M y lows, fromPrim highs)
  | y == x = (E, fromPrim (C.filterGT x ys))
partitionLT_GT _ h = (E, h)


minView E = fail "MinHeap.minView: empty heap"
minView (M x xs) = return (x, fromPrim xs)

minElem E = error "MinHeap.minElem: empty heap"
minElem (M x _) = x

maxView E = fail "MinHeap.maxView: empty heap"
maxView (M x xs) = case C.maxView xs of
                     Nothing     -> return (x, E)
                     Just (y,ys) -> return (y, M x ys)

maxElem E = error "MinHeap.minElem: empty heap"
maxElem (M x xs)
  | C.null xs   = x
  | otherwise = C.maxElem xs

foldr _ e E = e
foldr f e (M x xs) = f x (C.foldr f e xs)

foldr' _ e E = e
foldr' f e (M x xs) = f x $! (C.foldr' f e xs)

foldl _ e E = e
foldl f e (M x xs) = C.foldl f (f e x) xs

foldl' _ e E = e
foldl' f e (M x xs) = e `seq` C.foldl' f (f e x) xs

foldr1 _ E = error "MinHeap.foldr1: empty heap"
foldr1 f (M x xs)
  | C.null xs   = x
  | otherwise = f x (C.foldr1 f xs)

foldr1' _ E = error "MinHeap.foldr1': empty heap"
foldr1' f (M x xs)
  | C.null xs = x
  | otherwise = f x $! (C.foldr1' f xs)

foldl1 _ E = error "MinHeap.foldl1: empty heap"
foldl1 f (M x xs) = C.foldl f x xs

foldl1' _ E = error "MinHeap.foldl1': empty heap"
foldl1' f (M x xs) = C.foldl' f x xs

toOrdSeq E = S.empty
toOrdSeq (M x xs) = S.lcons x (C.toOrdSeq xs)

unsafeMapMonotonic = unsafeMapMonotonicUsingFoldr

strict h@E = h
strict h@(M _ xs) = C.strict xs `seq` h

strictWith _ h@E = h
strictWith f h@(M x xs) = f x `seq` C.strictWith f xs `seq` h


-- instance declarations

instance (C.OrdColl h a, Ord a) => C.CollX (Min h a) a where
  {singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; unionSeq = unionSeq;
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   strict = strict;
   structuralInvariant = structuralInvariant; instanceName _ = moduleName}

instance (C.OrdColl h a, Ord a) => C.OrdCollX (Min h a) a where
  {deleteMin = deleteMin; deleteMax = deleteMax;
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax;
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT;
   filterGE = filterGE; partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance (C.OrdColl h a, Ord a) => C.Coll (Min h a) a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM;
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault;
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   filter = filter; partition = partition; strictWith = strictWith}

instance (C.OrdColl h a, Ord a) => C.OrdColl (Min h a) a where
  {minView = minView; minElem = minElem; maxView = maxView;
   maxElem = maxElem; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1;  foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic}

-- instance Eq is derived

instance (C.OrdColl h a, Show h) => Show (Min h a) where
   showsPrec i xs rest
     | i == 0    = concat [    moduleName,".fromColl ",showsPrec 10 (toColl xs) rest]
     | otherwise = concat ["(",moduleName,".fromColl ",showsPrec 10 (toColl xs) (')':rest)]

instance (C.OrdColl h a, Read h) => Read (Min h a) where
   readsPrec _ xs = maybeParens p xs
       where p ys = tokenMatch (moduleName++".fromColl") ys
                      >>= readsPrec 10
                      >>= \(coll,rest) -> return (fromColl coll,rest)

instance (C.OrdColl h a,Arbitrary h,Arbitrary a) => Arbitrary (Min h a) where
  arbitrary = do xs <- arbitrary
                 x  <- arbitrary
                 i  <- arbitrary :: Gen Int
                 return (if C.null xs || x <= C.minElem xs then M x xs
                         else if odd i then M (C.minElem xs) xs
                                       else fromPrim xs)

instance (C.OrdColl h a,CoArbitrary h,CoArbitrary a) => CoArbitrary (Min h a) where
  coarbitrary E = variant 0
  coarbitrary (M x xs) = variant 1 . coarbitrary x . coarbitrary xs

instance (C.OrdColl h a) => SG.Semigroup (Min h a) where
    (<>) = union
instance (C.OrdColl h a) => Monoid (Min h a) where
    mempty  = empty
    mappend = (SG.<>)
    mconcat = unionSeq

instance (Eq h, C.OrdColl h a) => Ord (Min h a) where
    compare = compareUsingToOrdList
