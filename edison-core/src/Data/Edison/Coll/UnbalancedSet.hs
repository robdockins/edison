-- |
--   Module      :  Data.Edison.Coll.UnbalancedSet
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Sets implemented as unbalanced binary search trees.

module Data.Edison.Coll.UnbalancedSet (
    -- * Set type
    Set, -- instance of Coll/CollX, OrdColl/OrdCollX, Set/SetX, OrdSet/OrdSetX

    -- * CollX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,strict,structuralInvariant,

    -- * Coll operations
    toSeq,lookup,lookupM,lookupAll,lookupWithDefault,fold,fold',
    fold1,fold1',filter,partition,strictWith,

    -- * OrdCollX operations
    deleteMin,deleteMax,unsafeInsertMin,unsafeInsertMax,unsafeFromOrdSeq,
    unsafeAppend,filterLT,filterLE,filterGT,filterGE,partitionLT_GE,
    partitionLE_GT,partitionLT_GT,

    -- * OrdColl operations
    minView,minElem,maxView,maxElem,foldr,foldr',foldl,foldl',
    foldr1,foldr1',foldl1,foldl1',toOrdSeq,unsafeMapMonotonic,

    -- * SetX operations
    intersection,difference,symmetricDifference,properSubset,subset,

    -- * Set operations
    fromSeqWith,insertWith,insertSeqWith,unionl,unionr,unionWith,
    unionSeqWith,intersectionWith,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,foldl',lookup,filter)
import qualified Prelude
import qualified Control.Monad.Fail as Fail
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq as S
import Data.Edison.Coll.Defaults
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Semigroup as SG
import Test.QuickCheck

-- signatures for exported functions
moduleName :: String
empty      :: Set a
singleton  :: a -> Set a
fromSeq    :: (Ord a,S.Sequence seq) => seq a -> Set a
insert     :: Ord a => a -> Set a -> Set a
insertSeq  :: (Ord a,S.Sequence seq) => seq a -> Set a -> Set a
union      :: Ord a => Set a -> Set a -> Set a
unionSeq   :: (Ord a,S.Sequence seq) => seq (Set a) -> Set a
delete     :: Ord a => a -> Set a -> Set a
deleteAll  :: Ord a => a -> Set a -> Set a
deleteSeq  :: (Ord a,S.Sequence seq) => seq a -> Set a -> Set a
null       :: Set a -> Bool
size       :: Set a -> Int
member     :: Ord a => a -> Set a -> Bool
count      :: Ord a => a -> Set a -> Int
strict     :: Set a -> Set a

toSeq      :: (Ord a,S.Sequence seq) => Set a -> seq a
lookup     :: Ord a => a -> Set a -> a
lookupM    :: (Ord a, Fail.MonadFail m) => a -> Set a -> m a
lookupAll  :: (Ord a,S.Sequence seq) => a -> Set a -> seq a
lookupWithDefault :: Ord a => a -> a -> Set a -> a
fold       :: (a -> b -> b) -> b -> Set a -> b
fold1      :: (a -> a -> a) -> Set a -> a
fold'      :: (a -> b -> b) -> b -> Set a -> b
fold1'     :: (a -> a -> a) -> Set a -> a
filter     :: Ord a => (a -> Bool) -> Set a -> Set a
partition  :: Ord a => (a -> Bool) -> Set a -> (Set a, Set a)
strictWith :: (a -> b) -> Set a -> Set a

deleteMin        :: Ord a => Set a -> Set a
deleteMax        :: Ord a => Set a -> Set a
unsafeInsertMin  :: Ord a => a -> Set a -> Set a
unsafeInsertMax  :: Ord a => a -> Set a -> Set a
unsafeFromOrdSeq :: (Ord a,S.Sequence seq) => seq a -> Set a
unsafeAppend     :: Ord a => Set a -> Set a -> Set a
filterLT         :: Ord a => a -> Set a -> Set a
filterLE         :: Ord a => a -> Set a -> Set a
filterGT         :: Ord a => a -> Set a -> Set a
filterGE         :: Ord a => a -> Set a -> Set a
partitionLT_GE   :: Ord a => a -> Set a -> (Set a, Set a)
partitionLE_GT   :: Ord a => a -> Set a -> (Set a, Set a)
partitionLT_GT   :: Ord a => a -> Set a -> (Set a, Set a)

minView       :: (Fail.MonadFail m) => Set a -> m (a, Set a)
minElem       :: Set a -> a
maxView       :: (Fail.MonadFail m) => Set a -> m (a, Set a)
maxElem       :: Set a -> a
foldr         :: (a -> b -> b) -> b -> Set a -> b
foldl         :: (b -> a -> b) -> b -> Set a -> b
foldr1        :: (a -> a -> a) -> Set a -> a
foldl1        :: (a -> a -> a) -> Set a -> a
foldr'        :: (a -> b -> b) -> b -> Set a -> b
foldl'        :: (b -> a -> b) -> b -> Set a -> b
foldr1'       :: (a -> a -> a) -> Set a -> a
foldl1'       :: (a -> a -> a) -> Set a -> a
toOrdSeq      :: (Ord a,S.Sequence seq) => Set a -> seq a

intersection  :: Ord a => Set a -> Set a -> Set a
difference    :: Ord a => Set a -> Set a -> Set a
symmetricDifference :: Ord a => Set a -> Set a -> Set a
properSubset  :: Ord a => Set a -> Set a -> Bool
subset        :: Ord a => Set a -> Set a -> Bool

fromSeqWith   :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq a -> Set a
insertWith    :: Ord a => (a -> a -> a) -> a -> Set a -> Set a
insertSeqWith :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq a -> Set a -> Set a
unionl       :: Ord a => Set a -> Set a -> Set a
unionr       :: Ord a => Set a -> Set a -> Set a
unionWith    :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unionSeqWith :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq (Set a) -> Set a
intersectionWith :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unsafeMapMonotonic :: Ord a => (a -> a) -> Set a -> Set a

moduleName = "Data.Edison.Coll.UnbalancedSet"

data Set a = E | T (Set a) a (Set a)

-- invariants:
--   * Binary Search Tree order
structuralInvariant :: Ord a => Set a -> Bool
structuralInvariant t = bounded Nothing Nothing t
   where bounded _ _ E = True
         bounded lo hi (T l x r)  = cmp_l lo x
                                 && cmp_r x hi
                                 && bounded lo (Just x) l
                                 && bounded (Just x) hi r

         cmp_l Nothing  _ = True
         cmp_l (Just x) y = x < y

         cmp_r _ Nothing  = True
         cmp_r x (Just y) = x < y



empty = E
singleton x = T E x E

insertWith c x = ins
  where ins E = T E x E
        ins (T a y b) =
          case compare x y of
            LT -> T (ins a) y b
            EQ -> T a (c x y) b
            GT -> T a y (ins b)

delete _ E = E
delete x (T a y b) =
  case compare x y of
    LT -> T (delete x a) y b
    EQ -> unsafeAppend a b
    GT -> T a y (delete x b)

null E = True
null (T _ _ _) = False

size t = sz t 0
  where sz E i = i
        sz (T a _ b) i = sz a (sz b (i+1))

member _ E = False
member x (T a y b) =
  case compare x y of
    LT -> member x a
    EQ -> True
    GT -> member x b

lookupM _ E = fail "UnbalancedSet.lookupM: XXX"
lookupM x (T a y b) =
  case compare x y of
    LT -> lookupM x a
    EQ -> return y
    GT -> lookupM x b

fold _ e E = e
fold f e (T a x b) = f x (fold f (fold f e a) b)

fold' _ e E = e
fold' f e (T a x b) = e `seq` f x $! (fold' f (fold' f e a) b)

fold1 _ E = error "UnbalancedSet.fold1: empty collection"
fold1 f (T a x b) = fold f (fold f x a) b

fold1' _ E = error "UnbalancedSet.fold1': empty collection"
fold1' f (T a x b) = fold' f (fold' f x a) b

deleteMin E = E
deleteMin (T E _ b) = b
deleteMin (T a x b) = T (deleteMin a) x b

deleteMax E = E
deleteMax (T a _ E) = a
deleteMax (T a x b) = T a x (deleteMax b)

unsafeInsertMin x t = T E x t
unsafeInsertMax x t = T t x E

unsafeFromOrdSeq xs = fst (ins xs (S.size xs))
  where ins ys 0 = (E,ys)
        ins ys n = let m = n `div` 2
                       (a,ys') = ins ys m
                       (y,ys'') = fromJust (S.lview ys')
                       (b,ys''') = ins ys'' (n - m - 1)
                   in (T a y b,ys''')

unsafeAppend a b = case minView b of
                     Nothing -> a
                     Just (x,b') -> T a x b'

filterLT _ E = E
filterLT y (T a x b) =
  case compare x y of
    LT -> T a x (filterLT y b)
    EQ -> a
    GT -> filterLT y a

filterLE _ E = E
filterLE y (T a x b) =
  case compare x y of
    LT -> T a x (filterLE y b)
    EQ -> T a x E
    GT -> filterLE y a

filterGT _ E = E
filterGT y (T a x b) =
  case compare x y of
    LT -> filterGT y b
    EQ -> b
    GT -> T (filterGT y a) x b

filterGE _ E = E
filterGE y (T a x b) =
  case compare x y of
    LT -> filterGE y b
    EQ -> T E x b
    GT -> T (filterGE y a) x b

partitionLT_GE _ E = (E,E)
partitionLT_GE y (T a x b) =
  case compare x y of
    LT -> (T a x b0,b1)
          where (b0,b1) = partitionLT_GE y b
    EQ -> (a,T E x b)
    GT -> (a0,T a1 x b)
          where (a0,a1) = partitionLT_GE y a

partitionLE_GT _ E = (E,E)
partitionLE_GT y (T a x b) =
  case compare x y of
    LT -> (T a x b0,b1)
          where (b0,b1) = partitionLE_GT y b
    EQ -> (T a x E,b)
    GT -> (a0,T a1 x b)
          where (a0,a1) = partitionLE_GT y a

partitionLT_GT _ E = (E,E)
partitionLT_GT y (T a x b) =
  case compare x y of
    LT -> (T a x b0,b1)
          where (b0,b1) = partitionLT_GT y b
    EQ -> (a,b)
    GT -> (a0,T a1 x b)
          where (a0,a1) = partitionLT_GT y a

minView E = fail "UnbalancedSet.minView: empty collection"
minView (T E x b) = return (x, b)
minView (T a x b) = return (y, T a' x b)
  where (y,a') = fromJust (minView a)

minElem E = error "UnbalancedSet.minElem: empty collection"
minElem (T E x _) = x
minElem (T a _ _) = minElem a

maxView E = fail "UnbalancedSet.maxView: empty collection"
maxView (T a x E) = return (x, a)
maxView (T a x b) = return (y, T a x b')
  where (y, b') = fromJust (maxView b)

maxElem E = error "UnbalancedSet.maxElem: empty collection"
maxElem (T _ x E) = x
maxElem (T _ _ b) = maxElem b

foldr _ e E = e
foldr f e (T a x b) = foldr f (f x (foldr f e b)) a

foldr' _ e E = e
foldr' f e (T a x b) = e `seq` foldr' f (f x $! (foldr' f e b)) a

foldl _ e E = e
foldl f e (T a x b) = foldl f (f (foldl f e a) x) b

foldl' _ e E = e
foldl' f e (T a x b) = e `seq` foldl' f ((f $! (foldl' f e a)) x) b

foldr1 _ E = error "UnbalancedSet.foldr1: empty collection"
foldr1 f (T a x E) = foldr f x a
foldr1 f (T a x b) = foldr f (f x (foldr1 f b)) a

foldr1' _ E = error "UnbalancedSet.foldr1': empty collection"
foldr1' f (T a x E) = foldr' f x a
foldr1' f (T a x b) = foldr' f (f x $! (foldr1' f b)) a

foldl1 _ E = error "UnbalancedSet.foldl1: empty collection"
foldl1 f (T E x b) = foldl f x b
foldl1 f (T a x b) = foldl f (f (foldl1 f a) x) b

foldl1' _ E = error "UnbalancedSet.foldl1': empty collection"
foldl1' f (T E x b) = foldl' f x b
foldl1' f (T a x b) = foldl' f ((f $! (foldl1' f a)) x) b

unsafeMapMonotonic _ E = E
unsafeMapMonotonic f (T a x b) =
    T (unsafeMapMonotonic f a) (f x) (unsafeMapMonotonic f b)

strict s@E = s
strict s@(T l _ r) = strict l `seq` strict r `seq` s

strictWith _ s@E = s
strictWith f s@(T l x r) = f x `seq` strictWith f l `seq` strictWith f r `seq` s

-- the remaining functions all use default definitions

fromSeq = fromSeqUsingUnionSeq
insert = insertUsingInsertWith
insertSeq = insertSeqUsingUnion
union = unionUsingUnionWith
unionSeq = unionSeqUsingReduce
deleteAll = delete
deleteSeq = deleteSeqUsingDelete
count = countUsingMember

toSeq = toSeqUsingFold
lookup = lookupUsingLookupM
lookupAll = lookupAllUsingLookupM
lookupWithDefault = lookupWithDefaultUsingLookupM
filter = filterUsingOrdLists
partition = partitionUsingOrdLists
toOrdSeq = toOrdSeqUsingFoldr

intersection = intersectionUsingIntersectionWith
difference = differenceUsingOrdLists
symmetricDifference = symmetricDifferenceUsingDifference
properSubset = properSubsetUsingOrdLists
subset = subsetUsingOrdLists
fromSeqWith = fromSeqWithUsingInsertWith
insertSeqWith = insertSeqWithUsingInsertWith
unionl = unionlUsingUnionWith
unionr = unionrUsingUnionWith
unionWith = unionWithUsingOrdLists
unionSeqWith = unionSeqWithUsingReducer
intersectionWith = intersectionWithUsingOrdLists

-- instance declarations

instance Ord a => C.CollX (Set a) a where
  {singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; unionSeq = unionSeq;
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   strict = strict;
   structuralInvariant = structuralInvariant; instanceName _ = moduleName}

instance Ord a => C.OrdCollX (Set a) a where
  {deleteMin = deleteMin; deleteMax = deleteMax;
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax;
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT;
   filterGE = filterGE; partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Ord a => C.Coll (Set a) a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM;
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault;
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   filter = filter; partition = partition; strictWith = strictWith}

instance Ord a => C.OrdColl (Set a) a where
  {minView = minView; minElem = minElem; maxView = maxView;
   maxElem = maxElem; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic}

instance Ord a => C.SetX (Set a) a where
  {intersection = intersection; difference = difference;
   symmetricDifference = symmetricDifference;
   properSubset = properSubset; subset = subset}

instance Ord a => C.Set (Set a) a where
  {fromSeqWith = fromSeqWith; insertWith = insertWith;
   insertSeqWith = insertSeqWith; unionl = unionl; unionr = unionr;
   unionWith = unionWith; unionSeqWith = unionSeqWith;
   intersectionWith = intersectionWith}

instance Ord a => C.OrdSetX (Set a) a

instance Ord a => C.OrdSet (Set a) a


instance Ord a => Eq (Set a) where
  xs == ys = C.toOrdList xs == C.toOrdList ys

instance (Ord a, Show a) => Show (Set a) where
   showsPrec = showsPrecUsingToList

instance (Ord a, Read a) => Read (Set a) where
   readsPrec = readsPrecUsingFromList


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do (xs::[a]) <- arbitrary
                 return (Prelude.foldr insert empty xs)

instance (Ord a, CoArbitrary a) => CoArbitrary (Set a) where
  coarbitrary E = variant (0 :: Int)
  coarbitrary (T a x b) =
    variant (1 :: Int) . coarbitrary a . coarbitrary x . coarbitrary b

instance (Ord a) => Semigroup (Set a) where
  (<>) = union

instance (Ord a) => Monoid (Set a) where
    mempty  = empty
    mappend = (SG.<>)
    mconcat = unionSeq

instance (Ord a) => Ord (Set a) where
    compare = compareUsingToOrdList
