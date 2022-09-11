-- |
--   Module      :  Data.Edison.Assoc.AssocList
--   Copyright   :  Copyright (c) 1998, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   This module implements finite maps as simple association lists.
--
--   Duplicates are removed conceptually, but not physically.  The first
--   occurrence of a given key is the one that is considered to be in the map.
--
--   The list type is mildly customized to prevent boxing the pairs.

module Data.Edison.Assoc.AssocList (
    -- * Type of simple association lists
    FM, -- instance of Assoc(X), FiniteMap(X)
        -- also instance of Functor

    -- * AssocX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupAndDelete,lookupAndDeleteM,lookupAndDeleteAll,
    lookupWithDefault,adjust,adjustAll,adjustOrInsert,adjustAllOrInsert,
    adjustOrDelete,adjustOrDeleteAll,strict,strictWith,
    map,fold,fold',fold1,fold1',filter,partition,elements,structuralInvariant,

    -- * OrdAssocX operations
    minView, minElem, deleteMin, unsafeInsertMin, maxView, maxElem, deleteMax,
    unsafeInsertMax, foldr, foldr', foldl, foldl', foldr1, foldr1',
    foldl1, foldl1', unsafeFromOrdSeq, unsafeAppend,
    filterLT, filterLE, filterGT, filterGE,
    partitionLT_GE, partitionLE_GT, partitionLT_GT,

    -- * Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,foldWithKey',filterWithKey,partitionWithKey,

    -- * OrdAssoc operations
    minViewWithKey, minElemWithKey, maxViewWithKey, maxElemWithKey,
    foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey', toOrdSeq,

    -- * FiniteMapX operations
    fromSeqWith,fromSeqWithKey,insertWith,insertWithKey,insertSeqWith,
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectionWith,
    difference,properSubset,subset,properSubmapBy,submapBy,sameMapBy,
    properSubmap,submap,sameMap,

    -- * FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectionWithKey,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import qualified Prelude
import Data.Monoid
import Data.Semigroup as SG
import qualified Control.Monad.Fail as Fail
import qualified Data.Edison.Assoc as A
import Data.Edison.Prelude ( runFail_ )
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.BinaryRandList as RL
import Data.Edison.Assoc.Defaults
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), variant)

-- signatures for exported functions
moduleName    :: String
empty         :: Eq k => FM k a
singleton     :: Eq k => k -> a -> FM k a
fromSeq       :: (Eq k,S.Sequence seq) => seq (k,a) -> FM k a
insert        :: Eq k => k -> a -> FM k a -> FM k a
insertSeq     :: (Eq k,S.Sequence seq) => seq (k,a) -> FM k a -> FM k a
union         :: Eq k => FM k a -> FM k a -> FM k a
unionSeq      :: (Eq k,S.Sequence seq) => seq (FM k a) -> FM k a
delete        :: Eq k => k -> FM k a -> FM k a
deleteAll     :: Eq k => k -> FM k a -> FM k a
deleteSeq     :: (Eq k,S.Sequence seq) => seq k -> FM k a -> FM k a
null          :: Eq k => FM k a -> Bool
size          :: Eq k => FM k a -> Int
member        :: Eq k => k -> FM k a -> Bool
count         :: Eq k => k -> FM k a -> Int
lookup        :: Eq k => k -> FM k a -> a
lookupM       :: (Eq k, Fail.MonadFail rm) => k -> FM k a -> rm a
lookupAll     :: (Eq k,S.Sequence seq) => k -> FM k a -> seq a
lookupAndDelete    :: Eq k => k -> FM k a -> (a,FM k a)
lookupAndDeleteM   :: (Eq k, Fail.MonadFail rm)   => k -> FM k a -> rm (a,FM k a)
lookupAndDeleteAll :: (Eq k,S.Sequence seq) => k -> FM k a -> (seq a,FM k a)
lookupWithDefault  :: Eq k => a -> k -> FM k a -> a
adjust             :: Eq k => (a -> a) -> k -> FM k a -> FM k a
adjustAll          :: Eq k => (a -> a) -> k -> FM k a -> FM k a
adjustOrInsert     :: Eq k => (a -> a) -> a -> k -> FM k a -> FM k a
adjustAllOrInsert  :: Eq k => (a -> a) -> a -> k -> FM k a -> FM k a
adjustOrDelete     :: Eq k => (a -> Maybe a) -> k -> FM k a -> FM k a
adjustOrDeleteAll  :: Eq k => (a -> Maybe a) -> k -> FM k a -> FM k a
strict             :: FM k a -> FM k a
strictWith         :: (a -> b) -> FM k a -> FM k a
map           :: Eq k => (a -> b) -> FM k a -> FM k b
fold          :: Eq k => (a -> b -> b) -> b -> FM k a -> b
fold1         :: Eq k => (a -> a -> a) -> FM k a -> a
fold'         :: Eq k => (a -> b -> b) -> b -> FM k a -> b
fold1'        :: Eq k => (a -> a -> a) -> FM k a -> a
filter        :: Eq k => (a -> Bool) -> FM k a -> FM k a
partition     :: Eq k => (a -> Bool) -> FM k a -> (FM k a, FM k a)
elements      :: (Eq k,S.Sequence seq) => FM k a -> seq a

fromSeqWith      :: (Eq k,S.Sequence seq) =>
                        (a -> a -> a) -> seq (k,a) -> FM k a
fromSeqWithKey   :: (Eq k,S.Sequence seq) => (k -> a -> a -> a) -> seq (k,a) -> FM k a
insertWith       :: Eq k => (a -> a -> a) -> k -> a -> FM k a -> FM k a
insertWithKey    :: Eq k => (k -> a -> a -> a) -> k -> a -> FM k a -> FM k a
insertSeqWith    :: (Eq k,S.Sequence seq) =>
                        (a -> a -> a) -> seq (k,a) -> FM k a -> FM k a
insertSeqWithKey :: (Eq k,S.Sequence seq) =>
                        (k -> a -> a -> a) -> seq (k,a) -> FM k a -> FM k a
unionl           :: Eq k => FM k a -> FM k a -> FM k a
unionr           :: Eq k => FM k a -> FM k a -> FM k a
unionWith        :: Eq k => (a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWith     :: (Eq k,S.Sequence seq) =>
                        (a -> a -> a) -> seq (FM k a) -> FM k a
intersectionWith :: Eq k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference       :: Eq k => FM k a -> FM k b -> FM k a
properSubset     :: Eq k => FM k a -> FM k b -> Bool
subset           :: Eq k => FM k a -> FM k b -> Bool
properSubmapBy   :: Eq k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
submapBy         :: Eq k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
sameMapBy        :: Eq k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
properSubmap     :: (Eq k, Eq a) => FM k a -> FM k a -> Bool
submap           :: (Eq k, Eq a) => FM k a -> FM k a -> Bool
sameMap          :: (Eq k, Eq a) => FM k a -> FM k a -> Bool

toSeq            :: (Eq k,S.Sequence seq) => FM k a -> seq (k,a)
keys             :: (Eq k,S.Sequence seq) => FM k a -> seq k
mapWithKey       :: Eq k => (k -> a -> b) -> FM k a -> FM k b
foldWithKey      :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
foldWithKey'     :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
filterWithKey    :: Eq k => (k -> a -> Bool) -> FM k a -> FM k a
partitionWithKey :: Eq k => (k -> a -> Bool) -> FM k a -> (FM k a, FM k a)

unionWithKey     :: Eq k => (k -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey  :: (Eq k,S.Sequence seq) =>
                        (k -> a -> a -> a) -> seq (FM k a) -> FM k a
intersectionWithKey :: Eq k => (k -> a -> b -> c) -> FM k a -> FM k b -> FM k c

minView          :: (Ord k, Fail.MonadFail m) => FM k a -> m (a,FM k a)
minElem          :: Ord k => FM k a -> a
deleteMin        :: Ord k => FM k a -> FM k a
unsafeInsertMin  :: Ord k => k -> a -> FM k a -> FM k a
maxView          :: (Ord k, Fail.MonadFail m) => FM k a -> m (a,FM k a)
maxElem          :: Ord k => FM k a -> a
deleteMax        :: Ord k => FM k a -> FM k a
unsafeInsertMax  :: Ord k => k -> a -> FM k a -> FM k a
foldr            :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldr1           :: Ord k => (a -> a -> a) -> FM k a -> a
foldl            :: Ord k => (b -> a -> b) -> b -> FM k a -> b
foldl1           :: Ord k => (a -> a -> a) -> FM k a -> a
foldr'           :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldr1'          :: Ord k => (a -> a -> a) -> FM k a -> a
foldl'           :: Ord k => (b -> a -> b) -> b -> FM k a -> b
foldl1'          :: Ord k => (a -> a -> a) -> FM k a -> a
unsafeFromOrdSeq :: (Ord k,S.Sequence seq) => seq (k,a) -> FM k a
unsafeAppend     :: Ord k => FM k a -> FM k a -> FM k a
filterLT         :: Ord k => k -> FM k a -> FM k a
filterLE         :: Ord k => k -> FM k a -> FM k a
filterGT         :: Ord k => k -> FM k a -> FM k a
filterGE         :: Ord k => k -> FM k a -> FM k a
partitionLT_GE   :: Ord k => k -> FM k a -> (FM k a,FM k a)
partitionLE_GT   :: Ord k => k -> FM k a -> (FM k a,FM k a)
partitionLT_GT   :: Ord k => k -> FM k a -> (FM k a,FM k a)

minViewWithKey    :: (Ord k, Fail.MonadFail m) => FM k a -> m ((k, a), FM k a)
minElemWithKey    :: Ord k => FM k a -> (k,a)
maxViewWithKey    :: (Ord k, Fail.MonadFail m) => FM k a -> m ((k, a), FM k a)
maxElemWithKey    :: Ord k => FM k a -> (k,a)
foldrWithKey      :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey      :: Ord k => (b -> k -> a -> b) -> b -> FM k a -> b
foldrWithKey'     :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey'     :: Ord k => (b -> k -> a -> b) -> b -> FM k a -> b
toOrdSeq          :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)


moduleName = "Data.Edison.Assoc.AssocList"


data FM k a = E | I k a (FM k a)

-- no invariants
structuralInvariant :: Eq k => FM k a -> Bool
structuralInvariant = const True

---------------------------------------
-- some unexported utility functions

-- uncurried insert.
uinsert :: (t, t1) -> FM t t1 -> FM t t1
uinsert (k,x) = I k x


-- left biased merge.
mergeFM :: (Ord t) => FM t t1 -> FM t t1 -> FM t t1
mergeFM E m = m
mergeFM m E = m
mergeFM o1@(I k1 a1 m1) o2@(I k2 a2 m2) =
  case compare k1 k2 of
      LT -> I k1 a1 (mergeFM m1 o2)
      GT -> I k2 a2 (mergeFM o1 m2)
      EQ -> I k1 a1 (mergeFM m1 m2)

toRandList :: FM t t1 -> RL.Seq (FM t t1)
toRandList E = RL.empty
toRandList (I k a m) = RL.lcons (I k a E) (toRandList m)

mergeSortFM :: (Ord t) => FM t t1 -> FM t t1
mergeSortFM m = RL.reducer mergeFM E (toRandList m)

foldrFM :: Eq k => (a -> b -> b) -> b -> FM k a -> b
foldrFM _ z E = z
foldrFM f z (I k a m) = f a (foldrFM f z (delete k m))

foldr1FM :: Eq k => (a -> a -> a) -> FM k a -> a
foldr1FM _ (I _ a E) = a
foldr1FM f (I k a m) = f a (foldr1FM f (delete k m))
foldr1FM _ _ = error "invalid call to foldr1FM on empty map"

foldrFM' :: Eq k => (a -> b -> b) -> b -> FM k a -> b
foldrFM' _ z E = z
foldrFM' f z (I k a m) = f a $! (foldrFM' f z (delete k m))

foldr1FM' :: Eq k => (a -> a -> a) -> FM k a -> a
foldr1FM' _ (I _ a E) = a
foldr1FM' f (I k a m) = f a $! (foldr1FM' f (delete k m))
foldr1FM' _ _ = error "invalid call to foldr1FM' on empty map"

foldlFM :: Eq k => (b -> a -> b) -> b -> FM k a -> b
foldlFM _ x E = x
foldlFM f x (I k a m) = foldlFM f (f x a) (delete k m)

foldlFM' :: Eq k => (b -> a -> b) -> b -> FM k a -> b
foldlFM' _ x E = x
foldlFM' f x (I k a m) = x `seq` foldlFM' f (f x a) (delete k m)

foldrWithKeyFM :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
foldrWithKeyFM _ z E = z
foldrWithKeyFM f z (I k a m) = f k a (foldrWithKeyFM f z (delete k m))

foldrWithKeyFM' :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
foldrWithKeyFM' _ z E = z
foldrWithKeyFM' f z (I k a m) = f k a $! (foldrWithKeyFM' f z (delete k m))

foldlWithKeyFM :: Eq k => (b -> k -> a -> b) -> b -> FM k a -> b
foldlWithKeyFM _ x E = x
foldlWithKeyFM f x (I k a m) = foldlWithKeyFM f (f x k a) (delete k m)

foldlWithKeyFM' :: Eq k => (b -> k -> a -> b) -> b -> FM k a -> b
foldlWithKeyFM' _ x E = x
foldlWithKeyFM' f x (I k a m) = x `seq` foldlWithKeyFM' f (f x k a) (delete k m)

takeWhileFM :: (k -> Bool) -> FM k a -> FM k a
takeWhileFM _ E = E
takeWhileFM p (I k a m)
   | p k       = I k a (takeWhileFM p m)
   | otherwise = E

dropWhileFM :: (k -> Bool) -> FM k a -> FM k a
dropWhileFM _ E = E
dropWhileFM p o@(I k _ m)
   | p k       = dropWhileFM p m
   | otherwise = o

spanFM :: (k -> Bool) -> FM k a -> (FM k a,FM k a)
spanFM _ E = (E,E)
spanFM p o@(I k a m)
   | p k       = let (x,y) = spanFM p m in (I k a x,y)
   | otherwise = (E,o)


---------------------------------------------------
-- interface functions

empty = E
singleton k x = I k x E
insert = I
insertSeq kxs m = S.foldr uinsert m kxs
fromSeq = S.foldr uinsert E

union m E = m
union E m = m
union (I k x m1) m2 = I k x (union m1 m2)

unionSeq = S.foldr union E

deleteAll _ E = E
deleteAll key (I k x m) | key == k  = deleteAll key m
                        | otherwise = I k x (deleteAll key m)

delete = deleteAll

null E = True
null (I _ _ _) = False

size E = 0
size (I k _ m) = 1 + size (delete k m)

member _ E = False
member key (I k _ m) = key == k || member key m

count _ E = 0
count key (I k _ m) | key == k  = 1
                    | otherwise = count key m

lookup key m = runFail_ (lookupM key m)

lookupM _ E = fail "AssocList.lookup: lookup failed"
lookupM key (I k x m) | key == k  = return x
                      | otherwise = lookupM key m

lookupAll _ E = S.empty
lookupAll key (I k x m) | key == k  = S.singleton x
                        | otherwise = lookupAll key m

lookupAndDelete key m = runFail_ (lookupAndDeleteM key m)

lookupAndDeleteM _ E = fail "AssocList.lookupAndDeleteM: lookup failed"
lookupAndDeleteM key (I k x m)
   | key == k  = return (x,delete k m)
   | otherwise = lookupAndDeleteM key m >>=
                    \ (z, m') -> return (z, I k x m')

lookupAndDeleteAll key m =
   case lookupAndDeleteM key m of
      Nothing     -> (S.empty,m)
      Just (z,m') -> (S.singleton z,m')


lookupWithDefault d _ E = d
lookupWithDefault d key (I k x m) | key == k = x
                                  | otherwise = lookupWithDefault d key m

elements E = S.empty
elements (I k x m) = S.lcons x (elements (delete k m))

adjust _ _ E = E
adjust f key (I k x m) | key == k  = I key (f x) m
                       | otherwise = I k x (adjust f key m)

adjustAll = adjust

adjustOrInsert _ z key E = singleton key z
adjustOrInsert f z key (I k x m)
    | key == k  = I key (f x) m
    | otherwise = I k x (adjustOrInsert f z key m)

adjustAllOrInsert = adjustOrInsert

adjustOrDelete = adjustOrDeleteDefault
adjustOrDeleteAll = adjustOrDeleteAllDefault

map _ E = E
map f (I k x m) = I k (f x) (map f m)

fold _ c E = c
fold f c (I k x m) = fold f (f x c) (delete k m)

fold' _ c E = c
fold' f c (I k x m) = c `seq` fold' f (f x c) (delete k m)

fold1 _ E = error "AssocList.fold1: empty map"
fold1 f (I k x m) = fold f x (delete k m)

fold1' _ E = error "AssocList.fold1': empty map"
fold1' f (I k x m) = fold' f x (delete k m)

filter _ E = E
filter p (I k x m) | p x = I k x (filter p (delete k m))
                   | otherwise = filter p (delete k m)

partition _ E = (E, E)
partition p (I k x m)
    | p x       = (I k x m1,m2)
    | otherwise = (m1,I k x m2)
  where (m1,m2) = partition p (delete k m)


toSeq E = S.empty
toSeq (I k x m) = S.lcons (k,x) (toSeq (delete k m))

keys E = S.empty
keys (I k _ m) = S.lcons k (keys (delete k m))

mapWithKey _ E = E
mapWithKey f (I k x m) = I k (f k x) (mapWithKey f m)

foldWithKey _ c E = c
foldWithKey f c (I k x m) = foldWithKey f (f k x c) (delete k m)

foldWithKey' _ c E = c
foldWithKey' f c (I k x m) = c `seq` foldWithKey' f (f k x c) (delete k m)

filterWithKey _ E = E
filterWithKey p (I k x m)
    | p k x = I k x (filterWithKey p (delete k m))
    | otherwise = filterWithKey p (delete k m)

partitionWithKey _ E = (E, E)
partitionWithKey p (I k x m)
    | p k x     = (I k x m1,m2)
    | otherwise = (m1,I k x m2)
  where (m1,m2) = partitionWithKey p (delete k m)

unionl = union
unionr = flip union


findMin :: (Ord t) => t -> t1 -> FM t t1 -> (t, t1)
findMin k0 x E = (k0,x)
findMin k0 a0 (I k a m)
        | k < k0    = findMin k  a  (delete k m)
        | otherwise = findMin k0 a0 (delete k m)

findMax ::( Ord t) => t -> t1 -> FM t t1 -> (t, t1)
findMax k0 x E = (k0,x)
findMax k0 a0 (I k a m)
        | k > k0    = findMax k  a  (delete k m)
        | otherwise = findMax k0 a0 (delete k m)

minView E = fail (moduleName++".minView: empty map")
minView n@(I k a m) = let (k',x) = findMin k a m in return (x,delete k' n)

minElem E = error (moduleName++".minElem: empty map")
minElem (I k a m) = let (_,x) = findMin k a m in x

deleteMin E = error (moduleName++".deleteMin: empty map")
deleteMin n@(I k a m) = let (k',_) = findMin k a m in delete k' n

unsafeInsertMin  = insert

maxView E = fail (moduleName++".maxView: empty map")
maxView n@(I k a m) = let (k',x) = findMax k a m in return (x,delete k' n)

maxElem E = error (moduleName++".maxElem: empty map")
maxElem (I k a m) = let (_,x) = findMax k a m in x

deleteMax E = error (moduleName++".deleteMax: empty map")
deleteMax n@(I k a m) = let (k',_) = findMax k a m in delete k' n

unsafeInsertMax = insert

foldr  f z m = foldrFM  f z (mergeSortFM m)
foldr' f z m = foldrFM' f z (mergeSortFM m)

foldr1 f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldlr1: empty map"
    n -> foldr1FM f n

foldr1' f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldlr1': empty map"
    n -> foldr1FM' f n

foldl  f x m = foldlFM  f x (mergeSortFM m)
foldl' f x m = foldlFM' f x (mergeSortFM m)

foldl1 f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldl1: empty map"
    I k a n -> foldlFM f a (delete k n)

foldl1' f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldl1': empty map"
    I k a n -> foldlFM' f a (delete k n)

unsafeFromOrdSeq   = fromSeq
unsafeAppend       = union
filterLT k         = takeWhileFM (<k)  . mergeSortFM
filterLE k         = takeWhileFM (<=k) . mergeSortFM
filterGT k         = dropWhileFM (<=k) . mergeSortFM
filterGE k         = dropWhileFM (<k)  . mergeSortFM
partitionLT_GE k   = spanFM (<k)  . mergeSortFM
partitionLE_GT k   = spanFM (<=k) . mergeSortFM
partitionLT_GT k   = (\(x,y) -> (x,delete k y)) . spanFM (<k)  . mergeSortFM

minViewWithKey E   = fail $ moduleName++".minViewWithKey: empty map"
minViewWithKey n@(I k a m) = let (k',x) = findMin k a m in return ((k',x),delete k' n)

minElemWithKey E   = error $ moduleName++".minElemWithKey: empty map"
minElemWithKey (I k a m) = findMin k a m

maxViewWithKey E   = fail $ moduleName++".maxViewWithKey: empty map"
maxViewWithKey n@(I k a m) = let (k',x) = findMax k a m in return ((k',x),delete k' n)

maxElemWithKey E   = error $ moduleName++".maxElemWithKey: empty map"
maxElemWithKey (I k a m) = findMax k a m

foldrWithKey  f z   = foldrWithKeyFM  f z . mergeSortFM
foldrWithKey' f z   = foldrWithKeyFM' f z . mergeSortFM
foldlWithKey  f x   = foldlWithKeyFM  f x . mergeSortFM
foldlWithKey' f x   = foldlWithKeyFM' f x . mergeSortFM
toOrdSeq            = toSeq . mergeSortFM


strict n@E = n
strict n@(I _ _ m) = strict m `seq` n

strictWith _ n@E = n
strictWith f n@(I _ a m) = f a `seq` strictWith f m `seq` n


-- defaults

deleteSeq = deleteSeqUsingFoldr
insertWith = insertWithUsingLookupM
insertSeqWith = insertSeqWithUsingInsertWith
insertWithKey = insertWithKeyUsingInsertWith
insertSeqWithKey = insertSeqWithKeyUsingInsertWithKey
unionWith = unionWithUsingInsertWith
unionSeqWith = unionSeqWithUsingFoldr
fromSeqWith = fromSeqWithUsingInsertSeqWith
fromSeqWithKey = fromSeqWithKeyUsingInsertSeqWithKey
intersectionWith = intersectionWithUsingLookupM
difference = differenceUsingDelete
properSubset = properSubsetUsingSubset
subset = subsetUsingMember
properSubmapBy = properSubmapByUsingSubmapBy
submapBy = submapByUsingLookupM
sameMapBy = sameMapByUsingSubmapBy
properSubmap = A.properSubmap
submap = A.submap
sameMap = A.sameMap
unionWithKey = unionWithKeyUsingInsertWithKey
unionSeqWithKey = unionSeqWithKeyUsingFoldr
intersectionWithKey = intersectionWithKeyUsingLookupM

-- instance declarations

instance Eq k  => A.AssocX (FM k) k where
  {empty = empty; singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq;
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   lookup = lookup; lookupM = lookupM; lookupAll = lookupAll;
   lookupAndDelete = lookupAndDelete; lookupAndDeleteM = lookupAndDeleteM;
   lookupAndDeleteAll = lookupAndDeleteAll;
   lookupWithDefault = lookupWithDefault; adjust = adjust;
   adjustAll = adjustAll; adjustOrInsert = adjustOrInsert;
   adjustAllOrInsert = adjustAllOrInsert;
   adjustOrDelete = adjustOrDelete; adjustOrDeleteAll = adjustOrDeleteAll;
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   filter = filter; partition = partition; elements = elements;
   strict = strict; strictWith = strictWith;
   structuralInvariant = structuralInvariant; instanceName _ = moduleName}

instance Ord k => A.OrdAssocX (FM k) k where
  {minView = minView; minElem = minElem; deleteMin = deleteMin;
   unsafeInsertMin = unsafeInsertMin; maxView = maxView; maxElem = maxElem;
   deleteMax = deleteMax; unsafeInsertMax = unsafeInsertMax;
   foldr = foldr; foldr' = foldr'; foldl = foldl; foldl' = foldl';
   foldr1 = foldr1; foldr1' = foldr1'; foldl1 = foldl1; foldl1' = foldl1';
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterGT = filterGT; filterLE = filterLE;
   filterGE = filterGE; partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Eq k => A.FiniteMapX (FM k) k where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey;
   insertWith  = insertWith; insertWithKey = insertWithKey;
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey;
   unionl = unionl; unionr = unionr; unionWith = unionWith;
   unionSeqWith = unionSeqWith; intersectionWith = intersectionWith;
   difference = difference; properSubset = properSubset; subset = subset;
   properSubmapBy = properSubmapBy; submapBy = submapBy;
   sameMapBy = sameMapBy}

instance Ord k => A.OrdFiniteMapX (FM k) k

instance Eq k  => A.Assoc (FM k) k where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey;
   foldWithKey = foldWithKey; foldWithKey' = foldWithKey';
   filterWithKey = filterWithKey;
   partitionWithKey = partitionWithKey}

instance Ord k => A.OrdAssoc (FM k) k where
  {minViewWithKey = minViewWithKey; minElemWithKey = minElemWithKey;
   maxViewWithKey = maxViewWithKey; maxElemWithKey = maxElemWithKey;
   foldrWithKey = foldrWithKey; foldrWithKey' = foldrWithKey';
   foldlWithKey = foldlWithKey; foldlWithKey' = foldlWithKey';
   toOrdSeq = toOrdSeq}

instance Eq k => A.FiniteMap (FM k) k where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey;
   intersectionWithKey = intersectionWithKey}

instance Ord k => A.OrdFiniteMap (FM k) k

instance Eq k => Functor (FM k) where
  fmap =  map

instance (Eq k,Eq a) => Eq (FM k a) where
  (==) = sameMap

instance (Ord k, Ord a) => Ord (FM k a) where
  compare = compareUsingToOrdList

instance (Eq k,Show k,Show a) => Show (FM k a) where
  showsPrec = showsPrecUsingToList

instance (Eq k,Read k,Read a) => Read (FM k a) where
  readsPrec = readsPrecUsingFromList

instance (Eq k,Arbitrary k,Arbitrary a) => Arbitrary (FM k a) where
   arbitrary = do (xs::[(k,a)]) <- arbitrary
                  return (Prelude.foldr (uncurry insert) empty xs)

instance (Eq k,CoArbitrary k,CoArbitrary a) => CoArbitrary (FM k a) where
   coarbitrary E = variant 0
   coarbitrary (I k a m) = variant 1 . coarbitrary k
                         . coarbitrary a . coarbitrary m


instance Eq k => Semigroup (FM k a) where
   (<>) = union
instance Eq k => Monoid (FM k a) where
   mempty  = empty
   mappend = (SG.<>)
   mconcat = unionSeq
