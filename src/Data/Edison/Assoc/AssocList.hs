-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | This module implements finite maps as simple association lists.
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
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupWithDefault,adjust,adjustAll,adjustOrInsert,map,
    fold,fold',fold1,fold1',filter,partition,elements,structuralInvariant,

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
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectWith,
    difference,subset,subsetEq,

    -- * FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectWithKey,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import qualified Prelude
import Control.Monad.Identity
import Data.Edison.Prelude
import qualified Data.Edison.Assoc as A
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.BinaryRandList as RL
import Data.Edison.Assoc.Defaults
import Test.QuickCheck (Arbitrary(..), variant)

-- signatures for exported functions
moduleName    :: String
empty         :: Eq k => FM k a
single        :: Eq k => k -> a -> FM k a
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
lookupM       :: (Eq k, Monad rm) => k -> FM k a -> rm a
lookupAll     :: (Eq k,S.Sequence seq) => k -> FM k a -> seq a
lookupWithDefault :: Eq k => a -> k -> FM k a -> a
adjust        :: Eq k => (a -> a) -> k -> FM k a -> FM k a
adjustAll     :: Eq k => (a -> a) -> k -> FM k a -> FM k a
adjustOrInsert :: Eq k => (Maybe a -> a) -> k -> FM k a -> FM k a
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
intersectWith    :: Eq k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference       :: Eq k => FM k a -> FM k b -> FM k a
subset           :: Eq k => FM k a -> FM k b -> Bool    
subsetEq         :: Eq k => FM k a -> FM k b -> Bool    

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
intersectWithKey :: Eq k => (k -> a -> b -> c) -> FM k a -> FM k b -> FM k c

minView          :: (Ord k,Monad m) => FM k a -> m (a,FM k a)
minElem          :: Ord k => FM k a -> a
deleteMin        :: Ord k => FM k a -> FM k a
unsafeInsertMin  :: Ord k => k -> a -> FM k a -> FM k a
maxView          :: (Ord k,Monad m) => FM k a -> m (a,FM k a)
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

minViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
minElemWithKey    :: Ord k => FM k a -> (k,a)
maxViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
maxElemWithKey    :: Ord k => FM k a -> (k,a)
foldrWithKey      :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey      :: Ord k => (b -> k -> a -> b) -> b -> FM k a -> b
foldrWithKey'     :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey'     :: Ord k => (b -> k -> a -> b) -> b -> FM k a -> b
toOrdSeq          :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)


moduleName = "Data.Edison.Assoc.AssocList"


data FM k a = E | I k a (FM k a) deriving (Show)

-- no invariants
structuralInvariant :: Eq k => FM k a -> Bool
structuralInvariant = const True

---------------------------------------
-- some unexported utility functions

-- uncurried insert.
uinsert (k,x) = I k x


-- left biased merge.
mergeFM E m = m
mergeFM m E = m
mergeFM o1@(I k1 a1 m1) o2@(I k2 a2 m2) =
  case compare k1 k2 of
      LT -> I k1 a1 (mergeFM m1 o2)
      GT -> I k2 a2 (mergeFM o1 m2)
      EQ -> I k1 a1 (mergeFM m1 m2)

toRandList E = RL.empty
toRandList (I k a m) = RL.lcons (I k a E) (toRandList m)

mergeSortFM m = RL.reducer mergeFM E (toRandList m)

foldrFM :: Eq k => (a -> b -> b) -> b -> FM k a -> b
foldrFM f z E = z
foldrFM f z (I k a m) = f a (foldrFM f z (delete k m))

foldrFM' :: Eq k => (a -> b -> b) -> b -> FM k a -> b
foldrFM' f z E = z
foldrFM' f z (I k a m) = f a $! (foldrFM' f z (delete k m))

foldlFM :: Eq k => (b -> a -> b) -> b -> FM k a -> b
foldlFM f x E = x
foldlFM f x (I k a m) = foldlFM f (f x a) (delete k m)

foldlFM' :: Eq k => (b -> a -> b) -> b -> FM k a -> b
foldlFM' f x E = x
foldlFM' f x (I k a m) = x `seq` foldlFM' f (f x a) (delete k m)

foldrWithKeyFM :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
foldrWithKeyFM f z E = z
foldrWithKeyFM f z (I k a m) = f k a (foldrWithKeyFM f z (delete k m))

foldrWithKeyFM' :: Eq k => (k -> a -> b -> b) -> b -> FM k a -> b
foldrWithKeyFM' f z E = z
foldrWithKeyFM' f z (I k a m) = f k a $! (foldrWithKeyFM' f z (delete k m))

foldlWithKeyFM :: Eq k => (b -> k -> a -> b) -> b -> FM k a -> b
foldlWithKeyFM f x E = x
foldlWithKeyFM f x (I k a m) = foldlWithKeyFM f (f x k a) (delete k m)

foldlWithKeyFM' :: Eq k => (b -> k -> a -> b) -> b -> FM k a -> b
foldlWithKeyFM' f x E = x
foldlWithKeyFM' f x (I k a m) = x `seq` foldlWithKeyFM' f (f x k a) (delete k m)

takeWhileFM :: (k -> Bool) -> FM k a -> FM k a
takeWhileFM p E = E
takeWhileFM p (I k a m)
   | p k       = I k a (takeWhileFM p m)
   | otherwise = E

dropWhileFM :: (k -> Bool) -> FM k a -> FM k a
dropWhileFM p E = E
dropWhileFM p o@(I k a m)
   | p k       = dropWhileFM p m
   | otherwise = o

spanFM :: (k -> Bool) -> FM k a -> (FM k a,FM k a)
spanFM p E = (E,E)
spanFM p o@(I k a m)
   | p k       = let (x,y) = spanFM p m in (I k a x,y)
   | otherwise = (E,o)


---------------------------------------------------
-- interface functions

empty = E
single k x = I k x E
insert = I
insertSeq kxs m = S.foldr uinsert m kxs
fromSeq = S.foldr uinsert E

union m E = m
union E m = m
union (I k x m1) m2 = I k x (union m1 m2)

unionSeq = S.foldr union E

deleteAll key E = E
deleteAll key (I k x m) | key == k  = deleteAll key m 
                        | otherwise = I k x (deleteAll key m)

delete = deleteAll

null E = True
null (I k x m) = False

size E = 0
size (I k x m) = 1 + size (delete k m)

member key E = False
member key (I k x m) = key == k || member key m

count key E = 0
count key (I k x m) | key == k  = 1
                    | otherwise = count key m

lookup key m = runIdentity (lookupM key m)

lookupM key E = fail "AssocList.lookup: lookup failed"
lookupM key (I k x m) | key == k  = return x
                      | otherwise = lookupM key m

lookupAll key E = S.empty
lookupAll key (I k x m) | key == k  = S.single x 
                        | otherwise = lookupAll key m

lookupWithDefault d key E = d
lookupWithDefault d key (I k x m) | key == k = x
                                  | otherwise = lookupWithDefault d key m

elements E = S.empty
elements (I k x m) = S.lcons x (elements (delete k m))

adjust f key E = E
adjust f key (I k x m) | key == k  = I key (f x) m
                       | otherwise = I k x (adjust f key m)

adjustAll = adjust

adjustOrInsert f key E = single key (f Nothing)
adjustOrInsert f key (I k x m)
    | key == k  = I key (f (Just x)) m
    | otherwise = I k x (adjustOrInsert f key m)


map f E = E
map f (I k x m) = I k (f x) (map f m)

fold f c E = c
fold f c (I k x m) = fold f (f x c) (delete k m)

fold' f c E = c
fold' f c (I k x m) = c `seq` fold' f (f x c) (delete k m)

fold1 f E = error "AssocList.fold1: empty map"
fold1 f (I k x m) = fold f x (delete k m)

fold1' f E = error "AssocList.fold1': empty map"
fold1' f (I k x m) = fold' f x (delete k m)

filter p E = E
filter p (I k x m) | p x = I k x (filter p (delete k m))
                   | otherwise = filter p (delete k m)

partition p E = (E, E)
partition p (I k x m)
    | p x       = (I k x m1,m2)
    | otherwise = (m1,I k x m2)
  where (m1,m2) = partition p (delete k m)


toSeq E = S.empty
toSeq (I k x m) = S.lcons (k,x) (toSeq (delete k m))

keys E = S.empty
keys (I k x m) = S.lcons k (keys (delete k m))

mapWithKey f E = E
mapWithKey f (I k x m) = I k (f k x) (mapWithKey f m)

foldWithKey f c E = c
foldWithKey f c (I k x m) = foldWithKey f (f k x c) (delete k m)

foldWithKey' f c E = c
foldWithKey' f c (I k x m) = c `seq` foldWithKey' f (f k x c) (delete k m)

filterWithKey p E = E
filterWithKey p (I k x m) 
    | p k x = I k x (filterWithKey p (delete k m))
    | otherwise = filterWithKey p (delete k m)

partitionWithKey p E = (E, E)
partitionWithKey p (I k x m)
    | p k x     = (I k x m1,m2)
    | otherwise = (m1,I k x m2)
  where (m1,m2) = partitionWithKey p (delete k m)

unionl = union
unionr = flip union


findMin k0 x E = (k0,x)
findMin k0 a0 (I k a m)
        | k < k0    = findMin k  a  (delete k m)
        | otherwise = findMin k0 a0 (delete k m)

findMax k0 x E = (k0,x)
findMax k0 a0 (I k a m)
        | k > k0    = findMin k  a  (delete k m)
        | otherwise = findMin k0 a0 (delete k m)

minView E = fail (moduleName++".minView: empty map")
minView (I k a m) = let (k',x) = findMin k a m in return (x,delete k' m)

minElem E = error (moduleName++".minElem: empty map")
minElem (I k a m) = let (_,x) = findMin k a m in x

deleteMin E = error (moduleName++".deleteMin: empty map")
deleteMin (I k a m) = let (k',_) = findMin k a m in delete k' m

unsafeInsertMin  = insert

maxView E = fail (moduleName++".maxView: empty map")
maxView (I k a m) = let (k',x) = findMax k a m in return (x,delete k' m)

maxElem E = error (moduleName++".maxElem: empty map")
maxElem (I k a m) = let (_,x) = findMax k a m in x

deleteMax E = error (moduleName++".deleteMax: empty map")
deleteMax (I k a m) = let (k',_) = findMax k a m in delete k' m

unsafeInsertMax = insert

foldr  f z m = foldrFM  f z (mergeSortFM m)
foldr' f z m = foldrFM' f z (mergeSortFM m)

foldr1 f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldlr1: empty map"
    I k a m -> foldrFM f a (delete k m)

foldr1' f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldlr1': empty map"
    I k a m -> foldrFM' f a (delete k m)
   
foldl  f x m = foldlFM  f x (mergeSortFM m)
foldl' f x m = foldlFM' f x (mergeSortFM m)

foldl1 f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldl1: empty map"
    I k a m -> foldlFM f a (delete k m)

foldl1' f m =
  case mergeSortFM m of
    E -> error $ moduleName++".foldl1': empty map"
    I k a m -> foldlFM' f a (delete k m)

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
minViewWithKey (I k a m) = let (k',x) = findMin k a m in return ((k',x),delete k' m)

minElemWithKey E   = error $ moduleName++".minElemWithKey: empty map"
minElemWithKey (I k a m) = let (k',x) = findMin k a m in (k',x)

maxViewWithKey E   = fail $ moduleName++".maxViewWithKey: empty map"
maxViewWithKey (I k a m) = let (k',x) = findMin k a m in return ((k',x),delete k' m)

maxElemWithKey E   = error $ moduleName++".maxElemWithKey: empty map"
maxElemWithKey (I k a m) = let (k',x) = findMin k a m in (k',x)

foldrWithKey  f z   = foldrWithKeyFM  f z . mergeSortFM
foldrWithKey' f z   = foldrWithKeyFM' f z . mergeSortFM
foldlWithKey  f x   = foldlWithKeyFM  f x . mergeSortFM
foldlWithKey' f x   = foldlWithKeyFM' f x . mergeSortFM
toOrdSeq            = toSeq . mergeSortFM


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
intersectWith = intersectWithUsingLookupM
difference = differenceUsingDelete
subset = subsetUsingSubsetEq
subsetEq = subsetEqUsingMember
unionWithKey = unionWithKeyUsingInsertWithKey
unionSeqWithKey = unionSeqWithKeyUsingFoldr
intersectWithKey = intersectWithKeyUsingLookupM

-- instance declarations

instance Eq k  => A.AssocX (FM k) k where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert; 
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq; 
   null = null; size = size; member = member; count = count; 
   lookup = lookup; lookupM = lookupM; lookupAll = lookupAll; 
   lookupWithDefault = lookupWithDefault; adjust = adjust; 
   adjustAll = adjustAll; adjustOrInsert = adjustOrInsert;
   map = map; fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   filter = filter; partition = partition; elements = elements;
   structuralInvariant = structuralInvariant; instanceName m = moduleName}

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
   unionSeqWith = unionSeqWith; intersectWith = intersectWith; 
   difference = difference; subset = subset; subsetEq = subsetEq}

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
   intersectWithKey = intersectWithKey}

instance Ord k => A.OrdFiniteMap (FM k) k

instance Eq k => Functor (FM k) where
  fmap =  map

instance (Eq k,Arbitrary k,Arbitrary a) => Arbitrary (FM k a) where
   arbitrary = do xs <- arbitrary
                  return (Prelude.foldr (uncurry insert) empty xs)

   coarbitrary E = variant 0
   coarbitrary (I k a m) = variant 1 . coarbitrary k
                         . coarbitrary a . coarbitrary m

