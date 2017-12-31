-- |
--   Module      :  Data.Edison.Assoc.AssocList
--   Copyright   :  Copyright (c) 2006, 2008 Robert Dockins
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   The standard library "Data.Map" repackaged as an Edison
--   associative collection.

module Data.Edison.Assoc.StandardMap (
    -- * Type of standard finite maps
    FM,

    -- * AssocX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupAndDelete,lookupAndDeleteM,lookupAndDeleteAll,
    lookupWithDefault,adjust,adjustAll,adjustOrInsert,adjustAllOrInsert,
    adjustOrDelete,adjustOrDeleteAll,strict,strictWith,
    map,fold,fold',fold1,fold1',filter,partition,elements,structuralInvariant,

    -- * FiniteMapX operations
    fromSeqWith,fromSeqWithKey,insertWith,insertWithKey,insertSeqWith,
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectionWith,
    difference,properSubset,subset,properSubmapBy,submapBy,sameMapBy,
    properSubmap,submap,sameMap,

    -- * OrdAssocX operations
    minView, minElem, deleteMin, unsafeInsertMin, maxView, maxElem, deleteMax,
    unsafeInsertMax, foldr, foldr', foldl, foldl', foldr1, foldr1',
    foldl1, foldl1', unsafeFromOrdSeq,
    unsafeAppend, filterLT, filterLE, filterGT, filterGE,
    partitionLT_GE, partitionLE_GT, partitionLT_GT,

    -- * Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,foldWithKey',filterWithKey,partitionWithKey,

    -- * OrdAssoc operations
    minViewWithKey, minElemWithKey, maxViewWithKey, maxElemWithKey,
    foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey', toOrdSeq,

    -- * FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectionWithKey,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import qualified Prelude
import qualified Data.Edison.Assoc as A
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Assoc.Defaults
import Data.Int
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..))

import qualified Data.Map as DM

type FM = DM.Map

moduleName :: String
moduleName = "Data.Edison.Assoc.StandardMap"

empty             :: FM k a
singleton         :: Ord k => k -> a -> FM k a
fromSeq           :: (Ord k,S.Sequence seq) => seq (k,a) -> FM k a
insert            :: Ord k => k -> a -> FM k a -> FM k a
insertSeq         :: (Ord k,S.Sequence seq) => seq (k,a) -> FM k a -> FM k a
union             :: Ord k => FM k a -> FM k a -> FM k a
unionSeq          :: (Ord k,S.Sequence seq) => seq (FM k a) -> FM k a
delete            :: Ord k => k -> FM k a -> FM k a
deleteAll         :: Ord k => k -> FM k a -> FM k a
deleteSeq         :: (Ord k,S.Sequence seq) => seq k -> FM k a -> FM k a
null              :: FM k a -> Bool
size              :: FM k a -> Int
member            :: Ord k => k -> FM k a -> Bool
count             :: Ord k => k -> FM k a -> Int
lookup            :: Ord k => k -> FM k a -> a
lookupAll         :: (Ord k,S.Sequence seq) => k -> FM k a -> seq a
lookupM           :: (Ord k,Monad m) => k -> FM k a -> m a
lookupWithDefault :: Ord k => a -> k -> FM k a -> a
lookupAndDelete   :: Ord k => k -> FM k a -> (a, FM k a)
lookupAndDeleteM  :: (Ord k,Monad m) => k -> FM k a -> m (a, FM k a)
lookupAndDeleteAll :: (Ord k,S.Sequence seq) => k -> FM k a -> (seq a,FM k a)
adjust            :: Ord k => (a->a) -> k -> FM k a -> FM k a
adjustAll         :: Ord k => (a->a) -> k -> FM k a -> FM k a
adjustOrInsert    :: Ord k => (a -> a) -> a -> k -> FM k a -> FM k a
adjustAllOrInsert :: Ord k => (a -> a) -> a -> k -> FM k a -> FM k a
adjustOrDelete    :: Ord k => (a -> Maybe a) -> k -> FM k a -> FM k a
adjustOrDeleteAll :: Ord k => (a -> Maybe a) -> k -> FM k a -> FM k a
strict            :: Ord k => FM k a -> FM k a
strictWith        :: Ord k => (a -> b) -> FM k a -> FM k a
map               :: Ord k => (a -> b) -> FM k a -> FM k b
fold              :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1             :: Ord k => (a -> a -> a) -> FM k a -> a
fold'             :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1'            :: Ord k => (a -> a -> a) -> FM k a -> a
filter            :: Ord k => (a -> Bool) -> FM k a -> FM k a
partition         :: Ord k => (a -> Bool) -> FM k a -> (FM k a,FM k a)
elements          :: (Ord k,S.Sequence seq) => FM k a -> seq a

minView           :: (Ord k,Monad m) => FM k a -> m (a, FM k a)
minElem           :: Ord k => FM k a -> a
deleteMin         :: Ord k => FM k a -> FM k a
unsafeInsertMin   :: Ord k => k -> a -> FM k a -> FM k a
maxView           :: (Ord k,Monad m) => FM k a -> m (a, FM k a)
maxElem           :: Ord k => FM k a -> a
deleteMax         :: Ord k => FM k a -> FM k a
unsafeInsertMax   :: Ord k => k -> a -> FM k a -> FM k a
foldr             :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldl             :: Ord k => (b -> a -> b) -> b -> FM k a -> b
foldr1            :: Ord k => (a -> a -> a) -> FM k a -> a
foldl1            :: Ord k => (a -> a -> a) -> FM k a -> a
foldr'            :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldl'            :: Ord k => (b -> a -> b) -> b -> FM k a -> b
foldr1'           :: Ord k => (a -> a -> a) -> FM k a -> a
foldl1'           :: Ord k => (a -> a -> a) -> FM k a -> a
unsafeFromOrdSeq  :: (Ord k,S.Sequence seq) => seq (k,a) -> FM k a
unsafeAppend      :: Ord k => FM k a -> FM k a -> FM k a
filterLT          :: Ord k => k -> FM k a -> FM k a
filterGT          :: Ord k => k -> FM k a -> FM k a
filterLE          :: Ord k => k -> FM k a -> FM k a
filterGE          :: Ord k => k -> FM k a -> FM k a
partitionLT_GE    :: Ord k => k -> FM k a -> (FM k a,FM k a)
partitionLE_GT    :: Ord k => k -> FM k a -> (FM k a,FM k a)
partitionLT_GT    :: Ord k => k -> FM k a -> (FM k a,FM k a)

fromSeqWith       :: (Ord k,S.Sequence seq) => (a -> a -> a)
                         -> seq (k,a) -> FM k a
fromSeqWithKey    :: (Ord k,S.Sequence seq) => (k -> a -> a -> a)
                         -> seq (k,a) -> FM k a
insertWith        :: Ord k => (a -> a -> a) -> k -> a
                         -> FM k a -> FM k a
insertWithKey     :: Ord k => (k -> a -> a -> a) -> k -> a
                         -> FM k a -> FM k a
insertSeqWith     :: (Ord k,S.Sequence seq) => (a -> a -> a) -> seq (k,a)
                         -> FM k a -> FM k a
insertSeqWithKey  :: (Ord k,S.Sequence seq) => (k -> a -> a -> a) -> seq (k,a)
                         -> FM k a -> FM k a
unionl            :: Ord k => FM k a -> FM k a -> FM k a
unionr            :: Ord k => FM k a -> FM k a -> FM k a
unionWith         :: Ord k => (a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWith      :: (Ord k,S.Sequence seq) =>
                         (a -> a -> a) -> seq (FM k a) -> FM k a
intersectionWith  :: Ord k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference        :: Ord k => FM k a -> FM k b -> FM k a
properSubset      :: Ord k => FM k a -> FM k b -> Bool
subset            :: Ord k => FM k a -> FM k b -> Bool
properSubmapBy    :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
submapBy          :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
sameMapBy         :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
properSubmap      :: (Ord k,Eq a) => FM k a -> FM k a -> Bool
submap            :: (Ord k,Eq a) => FM k a -> FM k a -> Bool
sameMap           :: (Ord k,Eq a) => FM k a -> FM k a -> Bool

toSeq             :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)
keys              :: (Ord k,S.Sequence seq) => FM k a -> seq k
mapWithKey        :: Ord k => (k -> a -> b) -> FM k a -> FM k b
foldWithKey       :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
foldWithKey'      :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
filterWithKey     :: Ord k => (k -> a -> Bool) -> FM k a -> FM k a
partitionWithKey  :: Ord k => (k -> a -> Bool) -> FM k a -> (FM k a,FM k a)

minViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
minElemWithKey    :: Ord k => FM k a -> (k,a)
maxViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
maxElemWithKey    :: Ord k => FM k a -> (k,a)
foldrWithKey      :: (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey      :: (b -> k -> a -> b) -> b -> FM k a -> b
foldrWithKey'     :: (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey'     :: (b -> k -> a -> b) -> b -> FM k a -> b
toOrdSeq          :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)

unionWithKey      :: Ord k => (k -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey   :: (Ord k,S.Sequence seq) => (k -> a -> a -> a)
                        -> seq (FM k a) -> FM k a
intersectionWithKey  :: Ord k => (k -> a -> b -> c) -> FM k a -> FM k b -> FM k c

structuralInvariant :: Ord k => FM k a -> Bool
structuralInvariant = DM.valid


empty              = DM.empty
singleton          = DM.singleton
fromSeq            = fromSeqUsingInsertSeq
insert             = DM.insert
insertSeq          = insertSeqUsingFoldr
union              = DM.union
unionSeq           = DM.unions . S.toList
delete             = DM.delete
deleteAll          = DM.delete -- by finite map property
deleteSeq          = deleteSeqUsingFoldr
null               = DM.null
size               = DM.size
member             = DM.member
count              = countUsingMember
lookup k m         = maybe (error (moduleName ++ ".lookup: failed")) id (DM.lookup k m)
lookupM k m        = maybe (fail (moduleName ++ ".lookupM: failed")) return (DM.lookup k m)
lookupAll          = lookupAllUsingLookupM
lookupWithDefault  = DM.findWithDefault
lookupAndDelete    = lookupAndDeleteDefault
lookupAndDeleteM   = lookupAndDeleteMDefault
lookupAndDeleteAll = lookupAndDeleteAllDefault
adjust             = DM.adjust
adjustAll          = DM.adjust
adjustOrInsert     = adjustOrInsertUsingMember
adjustAllOrInsert  = adjustOrInsertUsingMember
adjustOrDelete     = DM.update
adjustOrDeleteAll  = DM.update
strict xs          = DM.foldr (flip const) () xs `seq` xs
strictWith f xs    = DM.foldr (\x z -> f x `seq` z) () xs `seq` xs
map                = fmap
fold               = DM.foldr
fold' f x xs       = L.foldl' (flip f) x (DM.elems xs)
fold1  f xs        = L.foldr1 f (DM.elems xs)
fold1' f xs        = L.foldl1' (flip f) (DM.elems xs)
filter             = DM.filter
partition          = DM.partition
elements           = elementsUsingFold

minView m          = if DM.null m
                       then fail (moduleName ++ ".minView: failed")
                       else let ((_,x),m') = DM.deleteFindMin m
                            in return (x,m')
minElem            = snd . DM.findMin
deleteMin          = DM.deleteMin
unsafeInsertMin    = DM.insert
maxView m          = if DM.null m
                       then fail (moduleName ++ ".maxView: failed")
                       else let ((_,x),m') = DM.deleteFindMax m
                            in return (x,m')
maxElem            = snd . DM.findMax
deleteMax          = DM.deleteMax
unsafeInsertMax    = DM.insert
foldr   f x m      = L.foldr   f x (DM.elems m)
foldl   f x m      = L.foldl   f x (DM.elems m)
foldr1  f   m      = L.foldr1  f   (DM.elems m)
foldl1  f   m      = L.foldl1  f   (DM.elems m)
foldr'  f x m      = L.foldr'  f x (DM.elems m)
foldl'  f x m      = L.foldl'  f x (DM.elems m)
foldr1' f   m      = L.foldr1' f   (DM.elems m)
foldl1' f   m      = L.foldl1' f   (DM.elems m)
unsafeFromOrdSeq   = DM.fromAscList . S.toList
unsafeAppend       = DM.union
filterLT k         = fst . DM.split k
filterGT k         = snd . DM.split k
filterLE k m       = let (lt, mx, _ ) = DM.splitLookup k m in maybe lt (\x -> insert k x lt) mx
filterGE k m       = let (_ , mx, gt) = DM.splitLookup k m in maybe gt (\x -> insert k x gt) mx
partitionLT_GE k m = let (lt, mx, gt) = DM.splitLookup k m in (lt, maybe gt (\x -> insert k x gt) mx)
partitionLE_GT k m = let (lt, mx, gt) = DM.splitLookup k m in (maybe lt (\x -> insert k x lt) mx, gt)
partitionLT_GT     = DM.split
fromSeqWith    f s = DM.fromListWith    f (S.toList s)
fromSeqWithKey f s = DM.fromListWithKey f (S.toList s)
insertWith         = DM.insertWith
insertWithKey      = insertWithKeyUsingInsertWith
insertSeqWith      = insertSeqWithUsingInsertWith
insertSeqWithKey   = insertSeqWithKeyUsingInsertWithKey
unionl             = DM.union
unionr             = flip DM.union
unionWith          = DM.unionWith
unionSeqWith       = unionSeqWithUsingReduce
intersectionWith   = DM.intersectionWith
difference         = DM.difference
properSubset       = DM.isProperSubmapOfBy (\_ _ -> True)
subset             = DM.isSubmapOfBy (\_ _ -> True)
properSubmapBy     = DM.isProperSubmapOfBy
submapBy           = DM.isSubmapOfBy
sameMapBy          = sameMapByUsingOrdLists
properSubmap       = A.properSubmap
submap             = A.submap
sameMap            = A.sameMap

toSeq              = toSeqUsingFoldWithKey
keys               = keysUsingFoldWithKey
mapWithKey         = DM.mapWithKey
foldWithKey        = DM.foldrWithKey
foldWithKey' f x m = L.foldl' (\b (k,a) -> f k a b) x (DM.toList m)
filterWithKey      = DM.filterWithKey
partitionWithKey   = DM.partitionWithKey

minViewWithKey m   = if DM.null m
                        then fail (moduleName ++ ".minViewWithKey: failed")
                        else return (DM.deleteFindMin m)
minElemWithKey     = DM.findMin
maxViewWithKey m   = if DM.null m
                        then fail (moduleName ++ ".maxViewWithKey: failed")
                        else return (DM.deleteFindMax m)
maxElemWithKey     = DM.findMax
foldrWithKey        = DM.foldrWithKey
foldrWithKey' f x m = L.foldr' (\(k,a) b -> f k a b) x (DM.toAscList m)
foldlWithKey  f x m = L.foldl  (\b (k,a) -> f b k a) x (DM.toAscList m)
foldlWithKey' f x m = L.foldl' (\b (k,a) -> f b k a) x (DM.toAscList m)
toOrdSeq           = S.fromList . DM.toAscList

unionWithKey       = DM.unionWithKey
unionSeqWithKey    = unionSeqWithKeyUsingReduce
intersectionWithKey = DM.intersectionWithKey


instance Ord k => A.AssocX (FM k) k where
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

instance Ord k => A.FiniteMapX (FM k) k where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey;
   insertWith = insertWith; insertWithKey = insertWithKey;
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey;
   unionl = unionl; unionr = unionr; unionWith = unionWith;
   unionSeqWith = unionSeqWith; intersectionWith = intersectionWith;
   difference = difference; properSubset = properSubset; subset = subset;
   properSubmapBy = properSubmapBy; submapBy = submapBy;
   sameMapBy = sameMapBy}

instance Ord k => A.OrdFiniteMapX (FM k) k

instance Ord k => A.Assoc (FM k) k where
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

instance Ord k => A.FiniteMap (FM k) k where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey;
   intersectionWithKey = intersectionWithKey}

instance Ord k => A.OrdFiniteMap (FM k) k
