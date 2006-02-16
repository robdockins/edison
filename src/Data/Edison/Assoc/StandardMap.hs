-- Copyright (c) 2006 Robert Dockins
-- See COPYRIGHT file for terms and conditions.

-- | The standard library "Data.Map" repackaged as an Edison
--   associative collection.

module Data.Edison.Assoc.StandardMap (
    -- * Type of standard finite maps
    FM,

    -- * AssocX operations
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupWithDefault,adjust,adjustAll,map,fold,fold1,filter,partition,elements,

    -- * FiniteMapX operations
    fromSeqWith,fromSeqWithKey,insertWith,insertWithKey,insertSeqWith,
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectWith,
    difference,subset,subsetEq,

    -- * OrdAssocX operations
    minView, minElem, deleteMin, unsafeInsertMin, maxView, maxElem, deleteMax,
    unsafeInsertMax, foldr, foldl, foldr1, foldl1, unsafeFromOrdSeq,
    unsafeAppend, filterLT, filterLE, filterGT, filterGE,
    partitionLT_GE, partitionLE_GT, partitionLT_GT,

    -- * Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,filterWithKey,partitionWithKey,

    -- * OrdAssoc operations
    minViewWithKey, minElemWithKey, maxViewWithKey, maxElemWithKey,
    foldrWithKey, foldlWithKey, toOrdSeq,

    -- * FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectWithKey,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import qualified Prelude
import Control.Monad.Identity (runIdentity)
import Data.Edison.Prelude
import qualified Data.Edison.Assoc as A
import qualified Data.Edison.Seq as S
import Data.Edison.Assoc.Defaults
import Data.Int
import Data.Bits
import Test.QuickCheck (Arbitrary(..))

import qualified Data.Map as DM

type FM = DM.Map

moduleName :: String
moduleName = "Data.Edison.Assoc.StandardMap"

empty             :: FM k a
single            :: Ord k => k -> a -> FM k a
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
adjust            :: Ord k => (a->a) -> k -> FM k a -> FM k a
adjustAll         :: Ord k => (a->a) -> k -> FM k a -> FM k a
map               :: (Ord k,Functor (FM k)) => (a -> b) -> FM k a -> FM k b
fold              :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1             :: Ord k => (a -> a -> a) -> FM k a -> a
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
intersectWith     :: Ord k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference        :: Ord k => FM k a -> FM k b -> FM k a
subset            :: Ord k => FM k a -> FM k b -> Bool
subsetEq          :: Ord k => FM k a -> FM k b -> Bool


toSeq             :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)
keys              :: (Ord k,S.Sequence seq) => FM k a -> seq k
mapWithKey        :: Ord k => (k -> a -> b) -> FM k a -> FM k b
foldWithKey       :: Ord k => (k -> a -> b -> b) -> b -> FM k a -> b
filterWithKey     :: Ord k => (k -> a -> Bool) -> FM k a -> FM k a
partitionWithKey  :: Ord k => (k -> a -> Bool) -> FM k a -> (FM k a,FM k a)

minViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
minElemWithKey    :: Ord k => FM k a -> (k,a)
maxViewWithKey    :: (Ord k,Monad m) => FM k a -> m ((k, a), FM k a)
maxElemWithKey    :: Ord k => FM k a -> (k,a)
foldrWithKey      :: (k -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey      :: (b -> k -> a -> b) -> b -> FM k a -> b
toOrdSeq          :: (Ord k,S.Sequence seq) => FM k a -> seq (k,a)

unionWithKey      :: Ord k => (k -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey   :: (Ord k,S.Sequence seq) => (k -> a -> a -> a)
                        -> seq (FM k a) -> FM k a
intersectWithKey  :: Ord k => (k -> a -> b -> c) -> FM k a -> FM k b -> FM k c


empty              = DM.empty
single             = DM.singleton
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
lookup k m         = case lookupM k m of
                         Nothing -> error (moduleName ++ ".lookup: failed")
                         Just x  -> x
lookupM            = DM.lookup
lookupAll          = lookupAllUsingLookupM
lookupWithDefault  = DM.findWithDefault
adjust             = DM.adjust
adjustAll          = DM.adjust
map                = fmap
fold               = DM.fold
fold1              = fold1UsingElements
filter             = DM.filter
partition          = DM.partition
elements           = elementsUsingFold

minView m          = if DM.null m
                       then fail (moduleName ++ ".minView: failed")
                       else let ((k,x),m') = DM.deleteFindMin m 
                            in return (x,m')
minElem            = snd . DM.findMin
deleteMin          = DM.deleteMin
unsafeInsertMin    = DM.insert
maxView m          = if DM.null m
                       then fail (moduleName ++ ".maxView: failed")
                       else let ((k,x),m') = DM.deleteFindMax m 
                            in return (x,m')
maxElem            = snd . DM.findMax
deleteMax          = DM.deleteMax
unsafeInsertMax    = DM.insert
foldr  f x m       = Prelude.foldr  f x (DM.elems m)
foldl  f x m       = Prelude.foldl  f x (DM.elems m)
foldr1 f   m       = Prelude.foldr1 f   (DM.elems m)
foldl1 f   m       = Prelude.foldl1 f   (DM.elems m)
unsafeFromOrdSeq   = DM.fromAscList . S.toList
unsafeAppend       = DM.union
filterLT k m       = fst . DM.split k $ m
filterGT k m       = snd . DM.split k $ m
filterLE k m       = DM.filterWithKey (\k' _ -> k' <= k) m
filterGE k m       = DM.filterWithKey (\k' _ -> k' >= k) m
partitionLT_GE k m = DM.partitionWithKey (\k' _ -> k' <  k) m
partitionLE_GT k m = DM.partitionWithKey (\k' _ -> k' <= k) m
partitionLT_GT     = DM.split

fromSeqWith    f s = DM.fromAscListWith    f (S.toList s)
fromSeqWithKey f s = DM.fromAscListWithKey f (S.toList s)
insertWith         = DM.insertWith
insertWithKey      = DM.insertWithKey
insertSeqWith      = insertSeqWithUsingInsertWith
insertSeqWithKey   = insertSeqWithKeyUsingInsertWithKey
unionl             = DM.union
unionr             = flip DM.union
unionWith          = DM.unionWith
unionSeqWith       = unionSeqWithUsingReduce
intersectWith      = DM.intersectionWith
difference         = DM.difference
subset             = DM.isProperSubmapOfBy (\_ _ -> True)
subsetEq           = DM.isSubmapOfBy (\_ _ -> True)

toSeq              = toSeqUsingFoldWithKey
keys               = keysUsingFoldWithKey
mapWithKey         = DM.mapWithKey
foldWithKey        = DM.foldWithKey
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
foldrWithKey       = DM.foldWithKey
foldlWithKey f x m = Prelude.foldl (\b (k,a) -> f b k a) x (DM.toAscList m)
toOrdSeq           = S.fromList . DM.toAscList

unionWithKey       = DM.unionWithKey
unionSeqWithKey    = unionSeqWithKeyUsingReduce
intersectWithKey   = DM.intersectionWithKey


instance Ord k => A.AssocX (FM k) k where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq;
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   lookup = lookup; lookupM = lookupM; lookupAll = lookupAll;
   lookupWithDefault = lookupWithDefault; adjust = adjust;
   adjustAll = adjustAll; map = map; fold = fold; fold1 = fold1;
   filter = filter; partition = partition; elements = elements;
   instanceName m = moduleName}

instance Ord k => A.OrdAssocX (FM k) k where
  {minView = minView; minElem = minElem; deleteMin = deleteMin;
   unsafeInsertMin = unsafeInsertMin; maxView = maxView; maxElem = maxElem;
   deleteMax = deleteMax; unsafeInsertMax = unsafeInsertMax;
   foldr = foldr; foldl = foldl; foldr1 = foldr1; foldl1 = foldl1;
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterGT = filterGT; filterLE = filterLE;
   filterGE = filterGE; partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Ord k => A.FiniteMapX (FM k) k where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey;
   insertWith = insertWith; insertWithKey = insertWithKey;
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey;
   unionl = unionl; unionr = unionr; unionWith = unionWith;
   unionSeqWith = unionSeqWith; intersectWith = intersectWith;
   difference = difference; subset = subset; subsetEq = subsetEq}

instance Ord k => A.OrdFiniteMapX (FM k) k

instance Ord k => A.Assoc (FM k) k where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey;
   foldWithKey = foldWithKey; filterWithKey = filterWithKey;
   partitionWithKey = partitionWithKey}

instance Ord k => A.OrdAssoc (FM k) k where
  {minViewWithKey = minViewWithKey; minElemWithKey = minElemWithKey;
   maxViewWithKey = maxViewWithKey; maxElemWithKey = maxElemWithKey;
   foldrWithKey = foldrWithKey; foldlWithKey = foldlWithKey;
   toOrdSeq = toOrdSeq}

instance Ord k => A.FiniteMap (FM k) k where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey;
   intersectWithKey = intersectWithKey}

instance Ord k => A.OrdFiniteMap (FM k) k

instance (Ord k,Arbitrary k,Arbitrary a) => Arbitrary (FM k a) where
   arbitrary = do xs <- arbitrary
                  return (Prelude.foldr (uncurry insert) empty xs)

   coarbitrary map = coarbitrary (A.toList map)