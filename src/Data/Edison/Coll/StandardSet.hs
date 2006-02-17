-- Copyright (c) 2006 Robert Dockins
-- See COPYRIGHT file for terms and conditions.

-- | The standard library "Data.Set" repackaged as an Edison collection.
module Data.Edison.Coll.StandardSet (
    -- * Set type
    Set,

    -- * CollX operations
    empty,single,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,

    -- * Coll operations
    toSeq,lookup,lookupM,lookupAll,lookupWithDefault,fold,fold1,
    filter,partition,structuralInvariant,

    -- * OrdCollX operations
    deleteMin,deleteMax,unsafeInsertMin,unsafeInsertMax,unsafeFromOrdSeq,
    unsafeAppend,filterLT,filterLE,filterGT,filterGE,partitionLT_GE,
    partitionLE_GT,partitionLT_GT,

    -- * OrdColl operations
    minView,minElem,maxView,maxElem,foldr,foldl,foldr1,foldl1,toOrdSeq,
    unsafeMapMonotonic,

    -- * SetX operations
    intersect,difference,subset,subsetEq,

    -- * Set operations
    fromSeqWith,insertWith,insertSeqWith,unionl,unionr,unionWith,
    unionSeqWith,intersectWith,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import qualified Prelude
import Data.Edison.Prelude
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Coll.Defaults
import Test.QuickCheck

import qualified Data.Set as DS

-- signatures for exported functions
moduleName :: String
empty      :: Set a
single     :: a -> Set a
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

toSeq      :: (Ord a,S.Sequence seq) => Set a -> seq a
lookup     :: Ord a => a -> Set a -> a
lookupM    :: (Ord a,Monad m) => a -> Set a -> m a
lookupAll  :: (Ord a,S.Sequence seq) => a -> Set a -> seq a
lookupWithDefault :: Ord a => a -> a -> Set a  -> a
fold       :: (a -> b -> b) -> b -> Set a -> b
fold1      :: (a -> a -> a) -> Set a -> a
filter     :: Ord a => (a -> Bool) -> Set a -> Set a
partition  :: Ord a => (a -> Bool) -> Set a -> (Set a, Set a)

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

minView       :: (Ord a,Monad m) => Set a -> m (a, Set a)
minElem       :: Set a -> a
maxView       :: (Ord a,Monad m) => Set a -> m (a, Set a)
maxElem       :: Set a -> a
foldr         :: (a -> b -> b) -> b -> Set a -> b
foldl         :: (b -> a -> b) -> b -> Set a -> b
foldr1        :: (a -> a -> a) -> Set a -> a
foldl1        :: (a -> a -> a) -> Set a -> a
toOrdSeq      :: (Ord a,S.Sequence seq) => Set a -> seq a

intersect     :: Ord a => Set a -> Set a -> Set a
difference    :: Ord a => Set a -> Set a -> Set a
subset        :: Ord a => Set a -> Set a -> Bool
subsetEq      :: Ord a => Set a -> Set a -> Bool

fromSeqWith   :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq a -> Set a
insertWith    :: Ord a => (a -> a -> a) -> a -> Set a -> Set a
insertSeqWith :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq a -> Set a -> Set a
unionl       :: Ord a => Set a -> Set a -> Set a
unionr       :: Ord a => Set a -> Set a -> Set a
unionWith    :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unionSeqWith :: (Ord a,S.Sequence seq) => (a -> a -> a) -> seq (Set a) -> Set a
intersectWith :: Ord a => (a -> a -> a) -> Set a -> Set a -> Set a
unsafeMapMonotonic :: Ord a => (a -> a) -> Set a -> Set a

moduleName = "Data.Edison.Coll.StandardSet"

type Set = DS.Set

structuralInvariant :: Ord a => Set a -> Bool
structuralInvariant = DS.valid

empty              = DS.empty
single             = DS.singleton
fromSeq            = fromSeqUsingFoldr
insert             = DS.insert
insertSeq          = insertSeqUsingUnion
union              = DS.union
unionSeq seq       = DS.unions (S.toList seq)
delete             = DS.delete
deleteAll          = DS.delete -- by set property
deleteSeq          = deleteSeqUsingDelete
null               = DS.null
size               = DS.size
member             = DS.member
count              = countUsingMember

toSeq              = toSeqUsingFold
lookup el set      = DS.findMin (DS.intersection set (DS.singleton el))
lookupM            = lookupMUsingLookupAll
lookupAll el set   = toSeqUsingFold (DS.intersection set (DS.singleton el))
lookupWithDefault  = lookupWithDefaultUsingLookupAll
fold               = DS.fold
fold1 f set        = let (x,s) = DS.deleteFindMin set in DS.fold f x s
filter             = DS.filter
partition          = DS.partition

deleteMin          = DS.deleteMin
deleteMax          = DS.deleteMax
unsafeInsertMin    = DS.insert
unsafeInsertMax    = DS.insert
unsafeFromOrdSeq   = DS.fromDistinctAscList . S.toList
unsafeAppend       = DS.union
filterLT x         = DS.filter (<x)
filterLE x         = DS.filter (<=x)
filterGT x         = DS.filter (>x)
filterGE x         = DS.filter (>=x)
partitionLT_GE x   = DS.partition (<x)
partitionLE_GT x   = DS.partition (<=x)
partitionLT_GT     = DS.split

minView set        = if DS.null set 
                        then fail (moduleName ++ ".minView: failed")
                        else return (DS.deleteFindMin set)
minElem            = DS.findMin

maxView set        = if DS.null set
                        then fail (moduleName ++ ".maxView: failed")
                        else return (DS.deleteFindMax set)
maxElem            = DS.findMax

foldr  f x set     = Prelude.foldr  f x (DS.toAscList set)
foldr1 f   set     = Prelude.foldr1 f   (DS.toAscList set)
foldl  f x set     = Prelude.foldl  f x (DS.toAscList set)
foldl1 f   set     = Prelude.foldl1 f   (DS.toAscList set)

toOrdSeq           = S.fromList . DS.toAscList

intersect          = DS.intersection
difference         = DS.difference
subset             = DS.isProperSubsetOf
subsetEq           = DS.isSubsetOf

fromSeqWith        = fromSeqWithUsingInsertWith
insertWith f x set = case lookupM x set of 
                        Nothing -> DS.insert x set
                        Just x' -> DS.insert (f x x') set
insertSeqWith      = insertSeqWithUsingInsertWith
unionl             = DS.union
unionr             = flip DS.union
unionWith          = unionWithUsingOrdLists
unionSeqWith       = unionSeqWithUsingReducer
intersectWith      = intersectWithUsingOrdLists
unsafeMapMonotonic = DS.mapMonotonic



instance Ord a => C.CollX (Set a) a where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   structuralInvariant = structuralInvariant; instanceName c = moduleName}

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
   fold = fold; fold1 = fold1; filter = filter; partition = partition}

instance Ord a => C.OrdColl (Set a) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldl = foldl; foldr1 = foldr1; 
   foldl1 = foldl1; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic }

instance Ord a => C.SetX (Set a) a where
  {intersect = intersect; difference = difference;
   subset = subset; subsetEq = subsetEq}

instance Ord a => C.Set (Set a) a where
  {fromSeqWith = fromSeqWith; insertWith = insertWith; 
   insertSeqWith = insertSeqWith; unionl = unionl; unionr = unionr;
   unionWith= unionWith; unionSeqWith = unionSeqWith;
   intersectWith = intersectWith}

instance Ord a => C.OrdSetX (Set a) a

instance Ord a => C.OrdSet (Set a) a


instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = do xs <- arbitrary
                 return (Prelude.foldr insert empty xs)

  coarbitrary set = coarbitrary (C.toList set)
