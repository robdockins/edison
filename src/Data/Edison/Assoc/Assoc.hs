-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Assoc.Assoc ( -- associative collections
{-
    -- non-observable classes
    AssocX(..),
    OrdAssocX(..),
    FiniteMapX(..),
    OrdFiniteMapX(..),

    -- observable classes
    Assoc(..),
    OrdAssoc(..),
    FiniteMap(..),
    OrdFiniteMap(..),

    -- specialize sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    lookupList,
    elementsList,
    unsafeFromOrdList,
    fromListWith,
    fromListWithKey,
    insertListWith,
    insertListWithKey,
    unionListWith,
    toList,
    keysList,
    toOrdList,
    unionListWithKey,
-}
    module Data.Edison.Assoc.Assoc
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)

import Data.Edison.EdisonPrelude

import Data.Edison.Seq.Sequence(Sequence)
import Data.Edison.Seq.ListSeq()

-- class (Eq k, Functor (m k)) => AssocX m k | m -> k
class Eq k => AssocX m k | m -> k where
  empty          :: m a

  single         :: k -> a -> m a
  fromSeq        :: Sequence seq => seq (k,a) -> m a

  insert         :: k -> a -> m a -> m a
  insertSeq      :: Sequence seq => seq (k,a) -> m a -> m a

  union          :: m a -> m a -> m a
  unionSeq       :: Sequence seq => seq (m a) -> m a

  delete         :: k -> m a -> m a
  deleteAll      :: k -> m a -> m a
  deleteSeq      :: Sequence seq => seq k -> m a -> m a

  null           :: m a -> Bool
  size           :: m a -> Int

  member         :: m a -> k -> Bool
  count          :: m a -> k -> Int

  lookup         :: m a -> k -> a
  lookupM        :: (Monad rm) => m a -> k -> rm a
  lookupAll      :: Sequence seq => m a -> k -> seq a
  lookupWithDefault  :: a -> m a -> k -> a

  adjust         :: (a -> a) -> k -> m a -> m a
  adjustAll      :: (a -> a) -> k -> m a -> m a

  -- only because can't yet put Functor as superclass
  map            :: (a -> b) -> m a -> m b

  fold           :: (a -> b -> b) -> b -> m a -> b
  fold1          :: (a -> a -> a) -> m a -> a

  filter         :: (a -> Bool) -> m a -> m a
  partition      :: (a -> Bool) -> m a -> (m a, m a)
  
  elements       :: Sequence seq => m a -> seq a

  instanceName   :: m a -> String

class (AssocX m k, Ord k) => OrdAssocX m k | m -> k where
  minView            :: (Monad rm) => m a -> rm (a, m a)
  minElem            :: m a -> a
  deleteMin          :: m a -> m a
  unsafeInsertMin    :: a -> a -> m a -> m a

  maxView            :: (Monad rm) => m a -> rm (m a, a)
  maxElem            :: m a -> a
  deleteMax          :: m a -> m a
  unsafeInsertMax    :: m a -> k -> a -> m a

  foldr              :: (a -> b -> b) -> b -> m a -> b
  foldl              :: (b -> a -> b) -> b -> m a -> b

  foldr1             :: (a -> a -> a) -> m a -> m a
  foldl1             :: (a -> a -> a) -> m a -> m a

  unsafeFromOrdSeq   :: Sequence seq => seq (k,a) -> m a
  unsafeAppend       :: m a -> m a -> m a

  filterLT           :: k -> m a -> m a
  filterLE           :: k -> m a -> m a
  filterGT           :: k -> m a -> m a
  filterGE           :: k -> m a -> m a

  partitionLT_GE     :: k -> m a -> (m a, m a)
  partitionLE_GT     :: k -> m a -> (m a, m a)
  partitionLT_GT     :: k -> m a -> (m a, m a)

class AssocX m k => FiniteMapX m k | m -> k where
  fromSeqWith        :: Sequence seq => (a -> a -> a) -> seq (k,a) -> m a
  fromSeqWithKey     :: Sequence seq => (k -> a -> a -> a) -> seq (k,a) -> m a

  insertWith         :: (a -> a -> a) -> k -> a -> m a -> m a
  insertWithKey      :: (k -> a -> a -> a) -> k -> a -> m a -> m a

  insertSeqWith      :: Sequence seq => 
                           (a -> a -> a) -> seq (k,a) -> m a -> m a
  insertSeqWithKey   :: Sequence seq => 
                           (k -> a -> a -> a) -> seq (k,a) -> m a -> m a

  unionl             :: m a -> m a -> m a
  unionr             :: m a -> m a -> m a
  unionWith          :: (a -> a -> a) -> m a -> m a -> m a

  unionSeqWith       :: Sequence seq => (a -> a -> a) -> seq (m a) -> m a

  intersectWith      :: (a -> b -> c) -> m a -> m b -> m c

  difference         :: m a -> m b -> m a

  subset             :: m a -> m b -> Bool    
  subsetEq           :: m a -> m b -> Bool    

class (OrdAssocX m k, FiniteMapX m k) => OrdFiniteMapX m k | m -> k
  -- no methods?


class AssocX m k => Assoc m k | m -> k where
  toSeq             :: Sequence seq => m a -> seq (k,a)
  keys              :: Sequence seq => m a -> seq k
  
  mapWithKey        :: (k -> a -> b) -> m a -> m b
  foldWithKey       :: (k -> a -> b -> b) -> b -> m a -> b

  filterWithKey     :: (k -> a -> Bool) -> m a -> m a
  partitionWithKey  :: (k -> a -> Bool) -> m a -> (m a, m a)

class (Assoc m k, OrdAssocX m k) => OrdAssoc m k | m -> k where
  minViewWithKey  :: (Monad rm) => m a -> rm (k, a, m a)
  minElemWithKey  :: m a -> (k,a)

  maxViewWithKey  :: (Monad rm) => m a -> rm (m a, k, a)
  maxElemWithKey  :: m a -> (k,a)

  foldrWithKey    :: (k -> a -> b -> b) -> b -> m a -> b
  foldlWithKey    :: (b -> k -> a -> b) -> b -> m a -> b

  toOrdSeq        :: Sequence seq => m a -> seq (k,a)

class (Assoc m k, FiniteMapX m k) => FiniteMap m k | m -> k where
  unionWithKey      :: (k -> a -> a -> a) -> m a -> m a -> m a
  unionSeqWithKey   :: Sequence seq => (k -> a -> a -> a) -> seq (m a) -> m a

  intersectWithKey  :: (k -> a -> b -> c) -> m a -> m b -> m c

class (OrdAssoc m k, FiniteMap m k) => OrdFiniteMap m k | m -> k
  -- no methods


-- specialize sequence operations to lists

fromList          :: AssocX m k => [(k,a)] -> m a
insertList        :: AssocX m k => [(k,a)] -> m a -> m a
unionList         :: AssocX m k => [m a] -> m a
deleteList        :: AssocX m k => [k] -> m a -> m a
lookupList        :: AssocX m k => m a -> k -> [a]
elementsList      :: AssocX m k => m a -> [a]
unsafeFromOrdList :: OrdAssocX m k => [(k,a)] -> m a
fromListWith      :: FiniteMapX m k => (a -> a -> a) -> [(k,a)] -> m a
fromListWithKey   :: FiniteMapX m k => (k -> a -> a -> a) -> [(k,a)] -> m a
insertListWith    :: FiniteMapX m k => 
                         (a -> a -> a) -> [(k,a)] -> m a -> m a
insertListWithKey :: FiniteMapX m k => 
                         (k -> a -> a -> a) -> [(k,a)] -> m a -> m a
unionListWith     :: FiniteMapX m k => (a -> a -> a) -> [m a] -> m a
toList            :: Assoc m k => m a -> [(k,a)]
keysList          :: Assoc m k => m a -> [k]
toOrdList         :: OrdAssoc m k => m a -> [(k,a)]
unionListWithKey  :: FiniteMap m k => (k -> a -> a -> a) -> [m a] -> m a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
lookupList = lookupAll
elementsList = elements
unsafeFromOrdList = unsafeFromOrdSeq
fromListWith = fromSeqWith
fromListWithKey = fromSeqWithKey
insertListWith = insertSeqWith
insertListWithKey = insertSeqWithKey
unionListWith = unionSeqWith
toList = toSeq
keysList = keys
toOrdList = toOrdSeq
unionListWithKey = unionSeqWithKey


{-
Leave out until somebody asks for:
witness????
compose????

  nub           :: m a -> m a  -- ???
  nubWith       :: (a -> a -> a) -> m a -> m a
  nubWithKey :: (k -> a -> a -> a) -> m a -> m a

  group         :: m a -> m [a] -- ???
?????  unsafeMapMonotonim :: (a -> a) -> m a -> m a


-- adjustPartial??? (adjustOrDelete???)
-- adjustAll       :: (a -> a) -> k -> m a -> m a
-- unionMap???
-- mapPartial???

  anyViewKey :: (Monad rm) => m a -> rm (k, a, m a)
  anyKeyElem :: m a -> (k,a) -- signals error if collection is empty
  deleteAny :: m a -> m a -- could go in AssocX but no point
    -- anyKeyElem and deleteAny must be consistent
    -- do they need to be consistent with anyView?

-- unionMap???
-- mapPartial???

  deleteAllList :: [k] -> m a -> m a

  disjoint      :: m a -> m b -> Bool

-}

