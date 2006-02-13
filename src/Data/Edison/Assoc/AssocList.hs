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
    lookupWithDefault,adjust,adjustAll,map,fold,fold1,filter,partition,elements,

    -- * Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,filterWithKey,partitionWithKey,

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
import Control.Monad.Identity
import Data.Edison.Prelude
import qualified Data.Edison.Assoc as A ( AssocX(..), Assoc(..), FiniteMapX(..), FiniteMap(..) )
import qualified Data.Edison.Seq as S
import Data.Edison.Assoc.Defaults

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
member        :: Eq k => FM k a -> k -> Bool
count         :: Eq k => FM k a -> k -> Int
lookup        :: Eq k => FM k a -> k -> a
lookupM       :: (Eq k, Monad rm) => FM k a -> k -> rm a
lookupAll     :: (Eq k,S.Sequence seq) => FM k a -> k -> seq a
lookupWithDefault :: Eq k => a -> FM k a -> k -> a
adjust        :: Eq k => (a -> a) -> k -> FM k a -> FM k a
adjustAll     :: Eq k => (a -> a) -> k -> FM k a -> FM k a
map           :: Eq k => (a -> b) -> FM k a -> FM k b
fold          :: Eq k => (a -> b -> b) -> b -> FM k a -> b
fold1         :: Eq k => (a -> a -> a) -> FM k a -> a
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
filterWithKey    :: Eq k => (k -> a -> Bool) -> FM k a -> FM k a
partitionWithKey :: Eq k => (k -> a -> Bool) -> FM k a -> (FM k a, FM k a)

unionWithKey     :: Eq k => (k -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey  :: (Eq k,S.Sequence seq) => 
                        (k -> a -> a -> a) -> seq (FM k a) -> FM k a
intersectWithKey :: Eq k => (k -> a -> b -> c) -> FM k a -> FM k b -> FM k c

moduleName = "Data.Edison.Assoc.AssocList"


data FM k a = E | I k a (FM k a)

-- uncurried insert.  not exported.
uinsert (k,x) = I k x

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

member E key = False
member (I k x m) key = key == k || member m key

count E key = 0
count (I k x m) key | key == k  = 1
                    | otherwise = count m key

lookup m key = runIdentity (lookupM m key)

lookupM E key = fail "AssocList.lookup: lookup failed"
lookupM (I k x m) key | key == k  = return x
                      | otherwise = lookupM m key

lookupAll E key = S.empty
lookupAll (I k x m) key | key == k  = S.single x 
                        | otherwise = lookupAll m key

lookupWithDefault d E key = d
lookupWithDefault d (I k x m) key | key == k = x
                                  | otherwise = lookupWithDefault d m key

elements E = S.empty
elements (I k x m) = S.lcons x (elements (delete k m))

adjust f key E = E
adjust f key (I k x m) | key == k  = I key (f x) m
                       | otherwise = I k x (adjust f key m)

adjustAll = adjust


map f E = E
map f (I k x m) = I k (f x) (map f m)

fold f c E = c
fold f c (I k x m) = fold f (f x c) (delete k m)

fold1 f E = error "AssocList.fold1: empty map"
fold1 f (I k x m) = fold f x (delete k m)

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
   adjustAll = adjustAll; map = map; fold = fold; fold1 = fold1; 
   filter = filter; partition = partition; elements = elements;
   instanceName m = moduleName}

instance Eq k  => A.Assoc (FM k) k where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey; 
   foldWithKey = foldWithKey; filterWithKey = filterWithKey; 
   partitionWithKey = partitionWithKey}

instance Eq k => A.FiniteMapX (FM k) k where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey; 
   insertWith  = insertWith; insertWithKey = insertWithKey; 
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey; 
   unionl = unionl; unionr = unionr; unionWith = unionWith; 
   unionSeqWith = unionSeqWith; intersectWith = intersectWith; 
   difference = difference; subset = subset; subsetEq = subsetEq}

instance Eq k => A.FiniteMap (FM k) k where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey; 
   intersectWithKey = intersectWithKey}

instance Eq k => Functor (FM k) where
  fmap =  map
