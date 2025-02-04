-- |
--   Module      :  Data.Edison.Assoc.PatriciaLoMap
--   Copyright   :  Copyright (c) 1998, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Finite maps implemented as little-endian Patricia trees.
--
--   /References:/
--
-- * Chris Okasaki and Any Gill.  \"Fast Mergeable Integer Maps\".
--   Workshop on ML, September 1998, pages 77-86.

module Data.Edison.Assoc.PatriciaLoMap (
    -- * Type of little-endian Patricia trees
    FM,

    -- * AssocX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupAndDelete,lookupAndDeleteM,lookupAndDeleteAll,strict,strictWith,
    lookupWithDefault,adjust,adjustAll,adjustOrInsert,adjustAllOrInsert,map,
    fold,fold',fold1,fold1',filter,partition,elements,structuralInvariant,

    -- * Assoc operations
    toSeq,keys,mapWithKey,foldWithKey,foldWithKey',filterWithKey,partitionWithKey,

    -- * FiniteMapX operations
    fromSeqWith,fromSeqWithKey,insertWith,insertWithKey,insertSeqWith,
    insertSeqWithKey,unionl,unionr,unionWith,unionSeqWith,intersectionWith,
    difference,properSubset,subset,properSubmapBy,submapBy,sameMapBy,
    properSubmap,submap,sameMap,

    -- * FiniteMap operations
    unionWithKey,unionSeqWithKey,intersectionWithKey,

    -- * OrdAssocX operations
    minView, minElem, deleteMin, unsafeInsertMin,
    maxView, maxElem, deleteMax, unsafeInsertMax,
    foldr, foldr', foldr1, foldr1', foldl, foldl', foldl1, foldl1',
    unsafeFromOrdSeq, unsafeAppend, filterLT, filterLE, filterGT, filterGE,
    partitionLT_GE, partitionLE_GT, partitionLT_GT,

    -- * OrdAssoc operations
    minViewWithKey, minElemWithKey, maxViewWithKey, maxElemWithKey,
    foldrWithKey, foldrWithKey', foldlWithKey, foldlWithKey',
    toOrdSeq,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,foldl',filter)
import qualified Prelude
import qualified Control.Monad.Fail as Fail
import Data.Monoid
import Data.Semigroup as SG
import qualified Data.Edison.Assoc as A
import Data.Edison.Prelude ( runFail_ )
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Assoc.Defaults
import Data.Int
import Data.Bits
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), variant)

moduleName :: String
moduleName = "Data.Edison.Assoc.PatriciaLoMap"

data FM a
  = E
  | L Int a
  | B Int Int !(FM a) !(FM a)

-- Invariants:
-- * No B node has an E child
-- * first argument to B is a prefix
-- * second argument to B is the "branching bit" and is
--   always an exact power of two
-- * all bits in the prefix >= the branching bit are zeros
-- * valid prefix bits match all subnodes

structuralInvariant :: FM a -> Bool
structuralInvariant E = True
structuralInvariant (L _ _) = True
structuralInvariant x = inv 0 0 x

inv :: Int -> Int -> FM a -> Bool
inv _ _ E = False
inv pre msk (L k _) = k .&. msk == pre
inv pre msk (B p m t0 t1) =
    (p .&. msk == pre) &&
    (bitcount 0 m == 1) &&
    (p .&. (complement (m - 1)) == 0) &&
    inv p0 msk' t0 &&
    inv p1 msk' t1

  where p0 = p
        p1 = p .|. m
        msk' = (m `shiftL` 1) - 1

bitcount :: Int -> Int -> Int
bitcount a 0 = a
bitcount a x = a `seq` bitcount (a+1) (x .&. (x-1))

-- auxiliary functions

makeB :: Int -> Int -> FM t -> FM t -> FM t
makeB _ _ E t = t
makeB _ _ t E = t
makeB p m t0 t1 = B p m t0 t1

lmakeB :: Int -> Int -> FM t -> FM t -> FM t
lmakeB _ _ E t = t
lmakeB p m t0 t1 = B p m t0 t1

rmakeB :: Int -> Int -> FM a -> FM a -> FM a
rmakeB _ _ t E = t
rmakeB p m t0 t1 = B p m t0 t1

lowestBit :: Word -> Word
lowestBit x = x .&. (-x)

branchingBit :: Int -> Int -> Int
branchingBit p0 p1 =
  fromIntegral (lowestBit (fromIntegral p0 `xor` fromIntegral p1))

mask :: Int -> Int -> Int
mask p m = fromIntegral (fromIntegral p .&. (fromIntegral m - (1 :: Word)))

shorter :: Int -> Int -> Bool
shorter m n = fromIntegral m < (fromIntegral n :: Word)

zeroBit :: Int -> Int -> Bool
zeroBit p m = (fromIntegral p) .&. (fromIntegral m) == (0 :: Word)

matchPrefix :: Int -> Int -> Int -> Bool
matchPrefix k p m = mask k m == p

join :: Int -> FM a -> Int -> FM a -> FM a
join p0 t0 p1 t1 =
  let m = branchingBit p0 p1
  in if zeroBit p0 m then B (mask p0 m) m t0 t1
                     else B (mask p0 m) m t1 t0

keepR :: forall t t1. t -> t1 -> t1
keepR _ y = y

-- end auxiliary functions

empty :: FM a
empty = E

singleton :: Int -> a -> FM a
singleton k x = L k x

fromSeq :: S.Sequence seq => seq (Int,a) -> FM a
fromSeq = S.foldl (\t (k, x) -> insert k x t) E

insert :: Int -> a -> FM a -> FM a
insert k x E = L k x
insert k x t@(L j _) = if j == k then L k x else join k (L k x) j t
insert k x t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x t0) t1
                     else B p m t0 (insert k x t1)
    else join k (L k x) p t

union :: FM a -> FM a -> FM a
union s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then B p m (union s0 t) s1
                                 else B p m s0 (union s1 t)
                else join p s q t
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then B q n (union s t0) t1
                                 else B q n t0 (union s t1)
                else join p s q t
  | otherwise = if p == q then B p m (union s0 t0) (union s1 t1)
                else join p s q t
union s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x s0) s1
                     else B p m s0 (insert k x s1)
    else join k (L k x) p s
union s@(B _ _ _ _) E = s
union (L k x) t = insert k x t
union E t = t

delete :: Int -> FM a -> FM a
delete _ E = E
delete k t@(L j _) = if k == j then E else t
delete k t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then lmakeB p m (delete k t0) t1
                     else rmakeB p m t0 (delete k t1)
    else t

null :: FM a -> Bool
null E = True
null _ = False

size :: FM a -> Int
size E = 0
size (L _ _) = 1
size (B _ _ t0 t1) = size t0 + size t1

member :: Int -> FM a -> Bool
member _ E = False
member k (L j _) = (j == k)
member k (B _ m t0 t1) = if zeroBit k m then member k t0 else member k t1

lookup :: Int -> FM a -> a
lookup k m = runFail_ (lookupM k m)

lookupM :: (Fail.MonadFail rm) => Int -> FM a -> rm a
lookupM _ E = fail "PatriciaLoMap.lookup: lookup failed"
lookupM k (L j x)
  | j == k    = return x
  | otherwise = fail "PatriciaLoMap.lookup: lookup failed"
lookupM k (B _ m t0 t1) = if zeroBit k m then lookupM k t0 else lookupM k t1

doLookupAndDelete :: z -> (a -> FM a -> z) -> Int -> FM a -> z
doLookupAndDelete onFail _ _ E = onFail
doLookupAndDelete onFail cont k (L j x)
     | j == k    = cont x E
     | otherwise = onFail
doLookupAndDelete onFail cont k (B p m t0 t1)
     | zeroBit k m = doLookupAndDelete onFail (\x t0' -> cont x (makeB p m t0' t1)) k t0
     | otherwise   = doLookupAndDelete onFail (\x t1' -> cont x (makeB p m t0 t1')) k t1

lookupAndDelete :: Int -> FM a -> (a, FM a)
lookupAndDelete        = doLookupAndDelete
                           (error "PatriciaLoMap.lookupAndDelete: lookup failed")
                           (,)

lookupAndDeleteM :: Fail.MonadFail m => Int -> FM a -> m (a, FM a)
lookupAndDeleteM       = doLookupAndDelete
                           (fail "PatriciaLoMap.lookupAndDelete: lookup failed")
                           (\x m -> return (x,m))

lookupAndDeleteAll :: S.Sequence seq => Int -> FM a -> (seq a,FM a)
lookupAndDeleteAll k m = doLookupAndDelete
                           (S.empty, m)
                           (\x m' -> (S.singleton x,m'))
                           k m


adjust :: (a -> a) -> Int -> FM a -> FM a
adjust _ _ E = E
adjust f k t@(L j x) = if k == j then L k (f x) else t
adjust f k t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (adjust f k t0) t1
                     else B p m t0 (adjust f k t1)
    else t

-- FIXME can we do better than this?
adjustOrInsert :: (a -> a) -> a -> Int -> FM a -> FM a
adjustOrInsert = adjustOrInsertUsingMember

adjustAllOrInsert :: (a -> a) -> a -> Int -> FM a -> FM a
adjustAllOrInsert = adjustOrInsertUsingMember

adjustOrDelete :: (a -> Maybe a) -> Int -> FM a -> FM a
adjustOrDelete = adjustOrDeleteDefault

adjustOrDeleteAll :: (a -> Maybe a) -> Int -> FM a -> FM a
adjustOrDeleteAll = adjustOrDeleteDefault

map :: (a -> b) -> FM a -> FM b
map _ E = E
map f (L k x) = L k (f x)
map f (B p m t0 t1) = B p m (map f t0) (map f t1)

fold :: (a -> b -> b) -> b -> FM a -> b
fold _ c E = c
fold f c (L _ x) = f x c
fold f c (B _ _ t0 t1) = fold f (fold f c t1) t0

fold' :: (a -> b -> b) -> b -> FM a -> b
fold' _ c E = c
fold' f c (L _ x) = c `seq` f x c
fold' f c (B _ _ t0 t1) = c `seq` (fold f $! (fold f c t1)) t0

fold1 :: (a -> a -> a) -> FM a -> a
fold1 _ E = error "PatriciaLoMap.fold1: empty map"
fold1 _ (L _ x) = x
fold1 f (B _ _ t0 t1) = f (fold1 f t0) (fold1 f t1)

fold1' :: (a -> a -> a) -> FM a -> a
fold1' _ E = error "PatriciaLoMap.fold1: empty map"
fold1' _ (L _ x) = x
fold1' f (B _ _ t0 t1) = f (fold1' f t0) $! (fold1' f t1)

filter :: (a -> Bool) -> FM a -> FM a
filter _ E = E
filter g t@(L _ x) = if g x then t else E
filter g (B p m t0 t1) = makeB p m (filter g t0) (filter g t1)

partition :: (a -> Bool) -> FM a -> (FM a, FM a)
partition _ E = (E, E)
partition g t@(L _ x) = if g x then (t, E) else (E, t)
partition g (B p m t0 t1) =
  let (t0',t0'') = partition g t0
      (t1',t1'') = partition g t1
  in (makeB p m t0' t1', makeB p m t0'' t1'')

fromSeqWith :: S.Sequence seq => (a -> a -> a) -> seq (Int,a) -> FM a
fromSeqWith f = S.foldl (\t (k, x) -> insertWith f k x t) E

insertWith :: (a -> a -> a) -> Int -> a -> FM a -> FM a
insertWith _ k x E = L k x
insertWith f k x t@(L j y) = if j == k then L k (f x y) else join k (L k x) j t
insertWith f k x t@(B p m t0 t1) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith f k x t0) t1
                     else B p m t0 (insertWith f k x t1)
    else join k (L k x) p t

unionl :: FM a -> FM a -> FM a
unionl s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionl s0 t) s1
                                 else B p m s0 (unionl s1 t)
                else join p s q t
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionl s t0) t1
                                 else B q n t0 (unionl s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionl s0 t0) (unionl s1 t1)
                else join p s q t
unionl s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith keepR k x s0) s1
                     else B p m s0 (insertWith keepR k x s1)
    else join k (L k x) p s
unionl s@(B _ _ _ _) E = s
unionl (L k x) t = insert k x t
unionl E t = t

unionr :: FM a -> FM a -> FM a
unionr s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionr s0 t) s1
                                 else B p m s0 (unionr s1 t)
                else join p s q t
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionr s t0) t1
                                 else B q n t0 (unionr s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionr s0 t0) (unionr s1 t1)
                else join p s q t
unionr s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insert k x s0) s1
                     else B p m s0 (insert k x s1)
    else join k (L k x) p s
unionr s@(B _ _ _ _) E = s
unionr (L k x) t = insertWith keepR k x t
unionr E t = t

unionWith :: (a -> a -> a) -> FM a -> FM a -> FM a
unionWith f s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionWith f s0 t) s1
                                 else B p m s0 (unionWith f s1 t)
                else join p s q t
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionWith f s t0) t1
                                 else B q n t0 (unionWith f s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionWith f s0 t0) (unionWith f s1 t1)
                else join p s q t
unionWith f s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith (flip f) k x s0) s1
                     else B p m s0 (insertWith (flip f) k x s1)
    else join k (L k x) p s
unionWith _ s@(B _ _ _ _) E = s
unionWith f (L k x) t = insertWith f k x t
unionWith _ E t = t

intersectionWith :: (a -> b -> c) -> FM a -> FM b -> FM c
intersectionWith f s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then intersectionWith f s0 t
                                 else intersectionWith f s1 t
                else E
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then intersectionWith f s t0
                                 else intersectionWith f s t1
                else E
  | otherwise = if p /= q then E
                else makeB p m (intersectionWith f s0 t0) (intersectionWith f s1 t1)
intersectionWith f (B _ m s0 s1) (L k y) =
    case lookupM k (if zeroBit k m then s0 else s1) of
      Just x  -> L k (f x y)
      Nothing -> E
intersectionWith _ (B _ _ _ _) E = E
intersectionWith f (L k x) t =
    case lookupM k t of
      Just y  -> L k (f x y)
      Nothing -> E
intersectionWith _ E _ = E

difference :: FM a -> FM b -> FM a
difference s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then lmakeB p m (difference s0 t) s1
                                 else rmakeB p m s0 (difference s1 t)
                else s
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then difference s t0
                                 else difference s t1
                else s
  | otherwise = if p /= q then s
                else makeB p m (difference s0 t0) (difference s1 t1)
difference s@(B p m s0 s1) (L k _) =
    if matchPrefix k p m then
      if zeroBit k m then lmakeB p m (delete k s0) s1
                     else rmakeB p m s0 (delete k s1)
    else s
difference s@(B _ _ _ _) E = s
difference s@(L k _) t = if member k t then E else s
difference E _ = E

properSubset :: FM a -> FM b -> Bool
properSubset s t = case subset' s t of {LT -> True; _ -> False}

subset' :: FM t -> FM t1 -> Ordering
subset' s@(B p m s0 s1) (B q n t0 t1)
  | shorter m n = GT
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then subset' s t0 SG.<> LT
                                 else subset' s t1 SG.<> LT
                else GT
  | otherwise = if p == q then case (subset' s0 t0,subset' s1 t1) of
                                  (GT,_)  -> GT
                                  (_,GT)  -> GT
                                  (EQ,EQ) -> EQ
                                  (_,_)   -> LT
                else GT
subset' (B _ _ _ _) _ = GT
subset' (L k _) (L j _) = if k == j then EQ else GT
subset' (L k _) t = if member k t then LT else GT
subset' E E = EQ
subset' E _ = LT

subset :: FM a -> FM b -> Bool
subset s@(B p m s0 s1) (B q n t0 t1)
  | shorter m n = False
  | shorter n m = matchPrefix p q n && (if zeroBit p n then subset s t0
                                                     else subset s t1)
  | otherwise = (p == q) && subset s0 t0 && subset s1 t1
subset (B _ _ _ _) _ = False
subset (L k _) t = member k t
subset E _ = True

properSubmapBy :: (a -> a -> Bool) -> FM a -> FM a -> Bool
properSubmapBy = properSubmapByUsingSubmapBy

submapBy :: (a -> a -> Bool) -> FM a -> FM a -> Bool
submapBy = submapByUsingLookupM

sameMapBy :: (a -> a -> Bool) -> FM a -> FM a -> Bool
sameMapBy = sameMapByUsingSubmapBy

properSubmap :: (Eq a) => FM a -> FM a -> Bool
properSubmap = A.properSubmap

submap :: (Eq a) => FM a -> FM a -> Bool
submap = A.submap

sameMap :: (Eq a) => FM a -> FM a -> Bool
sameMap = A.sameMap

mapWithKey :: (Int -> a -> b) -> FM a -> FM b
mapWithKey _ E = E
mapWithKey f (L k x) = L k (f k x)
mapWithKey f (B p m t0 t1) = B p m (mapWithKey f t0) (mapWithKey f t1)

foldWithKey :: (Int -> a -> b -> b) -> b -> FM a -> b
foldWithKey _ c E = c
foldWithKey f c (L k x) = f k x c
foldWithKey f c (B _ _ t0 t1) = foldWithKey f (foldWithKey f c t1) t0

foldWithKey' :: (Int -> a -> b -> b) -> b -> FM a -> b
foldWithKey' _ c E = c
foldWithKey' f c (L k x) = c `seq` f k x c
foldWithKey' f c (B _ _ t0 t1) = c `seq` (foldWithKey f $! (foldWithKey f c t1)) t0


filterWithKey :: (Int -> a -> Bool) -> FM a -> FM a
filterWithKey _ E = E
filterWithKey g t@(L k x) = if g k x then t else E
filterWithKey g (B p m t0 t1) =
  makeB p m (filterWithKey g t0) (filterWithKey g t1)

partitionWithKey :: (Int -> a -> Bool) -> FM a -> (FM a, FM a)
partitionWithKey _ E = (E, E)
partitionWithKey g t@(L k x) = if g k x then (t, E) else (E, t)
partitionWithKey g (B p m t0 t1) =
  let (t0',t0'') = partitionWithKey g t0
      (t1',t1'') = partitionWithKey g t1
  in (makeB p m t0' t1', makeB p m t0'' t1'')

unionWithKey :: (Int -> a -> a -> a) -> FM a -> FM a -> FM a
unionWithKey f s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then B p m (unionWithKey f s0 t) s1
                                 else B p m s0 (unionWithKey f s1 t)
                else join p s q t
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then B q n (unionWithKey f s t0) t1
                                 else B q n t0 (unionWithKey f s t1)
                else join p s q t
  | otherwise = if p == q then B p m (unionWithKey f s0 t0) (unionWithKey f s1 t1)
                else join p s q t
unionWithKey f s@(B p m s0 s1) (L k x) =
    if matchPrefix k p m then
      if zeroBit k m then B p m (insertWith (flip (f k)) k x s0) s1
                     else B p m s0 (insertWith (flip (f k)) k x s1)
    else join k (L k x) p s
unionWithKey _ s@(B _ _ _ _) E = s
unionWithKey f (L k x) t = insertWith (f k) k x t
unionWithKey _ E t = t

intersectionWithKey :: (Int -> a -> b -> c) -> FM a -> FM b -> FM c
intersectionWithKey f s@(B p m s0 s1) t@(B q n t0 t1)
  | shorter m n = if matchPrefix q p m then
                  if zeroBit q m then intersectionWithKey f s0 t
                                 else intersectionWithKey f s1 t
                else E
  | shorter n m = if matchPrefix p q n then
                  if zeroBit p n then intersectionWithKey f s t0
                                 else intersectionWithKey f s t1
                else E
  | otherwise = if p /= q then E
                else makeB p m (intersectionWithKey f s0 t0) (intersectionWithKey f s1 t1)
intersectionWithKey f (B _ m s0 s1) (L k y) =
    case lookupM k (if zeroBit k m then s0 else s1) of
      Just x  -> L k (f k x y)
      Nothing -> E
intersectionWithKey _ (B _ _ _ _) E = E
intersectionWithKey f (L k x) t =
    case lookupM k t of
      Just y  -> L k (f k x y)
      Nothing -> E
intersectionWithKey _ E _ = E

-- Datastructure definition is strict in all submaps,
-- no forcing required
strict :: t -> t
strict n = n

strictWith :: (t -> a) -> FM t -> FM t
strictWith _ n@E = n
strictWith f n@(L _ x) = f x `seq` n
strictWith f n@(B _ _ m1 m2) = strictWith f m1 `seq` strictWith f m2 `seq` n


ordListFM :: FM a -> [(Int,a)]
ordListFM E = []
ordListFM (L k x) = [(k,x)]
ordListFM (B _ _ t0 t1) = merge (ordListFM t0) (ordListFM t1)
  where merge [] ys = ys
        merge xs [] = xs
        merge (x@(k1,_):xs) (y@(k2,_):ys) =
           case compare k1 k2 of
              LT -> x : merge xs (y:ys)
              GT -> y : merge (x:xs) ys
              EQ -> error "PatriciaLoMap: bug in ordListFM"

ordListFM_rev :: FM a -> [(Int,a)]
ordListFM_rev E = []
ordListFM_rev (L k x) = [(k,x)]
ordListFM_rev (B _ _ t0 t1) = merge (ordListFM_rev t0) (ordListFM_rev t1)
  where merge [] ys = ys
        merge xs [] = xs
        merge (x@(k1,_):xs) (y@(k2,_):ys) =
         case compare k1 k2 of
            LT -> y : merge (x:xs) ys
            GT -> x : merge xs (y:ys)
            EQ -> error "PatriciaLoMap: bug in ordListFM_rev"

minView :: Fail.MonadFail m => FM a -> m (a, FM a)
minView fm =
   case ordListFM fm of
     [] -> fail $ moduleName++".minView: empty map"
     ((k,x):_) -> return (x,delete k fm)

minViewWithKey :: Fail.MonadFail m => FM a -> m ((Int, a), FM a)
minViewWithKey fm =
   case ordListFM fm of
     [] -> fail $ moduleName++".minViewWithKey: empty map"
     ((k,x):_) -> return ((k,x),delete k fm)

maxView :: Fail.MonadFail m => FM a -> m (a, FM a)
maxView fm =
  case ordListFM_rev fm of
     [] -> fail $ moduleName++".maxView: empty map"
     ((k,x):_) -> return (x,delete k fm)

maxViewWithKey :: Fail.MonadFail m => FM a -> m ((Int, a), FM a)
maxViewWithKey fm =
   case ordListFM_rev fm of
     [] -> fail $ moduleName++".maxViewWithKey: empty map"
     ((k,x):_) -> return ((k,x),delete k fm)

minElem :: FM a -> a
minElem = minElemUsingMinView

minElemWithKey :: FM a -> (Int,a)
minElemWithKey = minElemWithKeyUsingMinViewWithKey

deleteMin :: FM a -> FM a
deleteMin = deleteMinUsingMinView

unsafeInsertMin :: Int -> a -> FM a -> FM a
unsafeInsertMin = insert

maxElem :: FM a -> a
maxElem = maxElemUsingMaxView

deleteMax :: FM a -> FM a
deleteMax = deleteMaxUsingMaxView

maxElemWithKey :: FM a -> (Int,a)
maxElemWithKey = maxElemWithKeyUsingMaxViewWithKey

unsafeInsertMax :: Int -> a -> FM a -> FM a
unsafeInsertMax = insert

foldr :: (a -> b -> b) -> b -> FM a -> b
foldr f z fm = L.foldr f z . L.map snd . ordListFM $ fm

foldr' :: (a -> b -> b) -> b -> FM a -> b
foldr' f z fm = L.foldl' (flip f) z . L.map snd . ordListFM_rev $ fm

foldr1 :: (a -> a -> a) -> FM a -> a
foldr1 f fm = L.foldr1 f . L.map snd . ordListFM $ fm

foldr1' :: (a -> a -> a) -> FM a -> a
foldr1' f fm = L.foldl1' (flip f) . L.map snd . ordListFM_rev $ fm

foldl :: (b -> a -> b) -> b -> FM a -> b
foldl f z fm = L.foldr (flip f) z . L.map snd . ordListFM_rev $ fm

foldl' :: (b -> a -> b) -> b -> FM a -> b
foldl' f z fm = L.foldl' f z . L.map snd . ordListFM $ fm

foldl1 :: (a -> a -> a) -> FM a -> a
foldl1 f fm = L.foldr1 (flip f) . L.map snd . ordListFM_rev $ fm

foldl1' :: (a -> a -> a) -> FM a -> a
foldl1' f fm = L.foldl1' f . L.map snd . ordListFM $ fm

foldrWithKey :: (Int -> a -> b -> b) -> b -> FM a -> b
foldrWithKey f z fm = L.foldr (uncurry f) z . ordListFM $ fm

foldrWithKey' :: (Int -> a -> b -> b) -> b -> FM a -> b
foldrWithKey' f z fm = L.foldl' (flip (uncurry f)) z . ordListFM_rev $ fm

foldlWithKey :: (b -> Int -> a -> b) -> b -> FM a -> b
foldlWithKey f z fm = L.foldr (\(k,x) a -> f a k x) z . ordListFM_rev $ fm

foldlWithKey' :: (b -> Int -> a -> b) -> b -> FM a -> b
foldlWithKey' f z fm = L.foldl' (\a (k,x) -> f a k x) z . ordListFM $ fm


unsafeFromOrdSeq :: S.Sequence seq => seq (Int,a) -> FM a
unsafeFromOrdSeq = fromSeq

unsafeAppend :: FM a -> FM a -> FM a
unsafeAppend = union

filterLT :: Int -> FM a -> FM a
filterLT k = filterWithKey (\k' _ -> k' < k)

filterLE :: Int -> FM a -> FM a
filterLE k = filterWithKey (\k' _ -> k' <= k)

filterGT :: Int -> FM a -> FM a
filterGT k = filterWithKey (\k' _ -> k' > k)

filterGE :: Int -> FM a -> FM a
filterGE k = filterWithKey (\k' _ -> k' >= k)

partitionLT_GE :: Int -> FM a -> (FM a, FM a)
partitionLT_GE k fm = (filterLT k fm,filterGE k fm)

partitionLE_GT :: Int -> FM a -> (FM a,FM a)
partitionLE_GT k fm = (filterLE k fm,filterGT k fm)

partitionLT_GT :: Int -> FM a -> (FM a,FM a)
partitionLT_GT k fm = (filterLT k fm,filterGT k fm)

toOrdSeq :: S.Sequence seq => FM a -> seq (Int,a)
toOrdSeq = L.foldr S.lcons S.empty . ordListFM

-- defaults

insertSeq :: S.Sequence seq => seq (Int,a) -> FM a -> FM a
insertSeq = insertSeqUsingFoldr

unionSeq :: S.Sequence seq => seq (FM a) -> FM a
unionSeq = unionSeqUsingReduce

deleteAll :: Int -> FM a -> FM a
deleteAll = delete

deleteSeq :: S.Sequence seq => seq Int -> FM a -> FM a
deleteSeq = deleteSeqUsingFoldr

count :: Int -> FM a -> Int
count = countUsingMember

lookupAll :: S.Sequence seq => Int -> FM a -> seq a
lookupAll = lookupAllUsingLookupM

lookupWithDefault :: a -> Int -> FM a -> a
lookupWithDefault = lookupWithDefaultUsingLookupM

elements :: S.Sequence seq => FM a -> seq a
elements = elementsUsingFold

fromSeqWithKey ::
    S.Sequence seq => (Int -> a -> a -> a) -> seq (Int,a) -> FM a
fromSeqWithKey = fromSeqWithKeyUsingInsertSeqWithKey

insertWithKey :: (Int -> a -> a -> a) -> Int -> a -> FM a -> FM a
insertWithKey = insertWithKeyUsingInsertWith

insertSeqWith ::
    S.Sequence seq => (a -> a -> a) -> seq (Int,a) -> FM a -> FM a
insertSeqWith = insertSeqWithUsingInsertWith

insertSeqWithKey ::
    S.Sequence seq =>
      (Int -> a -> a -> a) -> seq (Int,a) -> FM a -> FM a
insertSeqWithKey = insertSeqWithKeyUsingInsertWithKey

adjustAll :: (a -> a) -> Int -> FM a -> FM a
adjustAll = adjust

unionSeqWith :: S.Sequence seq => (a -> a -> a) -> seq (FM a) -> FM a
unionSeqWith = unionSeqWithUsingReduce

toSeq :: S.Sequence seq => FM a -> seq (Int,a)
toSeq = toSeqUsingFoldWithKey

keys :: S.Sequence seq => FM a -> seq Int
keys = keysUsingFoldWithKey

unionSeqWithKey ::
    S.Sequence seq => (Int -> a -> a -> a) -> seq (FM a) -> FM a
unionSeqWithKey = unionSeqWithKeyUsingReduce

-- instance declarations

instance A.AssocX FM Int where
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

instance A.Assoc FM Int where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey;
   foldWithKey = foldWithKey; foldWithKey' = foldWithKey';
   filterWithKey = filterWithKey;
   partitionWithKey = partitionWithKey}

instance A.FiniteMapX FM Int where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey;
   insertWith = insertWith; insertWithKey = insertWithKey;
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey;
   unionl = unionl; unionr = unionr; unionWith = unionWith;
   unionSeqWith = unionSeqWith; intersectionWith = intersectionWith;
   difference = difference; properSubset = properSubset; subset = subset;
   properSubmapBy = properSubmapBy; submapBy = submapBy;
   sameMapBy = sameMapBy}

instance A.FiniteMap FM Int where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey;
   intersectionWithKey = intersectionWithKey}

instance A.OrdAssocX FM Int where
  {minView = minView; minElem = minElem; deleteMin = deleteMin;
   unsafeInsertMin = unsafeInsertMin; maxView = maxView; maxElem = maxElem;
   deleteMax = deleteMax; unsafeInsertMax = unsafeInsertMax;
   foldr = foldr; foldr' = foldr'; foldl = foldl; foldl' = foldl';
   foldr1 = foldr1; foldr1' = foldr1'; foldl1 = foldl1; foldl1' = foldl1';
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterGT = filterGT; filterLE = filterLE;
   filterGE = filterGE; partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance A.OrdAssoc FM Int where
  {minViewWithKey = minViewWithKey; minElemWithKey = minElemWithKey;
   maxViewWithKey = maxViewWithKey; maxElemWithKey = maxElemWithKey;
   foldrWithKey = foldrWithKey; foldrWithKey' = foldrWithKey';
   foldlWithKey = foldlWithKey; foldlWithKey' = foldlWithKey';
   toOrdSeq = toOrdSeq}

instance A.OrdFiniteMapX FM Int
instance A.OrdFiniteMap FM Int

instance Functor FM where
  fmap = map

instance (Show a) => Show (FM a) where
  showsPrec = showsPrecUsingToList

instance (Read a) => Read (FM a) where
  readsPrec = readsPrecUsingFromList

instance (Eq a) => Eq (FM a) where
  (==) = sameMap

instance (Ord a) => Ord (FM a) where
  compare = compareUsingToOrdList

instance (Arbitrary a) => Arbitrary (FM a) where
   arbitrary = do (xs::[(Int,a)]) <- arbitrary
                  return (Prelude.foldr (uncurry insert) empty xs)

instance (CoArbitrary a) => CoArbitrary (FM a) where
   coarbitrary E = variant 0
   coarbitrary (L i a) = variant 1 . coarbitrary i . coarbitrary a
   coarbitrary (B i j m n) = variant 2 . coarbitrary i . coarbitrary j
                           . coarbitrary m . coarbitrary n


instance Semigroup (FM a) where
   (<>) = union
instance Monoid (FM a) where
   mempty  = empty
   mappend = (SG.<>)
   mconcat = unionSeq
