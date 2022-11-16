-- |
--   Module      :  Data.Edison.Assoc.TernaryTrie
--   Copyright   :  Copyright (c) 2002, 2008 Andrew Bromage
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Finite maps implemented as ternary search tries

module Data.Edison.Assoc.TernaryTrie (
    -- * Type of ternary search tries
    FM,

    -- * AssocX operations
    empty,singleton,fromSeq,insert,insertSeq,union,unionSeq,delete,deleteAll,
    deleteSeq,null,size,member,count,lookup,lookupM,lookupAll,
    lookupAndDelete,lookupAndDeleteM,lookupAndDeleteAll,
    lookupWithDefault,adjust,adjustAll,adjustOrInsert,adjustAllOrInsert,
    adjustOrDelete,adjustOrDeleteAll,strict,strictWith,
    map,fold,fold',fold1,fold1',filter,partition,elements,structuralInvariant,

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

    -- * Other supported operations
    mergeVFM, mergeKVFM,

    -- * Documentation
    moduleName
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,foldl',filter)
import qualified Prelude
import qualified Data.Edison.Assoc as A
import Data.Edison.Prelude ( runFail_ )
import qualified Data.Edison.Seq as S
import qualified Data.List as L
import qualified Control.Monad.Fail as Fail
import Control.Monad
import Data.Coerce (coerce)
import Data.Monoid
import Data.Semigroup as SG
import Data.Maybe (isNothing)

import Data.Edison.Assoc.Defaults
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), Gen(), NonNegative(..), variant, sized, resize, choose, oneof)


-- signatures for exported functions
moduleName    :: String
empty         :: Ord k => FM k a
singleton     :: Ord k => [k] -> a -> FM k a
fromSeq       :: (Ord k,S.Sequence seq) => seq ([k],a) -> FM k a
insert        :: Ord k => [k] -> a -> FM k a -> FM k a
insertSeq     :: (Ord k,S.Sequence seq) => seq ([k],a) -> FM k a -> FM k a
union         :: Ord k => FM k a -> FM k a -> FM k a
unionSeq      :: (Ord k,S.Sequence seq) => seq (FM k a) -> FM k a
delete        :: Ord k => [k] -> FM k a -> FM k a
deleteAll     :: Ord k => [k] -> FM k a -> FM k a
deleteSeq     :: (Ord k,S.Sequence seq) => seq [k] -> FM k a -> FM k a
null          :: Ord k => FM k a -> Bool
size          :: Ord k => FM k a -> Int
member        :: Ord k => [k] -> FM k a -> Bool
count         :: Ord k => [k] -> FM k a -> Int
lookup        :: Ord k => [k] -> FM k a -> a
lookupM       :: (Ord k, Fail.MonadFail rm) => [k] -> FM k a -> rm a
lookupAll     :: (Ord k,S.Sequence seq) => [k] -> FM k a -> seq a
lookupAndDelete    :: Ord k => [k] -> FM k a -> (a, FM k a)
lookupAndDeleteM   :: (Ord k, Fail.MonadFail rm) => [k] -> FM k a -> rm (a, FM k a)
lookupAndDeleteAll :: (Ord k, S.Sequence seq) => [k] -> FM k a -> (seq a,FM k a)
lookupWithDefault  :: Ord k => a -> [k] -> FM k a -> a
adjust        :: Ord k => (a -> a) -> [k] -> FM k a -> FM k a
adjustAll     :: Ord k => (a -> a) -> [k] -> FM k a -> FM k a
adjustOrInsert    :: Ord k => (a -> a) -> a -> [k] -> FM k a -> FM k a
adjustAllOrInsert :: Ord k => (a -> a) -> a -> [k] -> FM k a -> FM k a
adjustOrDelete    :: Ord k => (a -> Maybe a) -> [k] -> FM k a -> FM k a
adjustOrDeleteAll :: Ord k => (a -> Maybe a) -> [k] -> FM k a -> FM k a
strict            :: FM k a -> FM k a
strictWith        :: (a -> b) -> FM k a -> FM k a
map           :: Ord k => (a -> b) -> FM k a -> FM k b
fold          :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1         :: Ord k => (a -> a -> a) -> FM k a -> a
fold'         :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1'        :: Ord k => (a -> a -> a) -> FM k a -> a
filter        :: Ord k => (a -> Bool) -> FM k a -> FM k a
partition     :: Ord k => (a -> Bool) -> FM k a -> (FM k a, FM k a)
elements      :: (Ord k,S.Sequence seq) => FM k a -> seq a

fromSeqWith      :: (Ord k,S.Sequence seq) =>
                        (a -> a -> a) -> seq ([k],a) -> FM k a
fromSeqWithKey   :: (Ord k,S.Sequence seq) => ([k] -> a -> a -> a) -> seq ([k],a) -> FM k a
insertWith       :: Ord k => (a -> a -> a) -> [k] -> a -> FM k a -> FM k a
insertWithKey    :: Ord k => ([k] -> a -> a -> a) -> [k] -> a -> FM k a -> FM k a
insertSeqWith    :: (Ord k,S.Sequence seq) =>
                        (a -> a -> a) -> seq ([k],a) -> FM k a -> FM k a
insertSeqWithKey :: (Ord k,S.Sequence seq) =>
                        ([k] -> a -> a -> a) -> seq ([k],a) -> FM k a -> FM k a
unionl           :: Ord k => FM k a -> FM k a -> FM k a
unionr           :: Ord k => FM k a -> FM k a -> FM k a
unionWith        :: Ord k => (a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWith     :: (Ord k,S.Sequence seq) =>
                        (a -> a -> a) -> seq (FM k a) -> FM k a
intersectionWith :: Ord k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference       :: Ord k => FM k a -> FM k b -> FM k a
properSubset     :: Ord k => FM k a -> FM k b -> Bool
subset           :: Ord k => FM k a -> FM k b -> Bool
properSubmapBy   :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
submapBy         :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
sameMapBy        :: Ord k => (a -> a -> Bool) -> FM k a -> FM k a -> Bool
properSubmap     :: (Ord k, Eq a) => FM k a -> FM k a -> Bool
submap           :: (Ord k, Eq a) => FM k a -> FM k a -> Bool
sameMap          :: (Ord k, Eq a) => FM k a -> FM k a -> Bool

toSeq            :: (Ord k,S.Sequence seq) => FM k a -> seq ([k],a)
keys             :: (Ord k,S.Sequence seq) => FM k a -> seq [k]
mapWithKey       :: Ord k => ([k] -> a -> b) -> FM k a -> FM k b
foldWithKey      :: Ord k => ([k] -> a -> b -> b) -> b -> FM k a -> b
foldWithKey'     :: Ord k => ([k] -> a -> b -> b) -> b -> FM k a -> b
filterWithKey    :: Ord k => ([k] -> a -> Bool) -> FM k a -> FM k a
partitionWithKey :: Ord k => ([k] -> a -> Bool) -> FM k a -> (FM k a, FM k a)
unionWithKey     :: Ord k => ([k] -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey  :: (Ord k,S.Sequence seq) =>
                       ([k] -> a -> a -> a) -> seq (FM k a) -> FM k a
intersectionWithKey :: Ord k => ([k] -> a -> b -> c) -> FM k a -> FM k b -> FM k c

foldr          :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldr1         :: Ord k => (a -> a -> a) -> FM k a -> a
foldr'         :: Ord k => (a -> b -> b) -> b -> FM k a -> b
foldr1'        :: Ord k => (a -> a -> a) -> FM k a -> a

foldrWithKey   :: Ord k => ([k] -> a -> b -> b) -> b -> FM k a -> b
foldrWithKey'  :: Ord k => ([k] -> a -> b -> b) -> b -> FM k a -> b
foldlWithKey   :: Ord k => (b -> [k] -> a -> b) -> b -> FM k a -> b
foldlWithKey'  :: Ord k => (b -> [k] -> a -> b) -> b -> FM k a -> b
toOrdSeq       :: (Ord k,S.Sequence seq) => FM k a -> seq ([k],a)

moduleName = "Data.Edison.Assoc.TernaryTrie"


data FM k a
  = FM !(Maybe a) !(FMB k a)

data FMB k v
  = E
  | I !Int !k !(Maybe v) !(FMB k v) !(FMB' k v) !(FMB k v)
  deriving Show

newtype FMB' k v
  = FMB' (FMB k v)
  deriving Show

balance :: Int
balance = 6

sizeFMB :: FMB k v -> Int
sizeFMB E = 0
sizeFMB (I size _ _ _ _ _) = size

mkFMB :: k -> Maybe v -> FMB k v -> FMB' k v -> FMB k v -> FMB k v
mkFMB k v l m r
  = I (1 + sizeFMB l + sizeFMB r) k v l m r

lookupFMB :: (Ord k) => [k] -> FMB k v -> Maybe v
lookupFMB []        _
  = Nothing
lookupFMB (_:_) E
  = Nothing
lookupFMB nk@(x:xs) (I _ k v l (FMB' fmbm) r)
  = case compare x k of
        LT -> lookupFMB nk l
        GT -> lookupFMB nk r
        EQ -> if L.null xs then v else lookupFMB xs fmbm

listToFMB :: [k] -> (Maybe v -> Maybe v) -> FMB k v
listToFMB [x]    fv = mkFMB x (fv Nothing) E (FMB' E)                 E
listToFMB (x:xs) fv = mkFMB x Nothing      E (FMB' $ listToFMB xs fv) E
listToFMB _ _ = error "TernaryTrie.listToFMB: bug!"

addToFMB :: (Ord k) => [k] -> (Maybe v -> Maybe v) -> FMB k v -> FMB k v
addToFMB xs combiner E
  = listToFMB xs combiner
addToFMB nk@(x:xs) combiner (I size k v l m@(FMB' fmbm) r)
  = case compare x k of
        LT -> mkBalancedFMB k v (addToFMB nk combiner l) m r
        GT -> mkBalancedFMB k v l m (addToFMB nk combiner r)
        EQ -> case xs of
                [] -> I size k (combiner v) l m r
                _  -> I size k v l (FMB' $ addToFMB xs combiner fmbm) r
addToFMB _ _ _ = error "TernaryTrie.addToFMB: bug!"

addToFM :: (Ord k) => [k] -> (Maybe v -> Maybe v) -> FM k v -> FM k v
addToFM [] combiner (FM n fmb)
  = FM (combiner n) fmb
addToFM xs combiner (FM n fmb)
  = FM n (addToFMB xs combiner fmb)

lookupAndDelFromFMB :: (Ord k) => z -> (v -> FMB k v -> z) -> [k] -> FMB k v -> z
lookupAndDelFromFMB onFail _ _ E = onFail
lookupAndDelFromFMB onFail cont nk@(x:xs) (I size k v l m@(FMB' fmbm) r)
  = case compare x k of
        LT -> lookupAndDelFromFMB onFail (\w l' -> cont w (mkBalancedFMB k v l' m r)) nk l
        GT -> lookupAndDelFromFMB onFail (\w r' -> cont w (mkBalancedFMB k v l m r')) nk r
        EQ -> case xs of
                [] -> case v of
                        Nothing -> onFail
                        Just w  -> case fmbm of
                                      E -> cont w (appendFMB l r)
                                      _ -> cont w (I size k Nothing l m r)
                _  -> lookupAndDelFromFMB onFail (\w m' -> cont w (I size k v l (FMB' m') r)) xs fmbm
lookupAndDelFromFMB _ _ _ _ = error "TernaryTrie.lookupAndDelFromFMB: bug!"

lookupAndDelFromFM :: (Ord k) => z -> (v -> FM k v -> z) -> [k] -> FM k v -> z
lookupAndDelFromFM onFail _ [] (FM Nothing _)  = onFail
lookupAndDelFromFM _ cont [] (FM (Just v) fmb) = cont v (FM Nothing fmb)
lookupAndDelFromFM onFail cont xs (FM n fmb) =
   lookupAndDelFromFMB onFail (\w fmb' -> cont w (FM n fmb')) xs fmb


delFromFMB :: (Ord k) => [k] -> FMB k v -> FMB k v
delFromFMB _ E
  = E
delFromFMB nk@(x:xs) (I size k v l m@(FMB' fmbm) r)
  = case compare x k of
        LT -> mkBalancedFMB k v (delFromFMB nk l) m r
        GT -> mkBalancedFMB k v l m (delFromFMB nk r)
        EQ -> case xs of
                [] -> case fmbm of
                        E -> appendFMB l r
                        _ -> I size k Nothing l m r
                _  -> I size k v l (FMB' $ delFromFMB xs fmbm) r
delFromFMB _ _ = error "TernaryTrie.delFromFMB: bug!"


delFromFM :: (Ord k) => [k] -> FM k v -> FM k v
delFromFM [] (FM _ fmb)
  = FM Nothing fmb
delFromFM xs (FM n fmb)
  = FM n (delFromFMB xs fmb)


mkBalancedFMB :: k -> Maybe v -> FMB k v -> FMB' k v -> FMB k v -> FMB k v
mkBalancedFMB k v l m r
  | size_l + size_r < 2
    = mkFMB k v l m r
  | size_r > balance * size_l        -- Right tree too big
    = case r of
        I _ _ _ rl _ rr
            | sizeFMB rl < 2 * sizeFMB rr
                -> single_L l m r
            | otherwise
                -> double_L l m r
        _ -> error "TernaryTrie.mkBalancedFMB: bug!"

  | size_l > balance * size_r   -- Left tree too big
    = case l of
        I _ _ _ ll _ lr
            | sizeFMB lr < 2 * sizeFMB ll
                -> single_R l m r
            | otherwise
                -> double_R l m r
        _ -> error "TernaryTrie.mkBalancedFMB: bug!"

  | otherwise                           -- No imbalance
    = mkFMB k v l m r
  where
        size_l   = sizeFMB l
        size_r   = sizeFMB r

        single_L l m (I _ k_r v_r rl rm rr)
          = mkFMB k_r v_r (mkFMB k v l m rl) rm rr
        single_L _ _ _ = error "TernaryTrie:mkBalancedFMB: bug!"

        double_L l m (I _ k_r v_r (I _ k_rl v_rl rll rlm rlr) rm rr)
          = mkFMB k_rl v_rl (mkFMB k v l m rll) rlm (mkFMB k_r v_r rlr rm rr)
        double_L _ _ _ = error "TernaryTrie:mkBalancedFMB: bug!"

        single_R (I _ k_l v_l ll lm lr) m r
          = mkFMB k_l v_l ll lm (mkFMB k v lr m r)
        single_R _ _ _ = error "TernaryTrie:mkBalancedFMB: bug!"

        double_R (I _ k_l v_l ll lm (I _ k_lr v_lr lrl lrm lrr)) m r
          = mkFMB k_lr v_lr (mkFMB k_l v_l ll lm lrl) lrm (mkFMB k v lrr m r)
        double_R _ _ _ = error "TernaryTrie:mkBalancedFMB: bug!"


mkVBalancedFMB :: k -> Maybe v -> FMB k v -> FMB' k v -> FMB k v -> FMB k v
mkVBalancedFMB k v E m E
  = mkFMB k v E m E
mkVBalancedFMB k v l@E m (I _ kr vr rl rm rr)
  = mkBalancedFMB kr vr (mkVBalancedFMB k v l m rl) rm rr
mkVBalancedFMB k v (I _ kl vl ll lm lr) m r@E
  = mkBalancedFMB kl vl ll lm (mkVBalancedFMB k v lr m r)
mkVBalancedFMB k v l@(I _ kl vl ll lm lr) m r@(I _ kr vr rl rm rr)
  | balance * size_l < size_r
    = mkBalancedFMB kr vr (mkVBalancedFMB k v l m rl) rm rr
  | balance * size_r < size_l
    = mkBalancedFMB kl vl ll lm (mkVBalancedFMB k v lr m r)
  | otherwise
    = mkFMB k v l m r
  where
        size_l = sizeFMB l
        size_r = sizeFMB r

    -- Constraint: All keys in the first FMB are less than
    -- that in the second FMB.
appendFMB :: FMB k v -> FMB k v -> FMB k v
appendFMB E m2 = m2
appendFMB m1 E = m1
appendFMB fmb1@(I size1 k1 v1 l1 m1 r1) fmb2@(I size2 k2 v2 l2 m2 r2)
  | size1 > size2
    = mkVBalancedFMB k1 v1 l1 m1 (appendFMB r1 fmb2)
  | otherwise
    = mkVBalancedFMB k2 v2 (appendFMB fmb1 l2) m2 r2

mapVFM :: (Maybe a -> Maybe b) -> FM k a -> FM k b
mapVFM f (FM n fmb)
  = FM (f n) (mapVFMB f fmb)

mapVFMB :: (Maybe a -> Maybe b) -> FMB k a -> FMB k b
mapVFMB f m
  = mapVFMB' m
  where
        mapVFMB' E = E
        mapVFMB' (I _ k v l (FMB' m) r)
          = case (mapVFMB' m, f v) of
                (E,Nothing) -> appendFMB (mapVFMB' l) (mapVFMB' r)
                (m',v')     -> mkVBalancedFMB k v'
                                    (mapVFMB' l) (FMB' m') (mapVFMB' r)

mapKVFM :: ([k] -> Maybe a -> Maybe b) -> FM k a -> FM k b
mapKVFM f (FM n fmb)
  = FM (f [] n) (mapKVFMB [] fmb)
  where
        mapKVFMB _ E = E
        mapKVFMB ks (I _ k v l (FMB' m) r)
          = mkVBalancedFMB k (f (reverse (k:ks)) v)
              (mapKVFMB ks l)
              (FMB' (mapKVFMB (k:ks) m))
              (mapKVFMB ks r)

nullFMB :: FMB k v -> Bool
nullFMB E = True
nullFMB (I _ _ v l (FMB' m) r)
  = case v of
      Just _  -> False
      Nothing -> nullFMB l && nullFMB m && nullFMB r

nullFM :: FM k v -> Bool
nullFM (FM (Just _) _)  = False
nullFM (FM Nothing fmb) = nullFMB fmb

data FMBCtx k v
  = T
  | L !k !(Maybe v) !(FMBCtx k v) !(FMB' k v) !(FMB k v)
  | R !k !(Maybe v) !(FMB k v) !(FMB' k v) !(FMBCtx k v)

splayFMB :: (Ord k) => k -> FMB k a -> (Maybe a, FMB k a, FMB' k a, FMB k a)
splayFMB key fmb
  = splaydown T fmb
  where
    splaydown ctx E
      = splayup ctx Nothing E (FMB' E) E
    splaydown ctx (I _ k v l m r)
      = case compare key k of
            LT -> splaydown (L k v ctx m r) l
            GT -> splaydown (R k v l m ctx) r
            EQ -> splayup ctx v l m r

    splayup ctx v l m r
      = splayup' ctx l r
      where
          splayup' T l r
            = (v, l, m, r)
          splayup' (L ck cv ctx cm cr) tl tr
            = splayup' ctx tl (mkVBalancedFMB ck cv tr cm cr)
          splayup' (R ck cv cl cm ctx) tl tr
            = splayup' ctx (mkVBalancedFMB ck cv cl cm tl) tr

mergeVFMB :: (Ord k) => (Maybe a -> Maybe b -> Maybe c) ->
                FMB k a -> FMB k b -> FMB k c
mergeVFMB f fmbx fmby
  = mergeVFMB' fmbx fmby
  where
    mergeVFMB' E E
      = E
    mergeVFMB' E fmby@(I _ _ _ _ (FMB' _) _)
      = mapVFMB (\v -> f Nothing v) fmby
    mergeVFMB' fmbx@(I _ _ _ _ (FMB' _) _) E
      = mapVFMB (\v -> f v Nothing) fmbx
    mergeVFMB' fmbx@(I sizex kx vx lx (FMB' mx) rx)
               fmby@(I sizey ky vy ly (FMB' my) ry)
      | sizex >= sizey
        = let (vy, ly, FMB' my, ry) = splayFMB kx fmby
          in case (mergeVFMB' mx my, f vx vy) of
                (E,Nothing) -> appendFMB (mergeVFMB' lx ly) (mergeVFMB' rx ry)
                (m',v)      -> mkVBalancedFMB kx v
                                   (mergeVFMB' lx ly)
                                   (FMB' m')
                                   (mergeVFMB' rx ry)
      | otherwise
        = let (vx, lx, FMB' mx, rx) = splayFMB ky fmbx
          in case (mergeVFMB' mx my, f vx vy) of
                (E,Nothing) -> appendFMB (mergeVFMB' lx ly) (mergeVFMB' rx ry)
                (m',v)      -> mkVBalancedFMB ky v
                                   (mergeVFMB' lx ly)
                                   (FMB' m')
                                   (mergeVFMB' rx ry)

mergeVFM :: (Ord k) => (Maybe a -> Maybe b -> Maybe c) ->
                FM k a -> FM k b -> FM k c
mergeVFM f (FM vx fmbx) (FM vy fmby)
  = FM (f vx vy) (mergeVFMB f fmbx fmby)


mergeKVFMB :: (Ord k) => ([k] -> Maybe a -> Maybe b -> Maybe c) ->
                FMB k a -> FMB k b -> FMB k c
mergeKVFMB f fmbx fmby
  = mergeKVFMB' [] fmbx fmby
  where
    mergeKVFMB' _ E E
      = E
    mergeKVFMB' ks E fmby
      = mergeKVFMBs (\k v -> f k Nothing v) ks fmby
    mergeKVFMB' ks fmbx E
      = mergeKVFMBs (\k v -> f k v Nothing) ks fmbx
    mergeKVFMB' ks fmbx@(I sizex kx vx lx (FMB' mx) rx)
                   fmby@(I sizey ky vy ly (FMB' my) ry)
      | sizex >= sizey
        = let (vy, ly, FMB' my, ry) = splayFMB kx fmby
              ks' = reverse (kx:ks)
          in case (mergeKVFMB' ks' mx my, f ks' vx vy) of
                (E,Nothing) -> appendFMB
                                    (mergeKVFMB' ks lx ly)
                                    (mergeKVFMB' ks rx ry)
                (m',v)      -> mkVBalancedFMB kx v
                                    (mergeKVFMB' ks lx ly)
                                    (FMB' m')
                                    (mergeKVFMB' ks rx ry)
      | otherwise
        = let (vx, lx, FMB' mx, rx) = splayFMB ky fmbx
              ks' = reverse (ky:ks)
          in case (mergeKVFMB' ks' mx my, f ks' vx vy) of
                (E,Nothing) -> appendFMB
                                    (mergeKVFMB' ks lx ly)
                                    (mergeKVFMB' ks rx ry)
                (m',v)      -> mkVBalancedFMB ky v
                                    (mergeKVFMB' ks lx ly)
                                    (FMB' m')
                                    (mergeKVFMB' ks rx ry)

    mergeKVFMBs f ks fmb
      = mergeKVFMBs' ks fmb
      where
          mergeKVFMBs' _ E
            = E
          mergeKVFMBs' ks (I _ k v l (FMB' m) r)
            = case (mergeKVFMBs' (k:ks) m, f (reverse (k:ks)) v) of
                (E, Nothing) -> appendFMB
                                    (mergeKVFMBs' ks l)
                                    (mergeKVFMBs' ks r)
                (m,v)        -> mkVBalancedFMB k v
                                    (mergeKVFMBs' ks l)
                                    (FMB' m)
                                    (mergeKVFMBs' ks r)

mergeKVFM :: (Ord k) => ([k] -> Maybe a -> Maybe b -> Maybe c) ->
                FM k a -> FM k b -> FM k c
mergeKVFM f (FM vx fmbx) (FM vy fmby)
  = FM (f [] vx vy) (mergeKVFMB f fmbx fmby)


-- The public interface.
--

-- AssocX

empty = FM Nothing E

singleton [] v = FM (Just v) E
singleton xs v = FM Nothing (listToFMB xs (\_ -> Just v))

fromSeq = fromSeqUsingInsertSeq

insert k v fm = addToFM k (\_ -> Just v) fm

insertSeq = insertSeqUsingFoldr

union = mergeVFM mplus

unionSeq = unionSeqUsingReduce

delete k fm = delFromFM k fm

deleteAll = delete

deleteSeq = deleteSeqUsingFoldr

null = nullFM

size (FM k fmb)
    | isNothing k = fmb_size fmb 0
    | otherwise   = fmb_size fmb 1
    where fmb_size E k = k
          fmb_size (I _ _ Nothing l (FMB' m) r) k = fmb_size l $ fmb_size m $ fmb_size r k
          fmb_size (I _ _ _ l (FMB' m) r ) k      = fmb_size l $ fmb_size m $ fmb_size r $! k+1


member = memberUsingLookupM

count = countUsingMember

lookup m k = runFail_ (lookupM m k)

lookupM [] (FM Nothing _)
  = fail "TernaryTrie.lookup: lookup failed"
lookupM [] (FM (Just v) _)
  = return v
lookupM xs (FM _ fmb)
  = case  lookupFMB xs fmb  of
        Nothing -> fail "TernaryTrie.lookup: lookup failed"
        Just v  -> return v

lookupAll = lookupAllUsingLookupM

lookupAndDelete =
    lookupAndDelFromFM
      (error "TernaryTrie.lookupAndDelete: lookup failed")
      (,)

lookupAndDeleteM =
    lookupAndDelFromFM
      (fail  "TernaryTrie.lookupAndDeleteM: lookup failed")
      (\w m -> return (w,m))

lookupAndDeleteAll k m =
    lookupAndDelFromFM
      (S.empty,m)
      (\w m' -> (S.singleton w,m'))
      k m

lookupWithDefault = lookupWithDefaultUsingLookupM

adjust f k
  = addToFM k (\mv -> case mv of
                        Nothing -> mv
                        Just v  -> Just (f v))

adjustAll = adjust

adjustOrInsert f z k
  = addToFM k (\mv -> case mv of
                        Nothing -> Just z
                        Just v  -> Just (f v))

adjustAllOrInsert = adjustOrInsert

adjustOrDelete f k
  = addToFM k (\mv -> case mv of
                        Nothing -> mv
                        Just v  -> f v)

adjustOrDeleteAll = adjustOrDelete

map f
  = mapVFM (\mv -> case mv of
                        Nothing -> Nothing
                        Just v  -> Just (f v))

fold = foldr
fold' = foldr'

foldr op z (FM n fmb)
  = foldMV n . foldFMB fmb $ z
  where
    foldMV Nothing  = id
    foldMV (Just v) = op v

    foldFMB E
      = id
    foldFMB (I _ _ v l (FMB' m) r)
      = foldFMB l . foldMV v . foldFMB m . foldFMB r

foldrWithKey f z (FM n fmb)
  = foldMV [] n . foldFMB id fmb $ z
  where
     foldMV _ Nothing  = id
     foldMV ks (Just v) = f ks v

     foldFMB _ E = id
     foldFMB kf (I _ k mv l (FMB' m) r)
       = foldFMB kf l . foldMV (kf [k]) mv . foldFMB (kf . (k:)) m . foldFMB kf r

foldlWithKey f z (FM n fmb)
  = foldFMB id fmb . foldMV [] n $ z
  where
     g k x a = f a k x

     foldMV _ Nothing  = id
     foldMV ks (Just v) = g ks v

     foldFMB _ E = id
     foldFMB kf (I _ k mv l (FMB' m) r)
       = foldFMB kf r . foldFMB (kf . (k:)) m . foldMV (kf [k]) mv . foldFMB kf l

foldrWithKey' = foldrWithKey
foldlWithKey' = foldlWithKey

foldl :: (a -> b -> a) -> a -> FM t b -> a
foldl op z (FM n fmb)
  = foldFMB fmb . foldMV n $ z
  where
    foldMV Nothing  = id
    foldMV (Just v) = (flip op) v

    foldFMB E = id
    foldFMB (I _ _ v l (FMB' m) r)
      = foldFMB r . foldFMB m . foldMV v . foldFMB l


-- FIXME, understand this code to strictify it
foldr' = foldr
foldl' :: (a -> b -> a) -> a -> FM t b -> a
foldl' = foldl

foldr1 f fm =
  case maxView fm of
     Just (z,fm') -> foldr f z fm'
     Nothing      -> error $ moduleName++".foldr1: empty map"

foldl1 :: (b -> b -> b) -> FM k b -> b
foldl1 f fm =
  case minView fm of
     Just (z,fm') -> foldl f z fm'
     Nothing      -> error $ moduleName++".foldl1: empty map"


basecase :: Maybe t1 -> (t1 -> t) -> t -> t
basecase Nothing  = \_ n -> n
basecase (Just x) = \j _ -> j x

comb ::                                (t1 -> t1 -> t1)
                                    -> ((t1 -> t2) -> t2 -> t3)
                                    -> ((t1 -> t) -> t -> t2)
                                    -> (t1 -> t)
                                    -> t
                                    -> t3
comb f p1 p2
   = \j n -> p1 (\x -> p2 (\y -> j (f x y)) (j x)) (p2 j n)

fold1 f (FM mv fmb)
  = comb f (basecase mv) (fold1FMB fmb) id (error $ moduleName++".fold1: empty map")
  where
      fold1FMB E
        = \_ n -> n
      fold1FMB (I _ _ mv l (FMB' m) r)
        = comb f (basecase mv) $ comb f (fold1FMB l) $ comb f (fold1FMB m) $ (fold1FMB r)

fold1' = fold1

{-
FIXME -- can these be somehow fixed to have the right order...

foldr1 f (FM v fmb)
  = comb f (basecase v) (fold1FMB fmb) id (error $ moduleName++".foldr1: empty map")
  where
      fold1FMB E
        = \j n -> n
      fold1FMB (I _ _ v l (FMB' m) r)
        = comb f (fold1FMB l) $ comb f (basecase v) $ comb f (fold1FMB m) $ (fold1FMB r)


foldl1 f (FM v fmb)
  = comb f (fold1FMB fmb) (basecase v) id (error $ moduleName++".foldl1: empty map")
  where
      fold1FMB E
        = \j n -> n
      fold1FMB (I _ _ v l (FMB' m) r)
        = comb f (fold1FMB r) $ comb f (fold1FMB m) $ comb f (basecase v) $ (fold1FMB l)
-}



-- FIXME, understand this code to strictify it
foldr1' = foldr1
foldl1' :: (b -> b -> b) -> FM k b -> b
foldl1' = foldl1


filter p = mapVFM (\mv -> case mv of
                            Nothing -> mv
                            Just v  -> if p v then mv else Nothing)

partition = partitionUsingFilter

elements = elementsUsingFold

strict z@(FM _ fmb) = strictFMB fmb `seq` z
 where strictFMB n@E = n
       strictFMB n@(I _ _ _ l (FMB' m) r) =
           strictFMB l `seq` strictFMB m `seq` strictFMB r `seq` n

strictWith f z@(FM v fmb) = f' v `seq` strictWithFMB fmb `seq` z
   where f' v@Nothing  = v
         f' v@(Just x) = f x `seq` v

         strictWithFMB n@E = n
         strictWithFMB n@(I _ _ v l (FMB' m) r) =
           f' v `seq` strictWithFMB l `seq` strictWithFMB m `seq` strictWithFMB r `seq` n


-- FiniteMapX

fromSeqWith = fromSeqWithUsingInsertSeqWith

fromSeqWithKey = fromSeqWithKeyUsingInsertSeqWithKey

insertWith f k v
  = addToFM k (\vem ->
      case vem of
          Nothing -> Just v
          Just ve -> Just (f ve v))

insertWithKey = insertWithKeyUsingInsertWith

insertSeqWith = insertSeqWithUsingInsertWith

insertSeqWithKey = insertSeqWithKeyUsingInsertWithKey

unionl = union
unionr = flip union

unionWith f = unionWithKey (const f)

unionSeqWith = unionSeqWithUsingReduce

intersectionWith f = intersectionWithKey (const f)

difference mx my
  = mergeVFM (\v1 v2 -> case v2 of
              Nothing -> v1
              Just _  -> Nothing) mx my

properSubset = properSubsetUsingSubset

subset (FM nx fmbx) (FM ny fmby)
  = subsetEqM nx ny && subsetEqFMB fmbx fmby
  where
    subsetEqM Nothing _ = True
    subsetEqM (Just _) Nothing = False
    subsetEqM (Just _) (Just _) = True

    subsetEqFMB E _ = True
    subsetEqFMB fmbx@(I _ _ _ _ _ _) E
      = nullFMB fmbx
    subsetEqFMB fmbx@(I sizex kx vx lx (FMB' mx) rx)
            fmby@(I sizey ky vy ly (FMB' my) ry)
      | sizex >= sizey
        = let (vy, ly, FMB' my, ry) = splayFMB kx fmby
          in    subsetEqM vx vy
             && subsetEqFMB lx ly
             && subsetEqFMB mx my
             && subsetEqFMB rx ry
      | otherwise
        = let (vx, lx, FMB' mx, rx) = splayFMB ky fmbx
          in    subsetEqM vx vy
             && subsetEqFMB lx ly
             && subsetEqFMB mx my
             && subsetEqFMB rx ry


submapBy = submapByUsingLookupM
properSubmapBy = properSubmapByUsingSubmapBy
sameMapBy = sameMapByUsingSubmapBy
properSubmap = A.properSubmap
submap = A.submap
sameMap = A.sameMap

-- Assoc

toSeq = toSeqUsingFoldWithKey

keys = keysUsingFoldWithKey

mapWithKey f
  = mapKVFM (\k mv -> case mv of
          Nothing -> Nothing
          Just v  -> Just (f k v))

foldWithKey op r (FM n fmb)
  = foldWithKeyB [] n . foldWithKeyFM [] fmb $ r
  where
      foldWithKeyB _ Nothing = id
      foldWithKeyB k (Just v) = op k v

      foldWithKeyFM _ E = id
      foldWithKeyFM ks (I _ k v l (FMB' m) r)
        = foldWithKeyFM ks l
        . foldWithKeyB (reverse (k:ks)) v
        . foldWithKeyFM (k:ks) m
        . foldWithKeyFM ks r


-- FIXME, make this strict
foldWithKey' = foldWithKey


filterWithKey f
  = mapKVFM (\k mv -> case mv of
          Nothing -> mv
          Just v  -> if f k v then mv else Nothing)

partitionWithKey f m
  = (filterWithKey f m, filterWithKey (\k v -> not (f k v)) m)

-- FiniteMap

unionWithKey f
  = mergeKVFM (\k v1m v2m ->
    case v1m of
        Nothing -> v2m
        Just v1 ->
            case v2m of
            Nothing -> v1m
            Just v2 -> Just (f k v1 v2))

unionSeqWithKey = unionSeqWithKeyUsingReduce

intersectionWithKey f
  = mergeKVFM (\k v1m v2m ->
    case v1m of
        Nothing -> Nothing
        Just v1 ->
            case v2m of
            Nothing -> Nothing
            Just v2 -> Just (f k v1 v2))

-- OrdAssocX

minViewFMB :: Fail.MonadFail m => FMB k a -> (FMB k a -> FM k a) -> m (a, FM k a)
minViewFMB E _ = fail $ moduleName++".minView: empty map"
minViewFMB (I i k (Just v) E m r)        f = return (v, f (I i k Nothing E m r))
minViewFMB (I _ _ Nothing  E (FMB' E) _) _ = error $ moduleName++".minView: bug!"
minViewFMB (I _ k Nothing  E (FMB' m) r) f = minViewFMB m (\m' -> f (mkVBalancedFMB k Nothing E (FMB' m') r))
minViewFMB (I _ k mv l m r)              f = minViewFMB l (\l' -> f (mkVBalancedFMB k mv l' m r))

minView :: Fail.MonadFail m => FM k a -> m (a,FM k a)
minView (FM (Just v) fmb) = return (v, FM Nothing fmb)
minView (FM Nothing fmb)  = minViewFMB fmb (FM Nothing)

minViewWithKeyFMB :: Fail.MonadFail m => FMB k a -> ([k] -> [k]) -> (FMB k a -> FM k a) -> m (([k],a),FM k a)
minViewWithKeyFMB E _ _ = fail $ moduleName++".minView: empty map"
minViewWithKeyFMB (I i k (Just v) E m r)        kf f = return ((kf [k],v),f (I i k Nothing E m r))
minViewWithKeyFMB (I _ _ Nothing  E (FMB' E) _) _ _ = error $ moduleName++".minViewWithKey: bug!"
minViewWithKeyFMB (I _ k Nothing  E (FMB' m) r) kf f = minViewWithKeyFMB m (kf . (k:))
                                                        (\m' -> f (mkVBalancedFMB k Nothing E (FMB' m') r))
minViewWithKeyFMB (I _ k mv l m r)              kf f = minViewWithKeyFMB l kf
                                                        (\l' -> f (mkVBalancedFMB k mv l' m r))

minViewWithKey :: Fail.MonadFail m => FM k a -> m (([k],a),FM k a)
minViewWithKey (FM (Just v) fmb) = return (([],v),FM Nothing fmb)
minViewWithKey (FM Nothing fmb)  = minViewWithKeyFMB fmb id (FM Nothing)


minElemFMB :: FMB k a -> a
minElemFMB E = error $ moduleName++".minElem: empty map"
minElemFMB (I _ _ (Just v) E _ _)        = v
minElemFMB (I _ _ Nothing  E (FMB' m) _) = minElemFMB m
minElemFMB (I _ _ _ l _ _)              = minElemFMB l

minElem :: FM t1 t -> t
minElem (FM (Just v) _) = v
minElem (FM Nothing  fmb) = minElemFMB fmb


minElemWithKeyFMB :: ([k] -> [k]) -> FMB k a -> ([k],a)
minElemWithKeyFMB _ E = error $ moduleName++".minElemWithKey: empty map"
minElemWithKeyFMB kf (I _ k (Just v) E _ _)        = (kf [k],v)
minElemWithKeyFMB kf (I _ k Nothing  E (FMB' m) _) = minElemWithKeyFMB (kf . (k:)) m
minElemWithKeyFMB kf (I _ _ _ l _ _)              = minElemWithKeyFMB kf l

minElemWithKey :: FM k a -> ([k],a)
minElemWithKey (FM (Just v) _) = ([],v)
minElemWithKey (FM Nothing  fmb) = minElemWithKeyFMB id fmb

deleteMin :: Ord k => FM k a -> FM k a
deleteMin = deleteMinUsingMinView

unsafeInsertMin :: Ord k => [k] -> a -> FM k a -> FM k a
unsafeInsertMin = insert

maxViewFMB :: Fail.MonadFail m => FMB k a -> (FMB k a -> FM k a) -> m (a, FM k a)
maxViewFMB (I _ _ (Just v) l (FMB' E) E) f = return (v, f l)
--maxViewFMB (I i k (Just v) l (FMB' E) E) f = return (v, f (I i k Nothing l (FMB' E) E))
maxViewFMB (I _ _ Nothing  _ (FMB' E) E) _ = error $ moduleName++".maxView: bug!"
maxViewFMB (I i k mv l (FMB' m) E)       f = maxViewFMB m (\m' -> f (I i k mv l (FMB' m') E))
maxViewFMB (I _ k mv l m r)              f = maxViewFMB r (\r' -> f (mkVBalancedFMB k mv l m r'))
maxViewFMB E                             _ = error $ moduleName++".maxView: bug!"

maxView :: Fail.MonadFail m => FM k a -> m (a, FM k a)
maxView (FM Nothing E)  = fail $ moduleName++".maxView: empty map"
maxView (FM (Just v) E) = return (v,FM Nothing E)
maxView (FM mv fmb)     = maxViewFMB fmb (FM mv)


maxViewWithKeyFMB :: Monad m => FMB k a -> ([k] -> [k]) -> (FMB k a -> FM k a) -> m (([k],a),FM k a)
maxViewWithKeyFMB (I _ k (Just v) l (FMB' E) E) kf f = return ((kf [k],v),f l)
maxViewWithKeyFMB (I _ _ Nothing  _ (FMB' E) E) _ _ = error $ moduleName++".maxViewWithKey: bug!"
maxViewWithKeyFMB (I i k mv l (FMB' m) E)       kf f = maxViewWithKeyFMB m (kf . (k:))
                                                        (\m' -> f (I i k mv l (FMB' m') E))
maxViewWithKeyFMB (I _ k mv l m r)              kf f = maxViewWithKeyFMB r kf
                                                        (\r' -> f (mkVBalancedFMB k mv l m r'))
maxViewWithKeyFMB E                             _ _ = error $ moduleName++".maxViewWithKey: bug!"


maxViewWithKey :: Fail.MonadFail m => FM k a -> m (([k],a), FM k a)
maxViewWithKey (FM Nothing E)  = fail $ moduleName++".maxViewWithKey: empty map"
maxViewWithKey (FM (Just v) E) = return (([],v),FM Nothing E)
maxViewWithKey (FM mv fmb)     = maxViewWithKeyFMB fmb id (FM mv)



maxElemFMB :: FMB k a -> a
maxElemFMB (I _ _ (Just v) _ (FMB' E) E) = v
maxElemFMB (I _ _ Nothing  _ (FMB' E) E) = error $ moduleName++".maxElem: bug!"
maxElemFMB (I _ _ _ _ (FMB' m) E)       = maxElemFMB m
maxElemFMB (I _ _ _ _ _ r)              = maxElemFMB r
maxElemFMB E                             = error $ moduleName++".maxElem: bug!"

maxElem :: FM k a -> a
maxElem (FM (Just v) E) = v
maxElem (FM Nothing  E) = error $ moduleName++".maxElem: empty map"
maxElem (FM _ fmb)      = maxElemFMB fmb

maxElemWithKeyFMB :: FMB k a -> ([k] -> [k]) -> ([k],a)
maxElemWithKeyFMB (I _ k (Just v) _ (FMB' E) E) kf = (kf [k],v)
maxElemWithKeyFMB (I _ _ Nothing  _ (FMB' E) E) _ = error $ moduleName++".maxElemWithKey: bug!"
maxElemWithKeyFMB (I _ k _ _ (FMB' m) E)       kf = maxElemWithKeyFMB m (kf . (k:))
maxElemWithKeyFMB (I _ _ _ _ _ r)              kf = maxElemWithKeyFMB r kf
maxElemWithKeyFMB E                             _ = error $ moduleName++".maxElemWithKey: bug!"


maxElemWithKey :: FM k a -> ([k],a)
maxElemWithKey (FM (Just v) E) = ([],v)
maxElemWithKey (FM Nothing E)  = error $ moduleName++".maxElemWithKey: empty map"
maxElemWithKey (FM _ fmb)      = maxElemWithKeyFMB fmb id


deleteMax :: Ord k => FM k a -> FM k a
deleteMax = deleteMaxUsingMaxView

unsafeInsertMax :: Ord k => [k] -> a -> FM k a -> FM k a
unsafeInsertMax = insert

unsafeFromOrdSeq :: (Ord k,S.Sequence seq) => seq ([k],a) -> FM k a
unsafeFromOrdSeq = fromSeq

unsafeAppend :: Ord k => FM k a -> FM k a -> FM k a
unsafeAppend = union

-- FIXME this doesn't respect the structural invariant... why??
{-
unsafeAppend (FM (Just v) fmb1) (FM Nothing fmb2) = FM (Just v) (appendFMB fmb1 fmb2)
unsafeAppend (FM Nothing  fmb1) (FM mv fmb2)      = FM mv       (appendFMB fmb1 fmb2)
unsafeAppend (FM (Just _) _) (FM (Just _) _)      = error $ moduleName++".unsafeAppend: bug!"
-}

filterL_FMB :: Ord k => (k -> Maybe a -> FMB k a -> FMB k a) -> k -> [k] -> FMB k a -> FMB k a
filterL_FMB _ _ _ E = E
filterL_FMB f k ks (I _ key mv l (FMB' m) r)
    | key < k   = mkVBalancedFMB key mv l (FMB' m) (filterL_FMB f k ks r)
    | key > k   = filterL_FMB f k ks l
    | otherwise = case ks of
                    []       -> f k mv l
                    (k':ks') -> mkVBalancedFMB key mv l (FMB' (filterL_FMB f k' ks' m)) E

filterLT :: Ord k => [k] -> FM k a -> FM k a
filterLT [] _               = FM Nothing E
filterLT (k:ks) (FM mv fmb) = FM mv (filterL_FMB (\_ _ l -> l) k ks fmb)

filterLE :: Ord k => [k] -> FM k a -> FM k a
filterLE [] (FM mv _)       = FM mv E
filterLE (k:ks) (FM mv fmb) = FM mv (filterL_FMB (\k mv l -> mkVBalancedFMB k mv l (FMB' E) E) k ks fmb)



filterG_FMB :: Ord k => (k -> Maybe a -> FMB k a -> FMB k a -> FMB k a) -> k -> [k] -> FMB k a -> FMB k a
filterG_FMB _ _ _ E = E
filterG_FMB f k ks (I _ key mv l (FMB' m) r)
    | key < k   = filterG_FMB f k ks r
    | key > k   = mkVBalancedFMB key mv (filterG_FMB f k ks l) (FMB' m) r
    | otherwise = case ks of
                    []       -> f k mv m r
                    (k':ks') -> mkVBalancedFMB key Nothing E (FMB' (filterG_FMB f k' ks' m)) r

filterGT :: Ord k => [k] -> FM k a -> FM k a
filterGT []     (FM _  fmb) = FM Nothing fmb
filterGT (k:ks) (FM _ fmb) = FM Nothing (filterG_FMB (\k _ m r -> mkVBalancedFMB k Nothing E (FMB' m) r) k ks fmb)

filterGE :: Ord k => [k] -> FM k a -> FM k a
filterGE []     fm          = fm
filterGE (k:ks) (FM _ fmb) = FM Nothing (filterG_FMB (\k mv m r -> mkVBalancedFMB k mv E (FMB' m) r) k ks fmb)

--FIXME do better...
partitionLT_GE :: Ord k => [k] -> FM k a -> (FM k a,FM k a)
partitionLT_GE ks fm = (filterLT ks fm, filterGE ks fm)

partitionLE_GT :: Ord k => [k] -> FM k a -> (FM k a,FM k a)
partitionLE_GT ks fm = (filterLE ks fm, filterGT ks fm)

partitionLT_GT :: Ord k => [k] -> FM k a -> (FM k a,FM k a)
partitionLT_GT ks fm = (filterLT ks fm, filterGT ks fm)

toOrdSeq = toOrdSeqUsingFoldrWithKey

-- instance declarations

instance Ord k  => A.AssocX (FM k) [k] where
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

instance Ord k  => A.Assoc (FM k) [k] where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey;
   foldWithKey = foldWithKey; foldWithKey' = foldWithKey';
   filterWithKey = filterWithKey;
   partitionWithKey = partitionWithKey}

instance Ord k => A.FiniteMapX (FM k) [k] where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey;
   insertWith  = insertWith; insertWithKey = insertWithKey;
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey;
   unionl = unionl; unionr = unionr; unionWith = unionWith;
   unionSeqWith = unionSeqWith; intersectionWith = intersectionWith;
   difference = difference; properSubset = properSubset; subset = subset;
   properSubmapBy = properSubmapBy; submapBy = submapBy;
   sameMapBy = sameMapBy}

instance Ord k => A.FiniteMap (FM k) [k] where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey;
   intersectionWithKey = intersectionWithKey}

instance Ord k => A.OrdAssocX (FM k) [k] where
  {minView = minView; minElem = minElem; deleteMin = deleteMin;
   unsafeInsertMin = unsafeInsertMin; maxView = maxView; maxElem = maxElem;
   deleteMax = deleteMax; unsafeInsertMax = unsafeInsertMax;
   foldr = foldr; foldr' = foldr'; foldl = foldl; foldl' = foldl';
   foldr1 = foldr1; foldr1' = foldr1'; foldl1 = foldl1; foldl1' = foldl1';
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend;
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT;
   filterGE = filterGE;  partitionLT_GE = partitionLT_GE;
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance Ord k => A.OrdAssoc (FM k) [k] where
  {minViewWithKey = minViewWithKey; minElemWithKey = minElemWithKey;
   maxViewWithKey = maxViewWithKey; maxElemWithKey = maxElemWithKey;
   foldrWithKey = foldrWithKey; foldrWithKey' = foldrWithKey';
   foldlWithKey = foldlWithKey; foldlWithKey' = foldlWithKey';
   toOrdSeq = toOrdSeq}

instance Ord k => A.OrdFiniteMapX (FM k) [k]
instance Ord k => A.OrdFiniteMap (FM k) [k]


instance Ord k => Functor (FM k) where
  fmap = map

instance (Ord k, Show k, Show a) => Show (FM k a) where
  showsPrec = showsPrecUsingToList

instance (Ord k, Read k, Read a) => Read (FM k a) where
  readsPrec = readsPrecUsingFromList

instance (Ord k, Eq a) => Eq (FM k a) where
  (==) = sameMap

instance (Ord k, Ord a) => Ord (FM k a) where
  compare = compareUsingToOrdList

--
-- Test code follows
--

keyInvariantFMB :: Ord k => (k -> Bool) -> FMB k a -> Bool
keyInvariantFMB _ E = True
keyInvariantFMB p (I _ k _ l _ r)
  =    p k
    && keyInvariantFMB p l
    && keyInvariantFMB p r

actualSizeFMB :: FMB k a -> Int
actualSizeFMB E = 0
actualSizeFMB (I _ _ _ l _ r) = 1 + actualSizeFMB l + actualSizeFMB r

structuralInvariantFMB :: Ord k => FMB k a -> Bool
structuralInvariantFMB E = True
structuralInvariantFMB fmb@(I size k _ l (FMB' m) r)
  =    structuralInvariantFMB l
    && structuralInvariantFMB m
    && structuralInvariantFMB r
    && keyInvariantFMB (<k) l
    && keyInvariantFMB (>k) r
    && actualSizeFMB fmb == size
    && isBalanced l r

isBalanced :: FMB k a -> FMB k a -> Bool
isBalanced l r = sizel + sizer <= 1
  || (sizel <= balance * sizer && sizer <= balance * sizel)
  where
      sizel = sizeFMB l
      sizer = sizeFMB r

structuralInvariant :: Ord k => FM k a -> Bool
structuralInvariant (FM _ fmb) = structuralInvariantFMB fmb

-- | Generate weight-balanced trees either by direct recursion or via
-- 'fromSeq'. The former is much more likely to hit counterexamples to wrong
-- @balance@ coefficients. We keep the latter generator around just in case,
-- because it generates a more realistic distribution.
instance (Integral k, Arbitrary k, Arbitrary a) => Arbitrary (FM k a) where
  arbitrary = oneof [genFM, fromSeq <$> (arbitrary :: Gen [([k], a)])]
  shrink (FM v m) = [FM v m | (v, FMB' m) <- shrinkTuple shrink shrinkFMB' (v, FMB' m)]

instance (Ord k,CoArbitrary k,CoArbitrary a) => CoArbitrary (FM k a) where
  coarbitrary (FM x fmb) = coarbitrary_maybe x . coarbitrary_fmb fmb


coarbitrary_maybe :: (CoArbitrary t) => Maybe t  -> Test.QuickCheck.Gen b
                                                 -> Test.QuickCheck.Gen b
coarbitrary_maybe Nothing = variant (0 :: Int)
coarbitrary_maybe (Just x) = variant (1 :: Int) . coarbitrary x

coarbitrary_fmb :: (CoArbitrary t1, CoArbitrary t) => FMB t t1 -> Gen a -> Gen a
coarbitrary_fmb E = variant (0 :: Int)
coarbitrary_fmb (I _ k x l (FMB' m) r) =
        variant (1 :: Int) . coarbitrary k . coarbitrary_maybe x .
        coarbitrary_fmb l . coarbitrary_fmb m . coarbitrary_fmb r

instance Ord k => Semigroup (FM k a) where
   (<>) = union
instance Ord k => Monoid (FM k a) where
   mempty  = empty
   mappend = (SG.<>)
   mconcat = unionSeq

-- Testing

genFM :: (Integral k, Arbitrary a) => Gen (FM k a)
genFM = do
  FM <$> arbitrary <*> genFMB_

-- Choose the number of elements in the top layer upfront,
-- and distribute it while recursing down.
genFMB_ :: (Integral k, Arbitrary a) => Gen (FMB k a)
genFMB_ = sized $ \sz -> do
  n <- choose (0, sz)
  resize (sz - n) (genFMB 0 n)

-- Distribute the size @sz@ to generate the middle children of the nodes in the
-- top layer.
genFMB :: (Integral k, Arbitrary a) => Int -> Int -> Gen (FMB k a)
genFMB i 0 = pure E
genFMB i n = sized $ \sz -> do
  let b = if n <= 2 then 0 else (n-1+balance) `div` (balance+1)
  l <- choose (b, n-1-b)
  z <- choose (0, sz)
  let k = fromIntegral (i+l)
  I n k
        -- Ensure leaves (nodes with both E children) are nonempty.
    <$> (if n == 1 then Just <$> arbitrary else arbitrary)
    <*> resize z (genFMB i l)
    <*> (FMB' <$> resize (min z (sz-z)) genFMB_)
    <*> resize (sz - z) (genFMB (i+l+1) (n-l-1))

-- Be careful to preserve balance during shrinking.
shrinkFMB :: Arbitrary a => FMB k a -> [FMB k a]
shrinkFMB E = []
shrinkFMB (I s k v l m r) = E : l : r : do
    let (*-) = shrinkTuple ; infixr 3 *-
    (v, (l, (m, r))) <- (shrinkJust *- shrinkFMB *- shrinkFMB' *- shrinkFMB) (v, (l, (m, r)))
    let s = sizeFMB l + sizeFMB r + 1
        t = I s k v l m r
    guard (isBalanced l r)
    pure t

shrinkFMB' :: Arbitrary a => FMB' k a -> [FMB' k a]
shrinkFMB' (FMB' m) = coerce $
  tailsFMB m ++ shrinkFMB m

-- List the middle children of the top layer.
tailsFMB :: FMB k a -> [FMB k a]
tailsFMB E = []
tailsFMB (I _ _ _ l (FMB' m) r) = m : tailsFMB l ++ tailsFMB r

-- Don't remove elements
shrinkJust :: Arbitrary a => Maybe a -> [Maybe a]
shrinkJust Nothing = []
shrinkJust (Just x) = Just <$> shrink x

shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrinkTuple sa sb (a, b) = [(a', b) | a' <- sa a] ++ [(a, b') | b' <- sb b]
