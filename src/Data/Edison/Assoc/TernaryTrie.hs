-- Copyright (c) 2002 Andrew Bromage.  
-- See COPYRIGHT file for terms and conditions.

-- | Finite maps implemented as ternary search tries.
module Data.Edison.Assoc.TernaryTrie (
    -- * Type of ternary search tries
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

    -- * Other supported operations
    mergeKVFM,

    -- * Documentation
    moduleName,

    -- * Unit testing
    structuralInvariantFM
) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)
import Data.Edison.Prelude
import qualified Data.Edison.Assoc as A ( AssocX(..), Assoc(..), FiniteMapX(..), FiniteMap(..) )
import qualified Data.Edison.Seq as S
import qualified Data.List as L
import Control.Monad.Identity
import Data.Edison.Assoc.Defaults

import Debug.Trace

import Maybe (isNothing)

-- signatures for exported functions
moduleName    :: String
empty         :: Ord k => FM k a
single        :: Ord k => [k] -> a -> FM k a
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
member        :: Ord k => FM k a -> [k] -> Bool
count         :: Ord k => FM k a -> [k] -> Int
lookup        :: Ord k => FM k a -> [k] -> a
lookupM       :: (Ord k, Monad rm) => FM k a -> [k] -> rm a
lookupAll     :: (Ord k,S.Sequence seq) => FM k a -> [k] -> seq a
lookupWithDefault :: Ord k => a -> FM k a -> [k] -> a
adjust        :: Ord k => (a -> a) -> [k] -> FM k a -> FM k a
adjustAll     :: Ord k => (a -> a) -> [k] -> FM k a -> FM k a
map           :: Ord k => (a -> b) -> FM k a -> FM k b
fold          :: Ord k => (a -> b -> b) -> b -> FM k a -> b
fold1         :: Ord k => (a -> a -> a) -> FM k a -> a
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
intersectWith    :: Ord k => (a -> b -> c) -> FM k a -> FM k b -> FM k c
difference       :: Ord k => FM k a -> FM k b -> FM k a
subset           :: Ord k => FM k a -> FM k b -> Bool    
subsetEq         :: Ord k => FM k a -> FM k b -> Bool    

toSeq            :: (Ord k,S.Sequence seq) => FM k a -> seq ([k],a)
keys             :: (Ord k,S.Sequence seq) => FM k a -> seq [k]
mapWithKey       :: Ord k => ([k] -> a -> b) -> FM k a -> FM k b
foldWithKey      :: Ord k => ([k] -> a -> b -> b) -> b -> FM k a -> b
filterWithKey    :: Ord k => ([k] -> a -> Bool) -> FM k a -> FM k a
partitionWithKey :: Ord k => ([k] -> a -> Bool) -> FM k a -> (FM k a, FM k a)
unionWithKey     :: Ord k => ([k] -> a -> a -> a) -> FM k a -> FM k a -> FM k a
unionSeqWithKey  :: (Ord k,S.Sequence seq) => 
                       ([k] -> a -> a -> a) -> seq (FM k a) -> FM k a
intersectWithKey :: Ord k => ([k] -> a -> b -> c) -> FM k a -> FM k b -> FM k c

moduleName = "TernaryTrie"


data FM k a
  = FM !(Maybe a) !(FMB k a)
          deriving (Show)

data FMB k v
  = E
  | I !Int !k !(Maybe v) !(FMB k v) !(FMB' k v) !(FMB k v)
          deriving (Show)

data FMB' k v
  = FMB' !(FMB k v)
      deriving (Show)

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
lookupFMB nk@(x:xs) E
  = Nothing
lookupFMB nk@(x:xs) (I _ k v l m@(FMB' fmbm) r)
  = case compare x k of
        LT -> lookupFMB nk l
        GT -> lookupFMB nk r
        EQ -> if L.null xs then v else lookupFMB xs fmbm

listToFMB :: [k] -> (Maybe v -> Maybe v) -> FMB k v
listToFMB [x]    fv = mkFMB x (fv Nothing) E (FMB' E)                 E
listToFMB (x:xs) fv = mkFMB x Nothing      E (FMB' $ listToFMB xs fv) E

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

addToFM :: (Ord k) => [k] -> (Maybe v -> Maybe v) -> FM k v -> FM k v
addToFM [] combiner (FM n fmb)
  = FM (combiner n) fmb
addToFM xs combiner (FM n fmb)
  = FM n (addToFMB xs combiner fmb)


delFromFMB :: (Ord k) => [k] -> FMB k v -> FMB k v
delFromFMB xs E
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


delFromFM :: (Ord k) => [k] -> FM k v -> FM k v
delFromFM [] (FM n fmb)
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
  | size_l > balance * size_r   -- Left tree too big
    = case l of
        I _ _ _ ll _ lr
            | sizeFMB lr < 2 * sizeFMB ll
                -> single_R l m r
            | otherwise
                -> double_R l m r
  | otherwise                           -- No imbalance
    = mkFMB k v l m r
  where
        size_l   = sizeFMB l
        size_r   = sizeFMB r

        single_L l m (I _ k_r v_r rl rm rr)
          = mkFMB k_r v_r (mkFMB k v l m rl) rm rr

        double_L l m (I _ k_r v_r (I _ k_rl v_rl rll rlm rlr) rm rr)
          = mkFMB k_rl v_rl (mkFMB k v l m rll) rlm (mkFMB k_r v_r rlr rm rr)

        single_R (I _ k_l v_l ll lm lr) m r
          = mkFMB k_l v_l ll lm (mkFMB k v lr m r)

        double_R (I _ k_l v_l ll lm (I _ k_lr v_lr lrl lrm lrr)) m r
          = mkFMB k_lr v_lr (mkFMB k_l v_l ll lm lrl) lrm (mkFMB k v lrr m r)


mkVBalancedFMB :: k -> Maybe v -> FMB k v -> FMB' k v -> FMB k v -> FMB k v
mkVBalancedFMB k v E m E
  = mkFMB k v E m E
mkVBalancedFMB k v l@E m r@(I _ kr vr rl rm rr)
  = mkBalancedFMB kr vr (mkVBalancedFMB k v l m rl) rm rr
mkVBalancedFMB k v l@(I _ kl vl ll lm lr) m r@E
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
        mapKVFMB ks (I size k v l (FMB' m) r)
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
      deriving (Show)

splayFMB :: (Ord k) => k -> FMB k a -> (Maybe a, FMB k a, FMB' k a, FMB k a)
splayFMB key fmb
  = splaydown T fmb
  where
    splaydown ctx E
      = splayup ctx Nothing E (FMB' E) E
    splaydown ctx y@(I _ k v l m r)
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
    mergeVFMB' E fmby@(I _ k v l (FMB' m) r)
      = mapVFMB (\v -> f Nothing v) fmby
    mergeVFMB' fmbx@(I _ k v l (FMB' m) r) E
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
    mergeKVFMB' ks E E
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
          mergeKVFMBs' ks E
            = E
          mergeKVFMBs' ks (I s k v l (FMB' m) r)
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

single [] v = FM (Just v) E
single xs v = FM Nothing (listToFMB xs (\_ -> Just v))

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

lookup m k = runIdentity (lookupM m k)

lookupM (FM Nothing _) []
  = fail "TernaryTrie.lookup: lookup failed"
lookupM (FM (Just v) _) []
  = return v
lookupM (FM _ fmb) xs
  = case  lookupFMB xs fmb  of
        Nothing -> fail "TernaryTrie.lookup: lookup failed"
        Just v  -> return v

lookupAll = lookupAllUsingLookupM

lookupWithDefault = lookupWithDefaultUsingLookupM

adjust f k
  = addToFM k (\mv -> case mv of
                        Nothing -> mv
                        Just v  -> Just (f v))

adjustAll = adjust

map f
  = mapVFM (\mv -> case mv of
                        Nothing -> Nothing
                        Just v  -> Just (f v))

fold op r (FM n fmb)
  = foldMV n . foldFMB fmb $ r
  where
    foldMV Nothing  = id
    foldMV (Just v) = op v

    foldFMB E
      = id
    foldFMB (I _ _ v l (FMB' m) r)
      = foldFMB l . foldMV v . foldFMB m . foldFMB r

fold1 f (FM v fmb)
  = comb (basecase v) (fold1FMB fmb) id (error "TernaryTrie.fold1: empty map")
  where
      basecase Nothing  = \j n -> n
      basecase (Just x) = \j n -> j x

      comb p1 p2
        = \j n -> p1 (\x -> p2 (\y -> j (f x y)) (j x)) (p2 j n)

      fold1FMB E
        = \j n -> n
      fold1FMB (I _ _ v l (FMB' m) r)
        = comb (fold1FMB l) (comb (basecase v) (comb (fold1FMB l)
          (comb (fold1FMB m) (fold1FMB r))))

filter p = mapVFM (\mv -> case mv of
                            Nothing -> mv
                            Just v  -> if p v then mv else Nothing)

partition = partitionUsingFilter

elements = elementsUsingFold

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
unionr = flip unionr

unionWith f = unionWithKey (const f)

unionSeqWith = unionSeqWithUsingReduce

intersectWith f = intersectWithKey (const f)

difference mx my
  = mergeVFM (\v1 v2 -> case v2 of
              Nothing -> v1
              Just _  -> Nothing) mx my

subset = subsetUsingSubsetEq

subsetEq (FM nx fmbx) (FM ny fmby)
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
      foldWithKeyB k Nothing = id
      foldWithKeyB k (Just v) = op k v

      foldWithKeyFM ks E = id
      foldWithKeyFM ks (I _ k v l (FMB' m) r)
        = foldWithKeyFM ks l
        . foldWithKeyB (reverse (k:ks)) v
        . foldWithKeyFM (k:ks) m
        . foldWithKeyFM ks r

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

intersectWithKey f
  = mergeKVFM (\k v1m v2m ->
    case v1m of
        Nothing -> Nothing
        Just v1 ->
            case v2m of
            Nothing -> Nothing
            Just v2 -> Just (f k v1 v2))


-- instance declarations

instance Ord k  => A.AssocX (FM k) [k] where
  {empty = empty; single = single; fromSeq = fromSeq; insert = insert; 
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq; 
   null = null; size = size; member = member; count = count; 
   lookup = lookup; lookupM = lookupM; lookupAll = lookupAll; 
   lookupWithDefault = lookupWithDefault; adjust = adjust; 
   adjustAll = adjustAll; map = map; fold = fold; fold1 = fold1; 
   filter = filter; partition = partition; elements = elements;
   instanceName m = moduleName}

instance Ord k  => A.Assoc (FM k) [k] where
  {toSeq = toSeq; keys = keys; mapWithKey = mapWithKey; 
   foldWithKey = foldWithKey; filterWithKey = filterWithKey; 
   partitionWithKey = partitionWithKey}

instance Ord k => A.FiniteMapX (FM k) [k] where
  {fromSeqWith = fromSeqWith; fromSeqWithKey = fromSeqWithKey; 
   insertWith  = insertWith; insertWithKey = insertWithKey; 
   insertSeqWith = insertSeqWith; insertSeqWithKey = insertSeqWithKey; 
   unionl = unionl; unionr = unionr; unionWith = unionWith; 
   unionSeqWith = unionSeqWith; intersectWith = intersectWith; 
   difference = difference; subset = subset; subsetEq = subsetEq}

instance Ord k => A.FiniteMap (FM k) [k] where
  {unionWithKey = unionWithKey; unionSeqWithKey = unionSeqWithKey; 
   intersectWithKey = intersectWithKey}

instance Ord k => Functor (FM k) where
  fmap =  map

--
-- Test code follows
--

keyInvariantFMB :: (Show k, Ord k) => (k -> Bool) -> FMB k a -> Bool
keyInvariantFMB p E = True
keyInvariantFMB p (I _ k _ l _ r)
  =    p k
    && keyInvariantFMB p l
    && keyInvariantFMB p r

actualSizeFMB :: FMB k a -> Int
actualSizeFMB E = 0
actualSizeFMB (I _ _ _ l _ r) = 1 + actualSizeFMB l + actualSizeFMB r

structuralInvariantFMB :: (Show k, Ord k) => FMB k a -> Bool
structuralInvariantFMB E = True
structuralInvariantFMB fmb@(I size k _ l (FMB' m) r)
  =    structuralInvariantFMB l
    && structuralInvariantFMB m
    && structuralInvariantFMB r
    && keyInvariantFMB (<k) l
    && keyInvariantFMB (>k) r
    && actualSizeFMB fmb == size
    && (sizel + sizer < 2
        || (sizel <= balance * sizer && sizer <= balance * sizel))
  where
      sizel = sizeFMB l
      sizer = sizeFMB r

structuralInvariantFM :: (Show k, Ord k) => FM k a -> Bool
structuralInvariantFM (FM k fmb) = structuralInvariantFMB fmb

