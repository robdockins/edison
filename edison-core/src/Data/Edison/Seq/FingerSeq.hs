-- |
--   Module      :  Data.Edison.Seq.FingerSeq
--   Copyright   :  Copyright (c) 2006 Robert Dockins
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)


module Data.Edison.Seq.FingerSeq (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence Operations
    empty,singleton,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,map,concatMap,
    fold,fold',fold1,fold1',foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,
    strict, strictWith,

    -- * Unit testing
    structuralInvariant,

    -- * Documentation
    moduleName
) where

import qualified Prelude
import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S
import Data.Edison.Seq.Defaults
import Control.Monad
import Control.Monad.Identity
import Data.Monoid
import Test.QuickCheck

#ifdef __GLASGOW_HASKELL__
import GHC.Base (unsafeCoerce#)
#endif


import qualified Data.Edison.Concrete.FingerTree as FT

moduleName     :: String
moduleName = "Data.Edison.Seq.FingerSeq"


newtype SizeM = SizeM Int deriving (Eq,Ord,Num,Enum,Show)
unSizeM (SizeM x) = x

instance Monoid SizeM where
   mempty  = 0
   mappend = (+)


newtype Elem a = Elem a
unElem (Elem x) = x

instance Measured SizeM (Elem a) where
   measure _ = 1

newtype Seq a = Seq (FT.FingerTree SizeM (Elem a))
unSeq (Seq ft) = ft



empty          :: Seq a
singleton      :: a -> Seq a
lcons          :: a -> Seq a -> Seq a
rcons          :: a -> Seq a -> Seq a
append         :: Seq a -> Seq a -> Seq a
lview          :: (Monad m) => Seq a -> m (a, Seq a)
lhead          :: Seq a -> a
lheadM         :: (Monad m) => Seq a -> m a
ltail          :: Seq a -> Seq a
ltailM         :: (Monad m) => Seq a -> m (Seq a)
rview          :: (Monad m) => Seq a -> m (a, Seq a)
rhead          :: Seq a -> a
rheadM         :: (Monad m) => Seq a -> m a
rtail          :: Seq a -> Seq a
rtailM         :: (Monad m) => Seq a -> m (Seq a)
null           :: Seq a -> Bool
size           :: Seq a -> Int
concat         :: Seq (Seq a) -> Seq a
reverse        :: Seq a -> Seq a
reverseOnto    :: Seq a -> Seq a -> Seq a
fromList       :: [a] -> Seq a
toList         :: Seq a -> [a]
map            :: (a -> b) -> Seq a -> Seq b
concatMap      :: (a -> Seq b) -> Seq a -> Seq b
fold           :: (a -> b -> b) -> b -> Seq a -> b
fold'          :: (a -> b -> b) -> b -> Seq a -> b
fold1          :: (a -> a -> a) -> Seq a -> a
fold1'         :: (a -> a -> a) -> Seq a -> a
foldr          :: (a -> b -> b) -> b -> Seq a -> b
foldl          :: (b -> a -> b) -> b -> Seq a -> b
foldr1         :: (a -> a -> a) -> Seq a -> a
foldl1         :: (a -> a -> a) -> Seq a -> a
reducer        :: (a -> a -> a) -> a -> Seq a -> a
reducel        :: (a -> a -> a) -> a -> Seq a -> a
reduce1        :: (a -> a -> a) -> Seq a -> a
foldr'         :: (a -> b -> b) -> b -> Seq a -> b
foldl'         :: (b -> a -> b) -> b -> Seq a -> b
foldr1'        :: (a -> a -> a) -> Seq a -> a
foldl1'        :: (a -> a -> a) -> Seq a -> a
reducer'       :: (a -> a -> a) -> a -> Seq a -> a
reducel'       :: (a -> a -> a) -> a -> Seq a -> a
reduce1'       :: (a -> a -> a) -> Seq a -> a
copy           :: Int -> a -> Seq a
inBounds       :: Int -> Seq a -> Bool
lookup         :: Int -> Seq a -> a
lookupM        :: (Monad m) => Int -> Seq a -> m a
lookupWithDefault :: a -> Int -> Seq a -> a
update         :: Int -> a -> Seq a -> Seq a
adjust         :: (a -> a) -> Int -> Seq a -> Seq a
mapWithIndex   :: (Int -> a -> b) -> Seq a -> Seq b
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
foldrWithIndex' :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldlWithIndex' :: (b -> Int -> a -> b) -> b -> Seq a -> b
take           :: Int -> Seq a -> Seq a
drop           :: Int -> Seq a -> Seq a
splitAt        :: Int -> Seq a -> (Seq a, Seq a)
subseq         :: Int -> Int -> Seq a -> Seq a
filter         :: (a -> Bool) -> Seq a -> Seq a
partition      :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
takeWhile      :: (a -> Bool) -> Seq a -> Seq a
dropWhile      :: (a -> Bool) -> Seq a -> Seq a
splitWhile     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
zip            :: Seq a -> Seq b -> Seq (a,b)
zip3           :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zipWith        :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith3       :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
unzip          :: Seq (a,b) -> (Seq a, Seq b)
unzip3         :: Seq (a,b,c) -> (Seq a, Seq b, Seq c)
unzipWith      :: (a -> b) -> (a -> c) -> Seq a -> (Seq b, Seq c)
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> Seq a -> (Seq b, Seq c, Seq d)
strict         :: Seq a -> Seq a
strictWith     :: (a -> b) -> Seq a -> Seq a
structuralInvariant :: Seq a -> Bool

#ifdef __GLASGOW_HASKELL__

mapElem   = unsafeCoerce#
mapUnElem = unsafeCoerce#

#else

mapElem   = Prelude.map Elem
mapUnElem = Prelude.map unElem

#endif

null         = FT.null . unSeq
empty        = Seq FT.empty
singleton    = Seq . FT.singleton . Elem
lcons x      = Seq . FT.lcons (Elem x) . unSeq
rcons x      = Seq . FT.rcons (Elem x) . unSeq
append p q   = Seq $ FT.append (unSeq p) (unSeq q)
fromList     = Seq . FT.fromList . mapElem
toList       = mapUnElem . FT.toList . unSeq
reverse      = Seq . FT.reverse . unSeq
size         = unSizeM . measure . unSeq
strict       = Seq . FT.strict . unSeq
strictWith f = Seq . FT.strictWith (f . unElem) . unSeq
structuralInvariant = FT.structuralInvariant . unSeq

#ifdef __GLASGOW_HASKELL__

lview (Seq xs) =
  let f = unsafeCoerce# :: Monad m => m (Elem a,FT.FingerTree SizeM (Elem a)) -> m (a,Seq a)
  in  f (FT.lview xs)

rview (Seq xs) =
  let f = unsafeCoerce# :: Monad m => m (Elem a,FT.FingerTree SizeM (Elem a)) -> m (a,Seq a)
  in  f (FT.rview xs)

#else

lview (Seq xs) = FT.lview xs >>= \(Elem a, zs) -> return (a, Seq zs)
rview (Seq xs) = FT.rview xs >>= \(Elem a, zs) -> return (a, Seq zs)

#endif


lheadM xs = lview xs >>= return . fst
ltailM xs = lview xs >>= return . snd
rheadM xs = rview xs >>= return . fst
rtailM xs = rview xs >>= return . snd
lhead = runIdentity . lheadM
ltail = runIdentity . ltailM
rhead = runIdentity . rheadM
rtail = runIdentity . rtailM

fold     = foldr
fold'    = foldr'
fold1    = foldr1
fold1'   = foldr1'

#ifdef __GLASGOW_HASKELL__

foldr  f z (Seq xs) = unElem $ FT.foldFT id (.) (unsafeCoerce# f) xs (Elem z)
foldr' f z (Seq xs) = unElem $ FT.foldFT id (.) (unsafeCoerce# f) xs (Elem z)

reduce1  f (Seq xs) = unElem $ FT.reduce1  (unsafeCoerce# f) xs
reduce1' f (Seq xs) = unElem $ FT.reduce1' (unsafeCoerce# f) xs

map f (Seq xs) = Seq $ FT.mapTree (unsafeCoerce# f) xs

#else

foldr  f z (Seq xs) = unElem $ FT.foldFT id (.) ( \(Elem x) (Elem y) -> Elem $ f x y) xs (Elem z)
foldr' f z (Seq xs) = unElem $ FT.foldFT id (.) ( \(Elem x) (Elem y) -> Elem $ f x y) xs (Elem z)

reduce1  f (Seq xs) = unElem $ FT.reduce1  ( \(Elem x) (Elem y) -> Elem $ f x y) xs
reduce1' f (Seq xs) = unElem $ FT.reduce1' ( \(Elem x) (Elem y) -> Elem $ f x y) xs

map f (Seq xs) = Seq $ FT.mapTree ( \(Elem x) -> Elem $ f x) xs

#endif

lookupM i (Seq xs)
    | inBounds i (Seq xs) =
	case FT.splitTree (> (SizeM i)) (SizeM 0) xs of
           FT.Split _ (Elem x) _ -> return x

    | otherwise = fail "FingerSeq.lookupM: index out of bounds"

lookupWithDefault d i (Seq xs)
    | inBounds i (Seq xs) =
	case FT.splitTree (> (SizeM i)) (SizeM 0) xs of
           FT.Split _ (Elem x) _ -> x

    | otherwise = d

update i x (Seq xs)
    | inBounds i (Seq xs) =
	case FT.splitTree (> (SizeM i)) (SizeM 0) xs of
           FT.Split l _ r -> Seq $ FT.append l $ FT.lcons (Elem x) $ r

    | otherwise = Seq xs

adjust f i (Seq xs)
    | inBounds i (Seq xs) =
	case FT.splitTree (> (SizeM i)) (SizeM 0) xs of
           FT.Split l x r -> Seq $ FT.append l $ FT.lcons (Elem (f (unElem x))) $ r

    | otherwise = Seq xs

take i (Seq xs) = Seq $ FT.takeUntil (> (SizeM i)) xs
drop i (Seq xs) = Seq $ FT.dropUntil (> (SizeM i)) xs
splitAt i (Seq xs) = let (a,b) = FT.split (> (SizeM i)) xs in (Seq a, Seq b)


inBounds = inBoundsUsingSize
lookup   = lookupUsingLookupM

foldr1 f xs =
   case rview xs of
      Nothing      -> error "FingerSeq.foldr1: empty sequence"
      Just (x,xs') -> foldr f x xs'

foldr1' f xs =
   case rview xs of
      Nothing      -> error "FingerSeq.foldr1': empty sequence"
      Just (x,xs') -> foldr' f x xs'

foldl    = foldlUsingLists
foldl'   = foldl'UsingLists
foldl1   = foldl1UsingLists
foldl1'  = foldl1'UsingLists

reducer  = reducerUsingReduce1
reducer' = reducer'UsingReduce1'
reducel  = reducelUsingReduce1
reducel' = reducel'UsingReduce1'

copy        = copyUsingLists
concat      = concatUsingFoldr
reverseOnto = reverseOntoUsingReverse
concatMap   = concatMapUsingFoldr
subseq      = subseqDefault
filter      = filterUsingLview
partition   = partitionUsingFoldr
takeWhile   = takeWhileUsingLview
dropWhile   = dropWhileUsingLview
splitWhile  = splitWhileUsingLview

mapWithIndex    = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists

zip = zipUsingLview
zip3 = zip3UsingLview
zipWith = zipWithUsingLview
zipWith3 = zipWith3UsingLview

unzip = unzipUsingFoldr
unzip3 = unzip3UsingFoldr
unzipWith = unzipWithUsingFoldr
unzipWith3 = unzipWith3UsingFoldr

-- instances

instance S.Sequence Seq where
  {lcons = lcons; rcons = rcons;
   lview = lview; lhead = lhead; ltail = ltail;
   lheadM = lheadM; ltailM = ltailM; rheadM = rheadM; rtailM = rtailM;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   foldr = foldr; foldr' = foldr'; foldl = foldl; foldl' = foldl';
   foldr1 = foldr1; foldr1' = foldr1'; foldl1 = foldl1; foldl1' = foldl1';
   reducer = reducer; reducer' = reducer'; reducel = reducel;
   reducel' = reducel'; reduce1 = reduce1;  reduce1' = reduce1';
   copy = copy; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust; mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldlWithIndex = foldlWithIndex;
   foldrWithIndex' = foldrWithIndex'; foldlWithIndex' = foldlWithIndex';
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   strict = strict; strictWith = strictWith;
   structuralInvariant = structuralInvariant; instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = singleton
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  xs == ys = toList xs == toList ys

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Elem a) where
   arbitrary   = arbitrary >>= return . Elem
   coarbitrary = coarbitrary . unElem

instance Arbitrary a => Arbitrary (Seq a) where
   arbitrary   = arbitrary >>= return . Seq
   coarbitrary = coarbitrary . unSeq

instance Monoid (Seq a) where
  mempty  = empty
  mappend = append
