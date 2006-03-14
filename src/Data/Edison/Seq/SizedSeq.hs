-- |
--   Module      :  Data.Edison.Seq.SizedSeq
--   Copyright   :  Copyright (c) 1998-1999 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   This module defines a sequence adaptor @Sized s@.
--   If @s@ is a sequence type constructor, then @Sized s@
--   is a sequence type constructor that is identical to @s@,
--   except that it also keeps track of the current size of
--   each sequence.

module Data.Edison.Seq.SizedSeq (
    -- * Sized Sequence Type
    Sized, -- Sized s instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence Operations
    empty,singleton,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,map,concatMap,
    fold,fold',fold1,fold1',foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,foldrWithIndex',foldlWithIndex',
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- * Unit testing
    structuralInvariant,

    -- * Documentation
    moduleName,instanceName,

    -- * Other supported operations
    fromSeq,toSeq
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S
import Data.Edison.Seq.Defaults -- only used by concatMap
import Control.Monad
import Test.QuickCheck


-- signatures for exported functions
moduleName     :: String
instanceName   :: S.Sequence s => Sized s a -> String
empty          :: S.Sequence s => Sized s a
singleton      :: S.Sequence s => a -> Sized s a
lcons          :: S.Sequence s => a -> Sized s a -> Sized s a
rcons          :: S.Sequence s => a -> Sized s a -> Sized s a
append         :: S.Sequence s => Sized s a -> Sized s a -> Sized s a
lview          :: (S.Sequence s, Monad m) => Sized s a -> m (a, Sized s a)
lhead          :: S.Sequence s => Sized s a -> a
lheadM         :: (S.Sequence s, Monad m) => Sized s a -> m a
ltail          :: S.Sequence s => Sized s a -> Sized s a
ltailM         :: (S.Sequence s, Monad m) => Sized s a -> m (Sized s a)
rview          :: (S.Sequence s, Monad m) => Sized s a -> m (a, Sized s a)
rhead          :: S.Sequence s => Sized s a -> a
rheadM         :: (S.Sequence s, Monad m) => Sized s a -> m a
rtail          :: S.Sequence s => Sized s a -> Sized s a
rtailM         :: (S.Sequence s, Monad m) => Sized s a -> m (Sized s a)
null           :: S.Sequence s => Sized s a -> Bool
size           :: S.Sequence s => Sized s a -> Int
concat         :: S.Sequence s => Sized s (Sized s a) -> Sized s a
reverse        :: S.Sequence s => Sized s a -> Sized s a
reverseOnto    :: S.Sequence s => Sized s a -> Sized s a -> Sized s a
fromList       :: S.Sequence s => [a] -> Sized s a
toList         :: S.Sequence s => Sized s a -> [a]
map            :: S.Sequence s => (a -> b) -> Sized s a -> Sized s b
concatMap      :: S.Sequence s => (a -> Sized s b) -> Sized s a -> Sized s b
fold           :: S.Sequence s => (a -> b -> b) -> b -> Sized s a -> b
fold'          :: S.Sequence s => (a -> b -> b) -> b -> Sized s a -> b
fold1          :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
fold1'         :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
foldr          :: S.Sequence s => (a -> b -> b) -> b -> Sized s a -> b
foldl          :: S.Sequence s => (b -> a -> b) -> b -> Sized s a -> b
foldr1         :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
foldl1         :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
reducer        :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reducel        :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reduce1        :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
foldr'         :: S.Sequence s => (a -> b -> b) -> b -> Sized s a -> b
foldl'         :: S.Sequence s => (b -> a -> b) -> b -> Sized s a -> b
foldr1'        :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
foldl1'        :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
reducer'       :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reducel'       :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reduce1'       :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
copy           :: S.Sequence s => Int -> a -> Sized s a
inBounds       :: S.Sequence s => Int -> Sized s a -> Bool
lookup         :: S.Sequence s => Int -> Sized s a -> a
lookupM        :: (S.Sequence s, Monad m) => Int -> Sized s a -> m a
lookupWithDefault :: S.Sequence s => a -> Int -> Sized s a -> a
update         :: S.Sequence s => Int -> a -> Sized s a -> Sized s a
adjust         :: S.Sequence s => (a -> a) -> Int -> Sized s a -> Sized s a
mapWithIndex   :: S.Sequence s => (Int -> a -> b) -> Sized s a -> Sized s b
foldrWithIndex :: S.Sequence s => (Int -> a -> b -> b) -> b -> Sized s a -> b
foldlWithIndex :: S.Sequence s => (b -> Int -> a -> b) -> b -> Sized s a -> b
foldrWithIndex' :: S.Sequence s => (Int -> a -> b -> b) -> b -> Sized s a -> b
foldlWithIndex' :: S.Sequence s => (b -> Int -> a -> b) -> b -> Sized s a -> b
take           :: S.Sequence s => Int -> Sized s a -> Sized s a
drop           :: S.Sequence s => Int -> Sized s a -> Sized s a
splitAt        :: S.Sequence s => Int -> Sized s a -> (Sized s a, Sized s a)
subseq         :: S.Sequence s => Int -> Int -> Sized s a -> Sized s a
filter         :: S.Sequence s => (a -> Bool) -> Sized s a -> Sized s a
partition      :: S.Sequence s => (a -> Bool) -> Sized s a -> (Sized s a, Sized s a)
takeWhile      :: S.Sequence s => (a -> Bool) -> Sized s a -> Sized s a
dropWhile      :: S.Sequence s => (a -> Bool) -> Sized s a -> Sized s a
splitWhile     :: S.Sequence s => (a -> Bool) -> Sized s a -> (Sized s a, Sized s a)
zip            :: S.Sequence s => Sized s a -> Sized s b -> Sized s (a,b)
zip3           :: S.Sequence s => Sized s a -> Sized s b -> Sized s c -> Sized s (a,b,c)
zipWith        :: S.Sequence s => (a -> b -> c) -> Sized s a -> Sized s b -> Sized s c
zipWith3       :: S.Sequence s => (a -> b -> c -> d) -> Sized s a -> Sized s b -> Sized s c -> Sized s d
unzip          :: S.Sequence s => Sized s (a,b) -> (Sized s a, Sized s b)
unzip3         :: S.Sequence s => Sized s (a,b,c) -> (Sized s a, Sized s b, Sized s c)
unzipWith      :: S.Sequence s => (a -> b) -> (a -> c) -> Sized s a -> (Sized s b, Sized s c)
unzipWith3     :: S.Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> Sized s a -> (Sized s b, Sized s c, Sized s d)
structuralInvariant :: S.Sequence s => Sized s a -> Bool

-- bonus functions, not in Sequence signature
fromSeq        :: S.Sequence s => s a -> Sized s a
toSeq          :: S.Sequence s => Sized s a -> s a



moduleName = "Data.Edison.Seq.SizedSeq"
instanceName (N n s) = "SizedSeq(" ++ S.instanceName s ++ ")"

data Sized s a = N !Int (s a)

fromSeq xs = N (S.size xs) xs
toSeq (N n xs) = xs

empty = N 0 S.empty
singleton x = N 1 (S.singleton x)
lcons x (N n xs) = N (n+1) (S.lcons x xs)
rcons x (N n xs) = N (n+1) (S.rcons x xs)
append (N m xs) (N n ys) = N (m+n) (S.append xs ys)

lview (N n xs) = case S.lview xs of
                   Nothing     -> fail "SizedSeq.lview: empty sequence"
                   Just (x,xs) -> return (x, N (n-1) xs)

lhead (N n xs) = S.lhead xs

lheadM (N n xs) = S.lheadM xs

ltail (N 0 xs) = error "SizedSeq.ltail: empty sequence"
ltail (N n xs) = N (n-1) (S.ltail xs)

ltailM (N 0 xs) = fail "SizedSeq.ltailM: empty sequence"
ltailM (N n xs) = return (N (n-1) (S.ltail xs))

rview (N n xs) = case S.rview xs of
                   Nothing     -> fail "SizedSeq.rview: empty sequence"
                   Just (x,xs) -> return (x, N (n-1) xs)
 
rhead (N n xs) = S.rhead xs

rheadM (N n xs) = S.rheadM xs

rtail (N 0 xs) = error "SizedSeq.rtail: empty sequence"
rtail (N n xs) = N (n-1) (S.rtail xs)

rtailM (N 0 xs) = fail "SizedSeq.rtailM: empty sequence"
rtailM (N n xs) = return (N (n-1) (S.rtail xs))

null (N n xs) = n == 0
size (N n xs) = n
concat (N n xss) = fromSeq (S.concat (S.map toSeq xss))
reverse (N n xs) = N n (S.reverse xs)
reverseOnto (N m xs) (N n ys) = N (m+n) (S.reverseOnto xs ys)
fromList = fromSeq . S.fromList
toList (N n xs) = S.toList xs
map f (N n xs) = N n (S.map f xs)

concatMap = concatMapUsingFoldr -- only function that uses a default

fold  f e (N n xs) = S.fold f e xs
fold' f e (N n xs) = S.fold' f e xs
fold1 f  (N n xs) = S.fold1 f xs
fold1' f (N n xs) = S.fold1' f xs
foldr  f e (N n xs) = S.foldr f e xs
foldr' f e (N n xs) = S.foldr' f e xs
foldl  f e (N n xs) = S.foldl f e xs
foldl' f e (N n xs) = S.foldl' f e xs
foldr1  f (N n xs) = S.foldr1 f xs
foldr1' f (N n xs) = S.foldr1' f xs
foldl1  f (N n xs) = S.foldl1 f xs
foldl1' f (N n xs) = S.foldl1' f xs
reducer  f e (N n xs) = S.reducer f e xs
reducer' f e (N n xs) = S.reducer' f e xs
reducel  f e (N n xs) = S.reducel f e xs
reducel' f e (N n xs) = S.reducel' f e xs
reduce1  f (N n xs) = S.reduce1 f xs
reduce1' f (N n xs) = S.reduce1' f xs

copy n x 
    | n <= 0 = empty
    | otherwise = N n (S.copy n x)

inBounds i (N n xs) = (i >= 0) && (i < n)
lookup i (N n xs) = S.lookup i xs
lookupM i (N n xs) = S.lookupM i xs
lookupWithDefault d i (N n xs) = S.lookupWithDefault d i xs
update i x (N n xs) = N n (S.update i x xs)
adjust f i (N n xs) = N n (S.adjust f i xs)
mapWithIndex f (N n xs) = N n (S.mapWithIndex f xs)
foldrWithIndex  f e (N n xs) = S.foldrWithIndex f e xs
foldrWithIndex' f e (N n xs) = S.foldrWithIndex' f e xs
foldlWithIndex  f e (N n xs) = S.foldlWithIndex f e xs
foldlWithIndex' f e (N n xs) = S.foldlWithIndex' f e xs

take i original@(N n xs)
  | i <= 0 = empty
  | i >= n = original
  | otherwise = N i (S.take i xs)

drop i original@(N n xs)
  | i <= 0 = original
  | i >= n = empty
  | otherwise = N (n-i) (S.drop i xs)

splitAt i original@(N n xs)
  | i <= 0 = (empty, original)
  | i >= n = (original, empty)
  | otherwise = let (ys,zs) = S.splitAt i xs
                in (N i ys, N (n-i) zs)

subseq i len original@(N n xs)
  | i <= 0 = take len original
  | i >= n || len <= 0 = empty
  | i+len >= n = N (n-i) (S.drop i xs)
  | otherwise = N len (S.subseq i len xs)

filter p = fromSeq . S.filter p . toSeq

partition p (N n xs) = (N m ys, N (n-m) zs)
  where (ys,zs) = S.partition p xs
        m = S.size ys

takeWhile p = fromSeq . S.takeWhile p . toSeq
dropWhile p = fromSeq . S.dropWhile p . toSeq

splitWhile p (N n xs) = (N m ys, N (n-m) zs)
  where (ys,zs) = S.splitWhile p xs
        m = S.size ys

zip (N m xs) (N n ys) = N (min m n) (S.zip xs ys)
zip3 (N l xs) (N m ys) (N n zs) = N (min l (min m n)) (S.zip3 xs ys zs)

zipWith f (N m xs) (N n ys) = N (min m n) (S.zipWith f xs ys)
zipWith3 f (N l xs) (N m ys) (N n zs) = N (min l (min m n)) (S.zipWith3 f xs ys zs)

unzip (N n xys) = (N n xs, N n ys)
  where (xs,ys) = S.unzip xys

unzip3 (N n xyzs) = (N n xs, N n ys, N n zs)
  where (xs,ys,zs) = S.unzip3 xyzs

unzipWith f g (N n xys) = (N n xs, N n ys)
  where (xs,ys) = S.unzipWith f g xys

unzipWith3 f g h (N n xyzs) = (N n xs, N n ys, N n zs)
  where (xs,ys,zs) = S.unzipWith3 f g h xyzs

structuralInvariant (N i s) = i == S.size s

-- instances

instance S.Sequence s => S.Sequence (Sized s) where
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
   reducel' = reducel'; reduce1 = reduce1; reduce1' = reduce1';
   copy = copy; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust; mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldrWithIndex' = foldrWithIndex';
   foldlWithIndex = foldlWithIndex; foldlWithIndex' = foldlWithIndex';
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   structuralInvariant = structuralInvariant; instanceName = instanceName}

instance S.Sequence s => Functor (Sized s) where
  fmap = map

instance S.Sequence s => Monad (Sized s) where
  return = singleton
  xs >>= k = concatMap k xs

instance S.Sequence s => MonadPlus (Sized s) where
  mplus = append
  mzero = empty


instance Eq (s a) => Eq (Sized s a) where
  (N m xs) == (N n ys) = (m == n) && (xs == ys)
  -- this is probably identical to the code that would be
  -- generated by "deriving (Eq)", but I wanted to be *sure*
  -- that the sizes were compared before the inner sequences

instance (S.Sequence s, Show (s a)) => Show (Sized s a) where
  show xs = show (toSeq xs)

instance (S.Sequence s, Arbitrary (s a)) => Arbitrary (Sized s a) where
  arbitrary = do xs <- arbitrary
                 return (fromSeq xs)

  coarbitrary xs = coarbitrary (toSeq xs)
