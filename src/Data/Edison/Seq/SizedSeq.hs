-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Seq.SizedSeq (
    -- generic adaptor for sequences to keep track of the current size
    Sized, -- Sized s instance of Sequence, Functor, Monad, MonadPlus

    -- sequence operations
    empty,single,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldl,foldr1,foldl1,reducer,reducel,reduce1,
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- documentation
    moduleName,instanceName,

    -- other supported operations
    fromSeq,toSeq
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Data.Edison.Prelude
import qualified Data.Edison.Seq.Sequence as S
import Data.Edison.Seq.SequenceDefaults -- only used by concatMap
import Control.Monad
import Test.QuickCheck

-- This module defines a sequence adaptor Sized s.
-- If s is a sequence type constructor, then Sized s
-- is a sequence type constructor that is identical to s,
-- except that it also keeps track of the current size of
-- each sequence.

-- signatures for exported functions
moduleName     :: String
instanceName   :: S.Sequence s => Sized s a -> String
empty          :: S.Sequence s => Sized s a
single         :: S.Sequence s => a -> Sized s a
lcons          :: S.Sequence s => a -> Sized s a -> Sized s a
rcons          :: S.Sequence s => Sized s a -> a -> Sized s a
append         :: S.Sequence s => Sized s a -> Sized s a -> Sized s a
lview          :: (S.Sequence s, Monad m) => Sized s a -> m (a, Sized s a)
lhead          :: S.Sequence s => Sized s a -> a
ltail          :: S.Sequence s => Sized s a -> Sized s a
rview          :: (S.Sequence s, Monad m) => Sized s a -> m (Sized s a, a)
rhead          :: S.Sequence s => Sized s a -> a
rtail          :: S.Sequence s => Sized s a -> Sized s a
null           :: S.Sequence s => Sized s a -> Bool
size           :: S.Sequence s => Sized s a -> Int
concat         :: S.Sequence s => Sized s (Sized s a) -> Sized s a
reverse        :: S.Sequence s => Sized s a -> Sized s a
reverseOnto    :: S.Sequence s => Sized s a -> Sized s a -> Sized s a
fromList       :: S.Sequence s => [a] -> Sized s a
toList         :: S.Sequence s => Sized s a -> [a]
map            :: S.Sequence s => (a -> b) -> Sized s a -> Sized s b
concatMap      :: S.Sequence s => (a -> Sized s b) -> Sized s a -> Sized s b
foldr          :: S.Sequence s => (a -> b -> b) -> b -> Sized s a -> b
foldl          :: S.Sequence s => (b -> a -> b) -> b -> Sized s a -> b
foldr1         :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
foldl1         :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
reducer        :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reducel        :: S.Sequence s => (a -> a -> a) -> a -> Sized s a -> a
reduce1        :: S.Sequence s => (a -> a -> a) -> Sized s a -> a
copy           :: S.Sequence s => Int -> a -> Sized s a
inBounds       :: S.Sequence s => Sized s a -> Int -> Bool
lookup         :: S.Sequence s => Sized s a -> Int -> a
lookupM        :: (S.Sequence s, Monad m) => Sized s a -> Int -> m a
lookupWithDefault :: S.Sequence s => a -> Sized s a -> Int -> a
update         :: S.Sequence s => Int -> a -> Sized s a -> Sized s a
adjust         :: S.Sequence s => (a -> a) -> Int -> Sized s a -> Sized s a
mapWithIndex   :: S.Sequence s => (Int -> a -> b) -> Sized s a -> Sized s b
foldrWithIndex :: S.Sequence s => (Int -> a -> b -> b) -> b -> Sized s a -> b
foldlWithIndex :: S.Sequence s => (b -> Int -> a -> b) -> b -> Sized s a -> b
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

-- bonus functions, not in Sequence signature
fromSeq        :: S.Sequence s => s a -> Sized s a
toSeq          :: S.Sequence s => Sized s a -> s a



moduleName = "SizedSeq"
instanceName (N n s) = "SizedSeq(" ++ S.instanceName s ++ ")"

data Sized s a = N !Int (s a)

fromSeq xs = N (S.size xs) xs
toSeq (N n xs) = xs

empty = N 0 S.empty
single x = N 1 (S.single x)
lcons x (N n xs) = N (n+1) (S.lcons x xs)
rcons (N n xs) x = N (n+1) (S.rcons xs x)
append (N m xs) (N n ys) = N (m+n) (S.append xs ys)

lview (N n xs) = case S.lview xs of
                   Nothing     -> fail "SizedSeq.lview: empty sequence"
                   Just (x,xs) -> return (x, N (n-1) xs)

lhead (N n xs) = S.lhead xs

ltail (N 0 xs) = error "SizedSeq.ltail: empty sequence"
ltail (N n xs) = N (n-1) (S.ltail xs)

rview (N n xs) = case S.rview xs of
                   Nothing     -> fail "SizedSeq.rview: empty sequence"
                   Just (xs,x) -> return (N (n-1) xs, x)
 
rhead (N n xs) = S.rhead xs

rtail (N 0 xs) = error "SizedSeq.rtail: empty sequence"
rtail (N n xs) = N (n-1) (S.rtail xs)

null (N n xs) = n == 0
size (N n xs) = n
concat (N n xss) = fromSeq (S.concat (S.map toSeq xss))
reverse (N n xs) = N n (S.reverse xs)
reverseOnto (N m xs) (N n ys) = N (m+n) (S.reverseOnto xs ys)
fromList = fromSeq . S.fromList
toList (N n xs) = S.toList xs
map f (N n xs) = N n (S.map f xs)

concatMap = concatMapUsingFoldr -- only function that uses a default

foldr f e (N n xs) = S.foldr f e xs
foldl f e (N n xs) = S.foldl f e xs
foldr1 f (N n xs) = S.foldr1 f xs
foldl1 f (N n xs) = S.foldl1 f xs
reducer f e (N n xs) = S.reducer f e xs
reducel f e (N n xs) = S.reducel f e xs
reduce1 f (N n xs) = S.reduce1 f xs

copy n x 
    | n <= 0 = empty
    | otherwise = N n (S.copy n x)

inBounds (N n xs) i = (i >= 0) && (i < n)
lookup (N n xs) = S.lookup xs
lookupM (N n xs) = S.lookupM xs
lookupWithDefault d (N n xs) = S.lookupWithDefault d xs
update i x (N n xs) = N n (S.update i x xs)
adjust f i (N n xs) = N n (S.adjust f i xs)
mapWithIndex f (N n xs) = N n (S.mapWithIndex f xs)
foldrWithIndex f e (N n xs) = S.foldrWithIndex f e xs
foldlWithIndex f e (N n xs) = S.foldlWithIndex f e xs

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

-- instances

instance S.Sequence s => S.Sequence (Sized s) where
  {empty = empty; single = single; lcons = lcons; rcons = rcons;
   append = append; lview = lview; lhead = lhead; ltail = ltail;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   map = map; concatMap = concatMap; foldr = foldr; foldl = foldl;
   foldr1 = foldr1; foldl1 = foldl1; reducer = reducer; 
   reducel = reducel; reduce1 = reduce1; copy = copy; 
   inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust; mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldlWithIndex = foldlWithIndex;
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   instanceName = instanceName}

instance S.Sequence s => Functor (Sized s) where
  fmap = map

instance S.Sequence s => Monad (Sized s) where
  return = single
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

