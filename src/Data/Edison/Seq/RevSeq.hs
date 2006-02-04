-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Seq.RevSeq (
    -- generic adaptor for sequences to keep them in the opposite order
    Rev, -- Rev s instance of Sequence, Functor, Monad, MonadPlus

    -- sequence operations
    empty,single,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldl,foldr1,foldl1,reducer,reducel,reduce1,
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- documentation
    moduleName,instanceName
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Data.Edison.EdisonPrelude
import qualified Data.Edison.Seq.Sequence as S ( Sequence(..) )
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Seq.SequenceDefaults -- only used by concatMap
import Control.Monad

-- This module defines a sequence adaptor Rev s.
-- If s is a sequence type constructor, then Rev s
-- is a sequence type constructor that is identical to s,
-- except that it is kept in the opposite order.
-- Also keeps explicit track of the size of the sequence,
-- similar to the Sized adaptor in SizedSeq.hs.
--
-- This module is most useful when s is a sequence type
-- that offers fast access to the front but slow access
-- to the rear, and your application needs the opposite
-- (i.e., fast access to the rear but slow access to the
-- front).

-- signatures for exported functions
moduleName     :: String
instanceName   :: S.Sequence s => Rev s a -> String
empty          :: S.Sequence s => Rev s a
single         :: S.Sequence s => a -> Rev s a
lcons          :: S.Sequence s => a -> Rev s a -> Rev s a
rcons          :: S.Sequence s => Rev s a -> a -> Rev s a
append         :: S.Sequence s => Rev s a -> Rev s a -> Rev s a
lview          :: (S.Sequence s, Monad m) => Rev s a -> m (a, Rev s a)
lhead          :: S.Sequence s => Rev s a -> a
ltail          :: S.Sequence s => Rev s a -> Rev s a
rview          :: (S.Sequence s, Monad m) => Rev s a -> m (Rev s a, a)
rhead          :: S.Sequence s => Rev s a -> a
rtail          :: S.Sequence s => Rev s a -> Rev s a
null           :: S.Sequence s => Rev s a -> Bool
size           :: S.Sequence s => Rev s a -> Int
concat         :: S.Sequence s => Rev s (Rev s a) -> Rev s a
reverse        :: S.Sequence s => Rev s a -> Rev s a
reverseOnto    :: S.Sequence s => Rev s a -> Rev s a -> Rev s a
fromList       :: S.Sequence s => [a] -> Rev s a
toList         :: S.Sequence s => Rev s a -> [a]
map            :: S.Sequence s => (a -> b) -> Rev s a -> Rev s b
concatMap      :: S.Sequence s => (a -> Rev s b) -> Rev s a -> Rev s b
foldr          :: S.Sequence s => (a -> b -> b) -> b -> Rev s a -> b
foldl          :: S.Sequence s => (b -> a -> b) -> b -> Rev s a -> b
foldr1         :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
foldl1         :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
reducer        :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reducel        :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reduce1        :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
copy           :: S.Sequence s => Int -> a -> Rev s a
inBounds       :: S.Sequence s => Rev s a -> Int -> Bool
lookup         :: S.Sequence s => Rev s a -> Int -> a
lookupM        :: (S.Sequence s, Monad m) => Rev s a -> Int -> m a
lookupWithDefault :: S.Sequence s => a -> Rev s a -> Int -> a
update         :: S.Sequence s => Int -> a -> Rev s a -> Rev s a
adjust         :: S.Sequence s => (a -> a) -> Int -> Rev s a -> Rev s a
mapWithIndex   :: S.Sequence s => (Int -> a -> b) -> Rev s a -> Rev s b
foldrWithIndex :: S.Sequence s => (Int -> a -> b -> b) -> b -> Rev s a -> b
foldlWithIndex :: S.Sequence s => (b -> Int -> a -> b) -> b -> Rev s a -> b
take           :: S.Sequence s => Int -> Rev s a -> Rev s a
drop           :: S.Sequence s => Int -> Rev s a -> Rev s a
splitAt        :: S.Sequence s => Int -> Rev s a -> (Rev s a, Rev s a)
subseq         :: S.Sequence s => Int -> Int -> Rev s a -> Rev s a
filter         :: S.Sequence s => (a -> Bool) -> Rev s a -> Rev s a
partition      :: S.Sequence s => (a -> Bool) -> Rev s a -> (Rev s a, Rev s a)
takeWhile      :: S.Sequence s => (a -> Bool) -> Rev s a -> Rev s a
dropWhile      :: S.Sequence s => (a -> Bool) -> Rev s a -> Rev s a
splitWhile     :: S.Sequence s => (a -> Bool) -> Rev s a -> (Rev s a, Rev s a)
zip            :: S.Sequence s => Rev s a -> Rev s b -> Rev s (a,b)
zip3           :: S.Sequence s => Rev s a -> Rev s b -> Rev s c -> Rev s (a,b,c)
zipWith        :: S.Sequence s => (a -> b -> c) -> Rev s a -> Rev s b -> Rev s c
zipWith3       :: S.Sequence s => (a -> b -> c -> d) -> Rev s a -> Rev s b -> Rev s c -> Rev s d
unzip          :: S.Sequence s => Rev s (a,b) -> (Rev s a, Rev s b)
unzip3         :: S.Sequence s => Rev s (a,b,c) -> (Rev s a, Rev s b, Rev s c)
unzipWith      :: S.Sequence s => (a -> b) -> (a -> c) -> Rev s a -> (Rev s b, Rev s c)
unzipWith3     :: S.Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> Rev s a -> (Rev s b, Rev s c, Rev s d)

moduleName = "RevSeq"
instanceName (N m s) = "RevSeq(" ++ S.instanceName s ++ ")"

data Rev s a = N !Int (s a)
  -- The Int is the size minus one.  The "minus one" makes indexing
  -- calculations easier.

fromSeq xs = N (S.size xs - 1) xs
toSeq (N m xs) = xs

empty = N (-1) S.empty
single x = N 0 (S.single x)
lcons x (N m xs) = N (m+1) (S.rcons xs x)
rcons (N m xs) x = N (m+1) (S.lcons x xs)
append (N m xs) (N n ys) = N (m+n+1) (S.append ys xs)

lview (N m xs) = case S.rview xs of
                   Nothing     -> fail "RevSeq.lview: empty sequence"
                   Just (xs,x) -> return (x, N (m-1) xs)

lhead (N m xs) = S.rhead xs

ltail (N (-1) xs) = error "RevSeq.ltail: empty sequence"
ltail (N m xs) = N (m-1) (S.rtail xs)

rview (N m xs) = case S.lview xs of
                   Nothing     -> fail "RevSeq.rview: empty sequence"
                   Just (x,xs) -> return (N (m-1) xs, x)
 
rhead (N m xs) = S.lhead xs

rtail (N (-1) xs) = error "RevSeq.rtail: empty sequence"
rtail (N m xs) = N (m-1) (S.ltail xs)

null (N m xs) = m == -1
size (N m xs) = m+1
concat (N m xss) = fromSeq (S.concat (S.map toSeq xss))
reverse (N m xs) = N m (S.reverse xs)
reverseOnto (N m xs) (N n ys) = N (m+n+1) (S.append ys (S.reverse xs))
fromList = fromSeq . S.fromList . L.reverse
toList (N m xs) = S.foldl (flip (:)) [] xs
map f (N m xs) = N m (S.map f xs)

concatMap = concatMapUsingFoldr -- only function that uses a default

foldr f e (N m xs) = S.foldl (flip f) e xs
foldl f e (N m xs) = S.foldr (flip f) e xs
foldr1 f (N m xs) = S.foldl1 (flip f) xs
foldl1 f (N m xs) = S.foldr1 (flip f) xs
reducer f e (N m xs) = S.reducel (flip f) e xs
reducel f e (N m xs) = S.reducer (flip f) e xs
reduce1 f (N m xs) = S.reduce1 (flip f) xs

copy n x 
    | n <= 0 = empty
    | otherwise = N (n-1) (S.copy n x)

inBounds (N m xs) i = (i >= 0) && (i <= m)
lookup (N m xs) i = S.lookup xs (m-i)
lookupM (N m xs) i = S.lookupM xs (m-i)
lookupWithDefault d (N m xs) i = S.lookupWithDefault d xs (m-i)
update i x (N m xs) = N m (S.update (m-i) x xs)
adjust f i (N m xs) = N m (S.adjust f (m-i) xs)
mapWithIndex f (N m xs) = N m (S.mapWithIndex (f . (m-)) xs)
foldrWithIndex f e (N m xs) = S.foldlWithIndex f' e xs
  where f' xs i x = f (m-i) x xs
foldlWithIndex f e (N m xs) = S.foldrWithIndex f' e xs
  where f' i x xs = f xs (m-i) x

take i original@(N m xs)
  | i <= 0 = empty
  | i >  m = original
  | otherwise = N (i-1) (S.drop (m-i+1) xs)

drop i original@(N m xs)
  | i <= 0 = original
  | i >  m = empty
  | otherwise = N (m-i) (S.take (m-i+1) xs)

splitAt i original@(N m xs)
  | i <= 0 = (empty, original)
  | i >  m = (original, empty)
  | otherwise = let (ys,zs) = S.splitAt (m-i+1) xs
                in (N (i-1) zs, N (m-i) ys)

subseq i len original@(N m xs)
  | i <= 0 = take len original
  | i >  m = empty
  | i+len > m = N (m-i) (S.take (m-i+1) xs)
  | otherwise = N (len-1) (S.subseq (m-i-len+1) len xs)

filter p = fromSeq . S.filter p . toSeq

partition p (N m xs) = (N (k-1) ys, N (m-k) zs)
  where (ys,zs) = S.partition p xs
        k = S.size ys

takeWhile p = fromSeq . S.reverse . S.takeWhile p . S.reverse . toSeq
dropWhile p = fromSeq . S.reverse . S.dropWhile p . S.reverse . toSeq

splitWhile p (N m xs) = (N (k-1) (S.reverse ys), N (m-k) (S.reverse zs))
  where (ys,zs) = S.splitWhile p (S.reverse xs)
        k = S.size ys

zip (N m xs) (N n ys)
  | m < n = N m (S.zip xs (S.drop (n-m) ys))
  | m > n = N n (S.zip (S.drop (m-n) xs) ys)
  | otherwise = N m (S.zip xs ys)
zip3 (N l xs) (N m ys) (N n zs) = N k (S.zip3 xs' ys' zs')
  where k = min l (min m n)
        xs' = if l == k then xs else S.drop (l-k) xs
        ys' = if m == k then ys else S.drop (m-k) ys
        zs' = if n == k then zs else S.drop (n-k) zs

zipWith f (N m xs) (N n ys)
  | m < n = N m (S.zipWith f xs (S.drop (n-m) ys))
  | m > n = N n (S.zipWith f (S.drop (m-n) xs) ys)
  | otherwise = N m (S.zipWith f xs ys)
zipWith3 f (N l xs) (N m ys) (N n zs) = N k (S.zipWith3 f xs' ys' zs')
  where k = min l (min m n)
        xs' = if l == k then xs else S.drop (l-k) xs
        ys' = if m == k then ys else S.drop (m-k) ys
        zs' = if n == k then zs else S.drop (n-k) zs

unzip (N m xys) = (N m xs, N m ys)
  where (xs,ys) = S.unzip xys

unzip3 (N m xyzs) = (N m xs, N m ys, N m zs)
  where (xs,ys,zs) = S.unzip3 xyzs

unzipWith f g (N m xys) = (N m xs, N m ys)
  where (xs,ys) = S.unzipWith f g xys

unzipWith3 f g h (N m xyzs) = (N m xs, N m ys, N m zs)
  where (xs,ys,zs) = S.unzipWith3 f g h xyzs

-- instances

instance S.Sequence s => S.Sequence (Rev s) where
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

instance S.Sequence s => Functor (Rev s) where
  fmap = map

instance S.Sequence s => Monad (Rev s) where
  return = single
  xs >>= k = concatMap k xs

instance S.Sequence s => MonadPlus (Rev s) where
  mplus = append
  mzero = empty

-- want to say
--   instance Eq (s a) => Eq (Rev s a) where
--     (N m xs) == (N n ys) = (m == n) && (xs == ys)
-- but can't because can't write Eq (s a) context
instance (S.Sequence s, Eq a) => Eq (Rev s a) where
  (N m xs) == (N n ys) = (m == n) && (S.toList xs == S.toList ys)

instance (S.Sequence s, Show a) => Show (Rev s a) where
  show xs = show (toList xs)
