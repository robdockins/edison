-- |
--   Module      :  Data.Edison.Seq.RevSeq
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   This module defines a sequence adaptor @Rev s@.
--   If @s@ is a sequence type constructor, then @Rev s@
--   is a sequence type constructor that is identical to @s@,
--   except that it is kept in the opposite order.
--   Also keeps explicit track of the size of the sequence,
--   similar to the @Sized@ adaptor in "Data.Edison.Seq.SizedSeq".
--
--   This module is most useful when s is a sequence type
--   that offers fast access to the front but slow access
--   to the rear, and your application needs the opposite
--   (i.e., fast access to the rear but slow access to the
--   front).
--
--   All time complexities are determined by the underlying
--   sequence, except that the complexities for accessing
--   the left and right sides of the sequence are exchanged,
--   and size becomes @O( 1 )@.

module Data.Edison.Seq.RevSeq (
    -- * Rev Sequence Type
    Rev, -- Rev s instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence Operations
    empty,singleton,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,map,concatMap,
    fold,fold',fold1,fold1',foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldrWithIndex',foldlWithIndex,foldlWithIndex',
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,
    strict, strictWith,

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

import qualified Control.Applicative as App

import qualified Data.Edison.Seq as S
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Seq.Defaults -- only used by concatMap
import Control.Monad
import qualified Control.Monad.Fail as MF
import Data.Monoid
import Data.Semigroup as SG
import Test.QuickCheck


-- signatures for exported functions
moduleName     :: String
instanceName   :: S.Sequence s => Rev s a -> String
empty          :: S.Sequence s => Rev s a
singleton      :: S.Sequence s => a -> Rev s a
lcons          :: S.Sequence s => a -> Rev s a -> Rev s a
rcons          :: S.Sequence s => a -> Rev s a -> Rev s a
append         :: S.Sequence s => Rev s a -> Rev s a -> Rev s a
lview          :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m (a, Rev s a)
lhead          :: S.Sequence s => Rev s a -> a
lheadM         :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m a
ltail          :: S.Sequence s => Rev s a -> Rev s a
ltailM         :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m (Rev s a)
rview          :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m (a, Rev s a)
rhead          :: S.Sequence s => Rev s a -> a
rheadM         :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m a
rtail          :: S.Sequence s => Rev s a -> Rev s a
rtailM         :: (S.Sequence s, MF.MonadFail m) => Rev s a -> m (Rev s a)
null           :: S.Sequence s => Rev s a -> Bool
size           :: S.Sequence s => Rev s a -> Int
concat         :: S.Sequence s => Rev s (Rev s a) -> Rev s a
reverse        :: S.Sequence s => Rev s a -> Rev s a
reverseOnto    :: S.Sequence s => Rev s a -> Rev s a -> Rev s a
fromList       :: S.Sequence s => [a] -> Rev s a
toList         :: S.Sequence s => Rev s a -> [a]
map            :: S.Sequence s => (a -> b) -> Rev s a -> Rev s b
concatMap      :: S.Sequence s => (a -> Rev s b) -> Rev s a -> Rev s b
fold           :: S.Sequence s => (a -> b -> b) -> b -> Rev s a -> b
fold'          :: S.Sequence s => (a -> b -> b) -> b -> Rev s a -> b
fold1          :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
fold1'         :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
foldr          :: S.Sequence s => (a -> b -> b) -> b -> Rev s a -> b
foldl          :: S.Sequence s => (b -> a -> b) -> b -> Rev s a -> b
foldr1         :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
foldl1         :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
reducer        :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reducel        :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reduce1        :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
foldr'         :: S.Sequence s => (a -> b -> b) -> b -> Rev s a -> b
foldl'         :: S.Sequence s => (b -> a -> b) -> b -> Rev s a -> b
foldr1'        :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
foldl1'        :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
reducer'       :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reducel'       :: S.Sequence s => (a -> a -> a) -> a -> Rev s a -> a
reduce1'       :: S.Sequence s => (a -> a -> a) -> Rev s a -> a
copy           :: S.Sequence s => Int -> a -> Rev s a
inBounds       :: S.Sequence s => Int -> Rev s a -> Bool
lookup         :: S.Sequence s => Int -> Rev s a -> a
lookupM        :: (S.Sequence s, MF.MonadFail m) => Int -> Rev s a -> m a
lookupWithDefault :: S.Sequence s => a -> Int -> Rev s a -> a
update         :: S.Sequence s => Int -> a -> Rev s a -> Rev s a
adjust         :: S.Sequence s => (a -> a) -> Int -> Rev s a -> Rev s a
mapWithIndex   :: S.Sequence s => (Int -> a -> b) -> Rev s a -> Rev s b
foldrWithIndex :: S.Sequence s => (Int -> a -> b -> b) -> b -> Rev s a -> b
foldlWithIndex :: S.Sequence s => (b -> Int -> a -> b) -> b -> Rev s a -> b
foldrWithIndex' :: S.Sequence s => (Int -> a -> b -> b) -> b -> Rev s a -> b
foldlWithIndex' :: S.Sequence s => (b -> Int -> a -> b) -> b -> Rev s a -> b
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
strict         :: S.Sequence s => Rev s a -> Rev s a
strictWith     :: S.Sequence s => (a -> b) -> Rev s a -> Rev s a
structuralInvariant :: S.Sequence s => Rev s a -> Bool

-- bonus functions, not in Sequence signature
fromSeq        :: S.Sequence s => s a -> Rev s a
toSeq          :: S.Sequence s => Rev s a -> s a


moduleName = "Data.Edison.Seq.RevSeq"
instanceName (N _ s) = "RevSeq(" ++ S.instanceName s ++ ")"

data Rev s a = N !Int (s a)
  -- The Int is the size minus one.  The "minus one" makes indexing
  -- calculations easier.

fromSeq xs = N (S.size xs - 1) xs
toSeq (N _ xs) = xs

empty = N (-1) S.empty
singleton x = N 0 (S.singleton x)
lcons x (N m xs) = N (m+1) (S.rcons x xs)
rcons x (N m xs) = N (m+1) (S.lcons x xs)
append (N m xs) (N n ys) = N (m+n+1) (S.append ys xs)

lview (N m xs) = case S.rview xs of
                   Nothing     -> fail "RevSeq.lview: empty sequence"
                   Just (x,xs) -> return (x, N (m-1) xs)

lhead (N _ xs) = S.rhead xs

lheadM (N _ xs) = S.rheadM xs

ltail (N (-1) _) = error "RevSeq.ltail: empty sequence"
ltail (N m xs) = N (m-1) (S.rtail xs)

ltailM (N (-1) _) = fail "RevSeq.ltailM: empty sequence"
ltailM (N m xs) = return (N (m-1) (S.rtail xs))

rview (N m xs) = case S.lview xs of
                   Nothing     -> fail "RevSeq.rview: empty sequence"
                   Just (x,xs) -> return (x, N (m-1) xs)

rhead (N _ xs) = S.lhead xs

rheadM (N _ xs) = S.lheadM xs

rtail (N (-1) _) = error "RevSeq.rtail: empty sequence"
rtail (N m xs) = N (m-1) (S.ltail xs)

rtailM (N (-1) _) = fail "RevSeq.rtailM: empty sequence"
rtailM (N m xs) = return (N (m-1) (S.ltail xs))

null (N m _) = m == -1
size (N m _) = m+1
concat (N _ xss) = fromSeq (S.concat (S.map toSeq xss))
reverse (N m xs) = N m (S.reverse xs)
reverseOnto (N m xs) (N n ys) = N (m+n+1) (S.append ys (S.reverse xs))
fromList = fromSeq . S.fromList . L.reverse
toList (N _ xs) = S.foldl (flip (:)) [] xs
map f (N m xs) = N m (S.map f xs)

concatMap = concatMapUsingFoldr -- only function that uses a default

fold f e (N _ xs) = S.fold f e xs
fold' f e (N _ xs) = S.fold' f e xs
fold1 f (N _ xs) = S.fold1 f xs
fold1' f (N _ xs) = S.fold1' f xs
foldr f e (N _ xs) = S.foldl (flip f) e xs
foldr' f e (N _ xs) = S.foldl' (flip f) e xs
foldl f e (N _ xs) = S.foldr (flip f) e xs
foldl' f e (N _ xs) = S.foldr' (flip f) e xs
foldr1 f (N _ xs) = S.foldl1 (flip f) xs
foldr1' f (N _ xs) = S.foldl1' (flip f) xs
foldl1 f (N _ xs) = S.foldr1 (flip f) xs
foldl1' f (N _ xs) = S.foldr1' (flip f) xs
reducer f e (N _ xs) = S.reducel (flip f) e xs
reducer' f e (N _ xs) = S.reducel' (flip f) e xs
reducel f e (N _ xs) = S.reducer (flip f) e xs
reducel' f e (N _ xs) = S.reducer' (flip f) e xs
reduce1 f (N _ xs) = S.reduce1 (flip f) xs
reduce1' f (N _ xs) = S.reduce1' (flip f) xs

copy n x
    | n <= 0 = empty
    | otherwise = N (n-1) (S.copy n x)

inBounds i (N m _) = (i >= 0) && (i <= m)
lookup i (N m xs) = S.lookup (m-i) xs
lookupM i (N m xs) = S.lookupM (m-i) xs
lookupWithDefault d i (N m xs) = S.lookupWithDefault d (m-i) xs
update i x (N m xs) = N m (S.update (m-i) x xs)
adjust f i (N m xs) = N m (S.adjust f (m-i) xs)
mapWithIndex f (N m xs) = N m (S.mapWithIndex (f . (m-)) xs)

foldrWithIndex f e (N m xs) = S.foldlWithIndex f' e xs
  where f' xs i x = f (m-i) x xs
foldrWithIndex' f e (N m xs) = S.foldlWithIndex' f' e xs
  where f' xs i x = f (m-i) x xs

foldlWithIndex f e (N m xs) = S.foldrWithIndex f' e xs
  where f' i x xs = f xs (m-i) x
foldlWithIndex' f e (N m xs) = S.foldrWithIndex' f' e xs
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
  | i >  m || len <= 0 = empty
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

strict s@(N _ s') = S.strict s' `seq` s
strictWith f s@(N _ s') = S.strictWith f s' `seq` s

structuralInvariant (N i s) = i == ((S.size s) - 1)

-- instances

instance S.Sequence s => S.Sequence (Rev s) where
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
   strict = strict; strictWith = strictWith;
   structuralInvariant = structuralInvariant; instanceName = instanceName}

instance S.Sequence s => Functor (Rev s) where
  fmap = map

instance S.Sequence s => App.Alternative (Rev s) where
  empty = empty
  (<|>) = append

instance S.Sequence s => App.Applicative (Rev s) where
  pure = return
  x <*> y = do
     x' <- x
     y' <- y
     return (x' y')

instance S.Sequence s => Monad (Rev s) where
  return = singleton
  xs >>= k = concatMap k xs

instance S.Sequence s => MonadPlus (Rev s) where
  mplus = append
  mzero = empty

instance Eq (s a) => Eq (Rev s a) where
  (N m xs) == (N n ys) = (m == n) && (xs == ys)

instance (S.Sequence s, Ord a, Eq (s a)) => Ord (Rev s a) where
  compare = defaultCompare

instance (S.Sequence s, Show (s a)) => Show (Rev s a) where
  showsPrec i xs rest
     | i == 0    = L.concat [    moduleName,".fromSeq ",showsPrec 10 (toSeq xs) rest]
     | otherwise = L.concat ["(",moduleName,".fromSeq ",showsPrec 10 (toSeq xs) (')':rest)]

instance (S.Sequence s, Read (s a)) => Read (Rev s a) where
  readsPrec _ xs = maybeParens p xs
      where p xs = tokenMatch (moduleName++".fromSeq") xs
                     >>= readsPrec 10
                     >>= \(l,rest) -> return (fromSeq l,rest)

instance (S.Sequence s, Arbitrary (s a)) => Arbitrary (Rev s a) where
  arbitrary = do xs <- arbitrary
                 return (fromSeq xs)

instance (S.Sequence s, CoArbitrary (s a)) => CoArbitrary (Rev s a) where
  coarbitrary xs = coarbitrary (toSeq xs)

instance S.Sequence s => Semigroup (Rev s a) where
  (<>) = append
instance S.Sequence s => Monoid (Rev s a) where
  mempty  = empty
  mappend = (SG.<>)
