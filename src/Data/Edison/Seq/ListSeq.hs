-- |
--   Module      :  Data.Edison.Seq.ListSeq
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   This module packages the standard prelude list type as a
--   sequence.  This is the baseline sequence implementation and
--   all methods have the default running times listed in
--   "Data.Edison.Seq".

module Data.Edison.Seq.ListSeq (
    -- * Sequence Type
    Seq,

    -- * Sequence Operations
    empty,singleton,lcons,rcons,append,lview,lhead,lheadM,ltail,ltailM,
    rview,rhead,rheadM,rtail,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldrWithIndex',foldlWithIndex,foldlWithIndex',
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- * Unit testing
    structuralInvariant,

    -- * Documentation
    moduleName
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Control.Monad.Identity as ID
import qualified Prelude
import Data.Edison.Prelude
import qualified Data.List
import qualified Data.Edison.Seq as S ( Sequence(..) ) 

-- signatures for exported functions
moduleName     :: String
empty          :: [a]
singleton      :: a -> [a]
lcons          :: a -> [a] -> [a]
rcons          :: a -> [a] -> [a]
append         :: [a] -> [a] -> [a]
lview          :: (Monad rm) => [a] -> rm (a, [a])
lhead          :: [a] -> a
lheadM         :: (Monad rm) => [a] -> rm a
ltail          :: [a] -> [a]
ltailM         :: (Monad rm) => [a] -> rm [a]
rview          :: (Monad rm) => [a] -> rm (a, [a])
rhead          :: [a] -> a
rheadM         :: (Monad rm) => [a] -> rm a
rtail          :: [a] -> [a]
rtailM         :: (Monad rm) => [a] -> rm [a]
null           :: [a] -> Bool
size           :: [a] -> Int
concat         :: [[a]] -> [a]
reverse        :: [a] -> [a]
reverseOnto    :: [a] -> [a] -> [a]
fromList       :: [a] -> [a]
toList         :: [a] -> [a]
map            :: (a -> b) -> [a] -> [b]
concatMap      :: (a -> [b]) -> [a] -> [b]
foldr          :: (a -> b -> b) -> b -> [a] -> b
foldl          :: (b -> a -> b) -> b -> [a] -> b
foldr1         :: (a -> a -> a) -> [a] -> a
foldl1         :: (a -> a -> a) -> [a] -> a
reducer        :: (a -> a -> a) -> a -> [a] -> a
reducel        :: (a -> a -> a) -> a -> [a] -> a
reduce1        :: (a -> a -> a) -> [a] -> a
foldl'         :: (b -> a -> b) -> b -> [a] -> b
foldl1'        :: (a -> a -> a) -> [a] -> a
reducer'       :: (a -> a -> a) -> a -> [a] -> a
reducel'       :: (a -> a -> a) -> a -> [a] -> a
reduce1'       :: (a -> a -> a) -> [a] -> a
copy           :: Int -> a -> [a]
inBounds       :: Int -> [a] -> Bool
lookup         :: Int -> [a] -> a
lookupM        :: (Monad m) => Int -> [a] -> m a
lookupWithDefault :: a -> Int -> [a] -> a
update         :: Int -> a -> [a] -> [a]
adjust         :: (a -> a) -> Int -> [a] -> [a]
mapWithIndex   :: (Int -> a -> b) -> [a] -> [b]
foldrWithIndex :: (Int -> a -> b -> b) -> b -> [a] -> b
foldlWithIndex :: (b -> Int -> a -> b) -> b -> [a] -> b
foldlWithIndex' :: (b -> Int -> a -> b) -> b -> [a] -> b
take           :: Int -> [a] -> [a]
drop           :: Int -> [a] -> [a]
splitAt        :: Int -> [a] -> ([a], [a])
subseq         :: Int -> Int -> [a] -> [a]
filter         :: (a -> Bool) -> [a] -> [a]
partition      :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile      :: (a -> Bool) -> [a] -> [a]
dropWhile      :: (a -> Bool) -> [a] -> [a]
splitWhile     :: (a -> Bool) -> [a] -> ([a], [a])
zip            :: [a] -> [b] -> [(a,b)]
zip3           :: [a] -> [b] -> [c] -> [(a,b,c)]
zipWith        :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3       :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
unzip          :: [(a,b)] -> ([a], [b])
unzip3         :: [(a,b,c)] -> ([a], [b], [c])
unzipWith      :: (a -> b) -> (a -> c) -> [a] -> ([b], [c])
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> [a] -> ([b], [c], [d])
structuralInvariant :: [a] -> Bool

moduleName = "Data.Edison.Seq.ListSeq"

type Seq a = [a]

empty = []
singleton x = [x]
lcons = (:)
rcons x s = s ++ [x]
append = (++)

lview [] = fail "ListSeq.lview: empty sequence"
lview (x:xs) = return (x, xs)

lheadM [] = fail "ListSeq.lheadM: empty sequence"
lheadM (x:xs) = return x

lhead [] = error "ListSeq.lhead: empty sequence"
lhead (x:xs) = x

ltailM [] = fail "ListSeq.ltailM: empty sequence"
ltailM (x:xs) = return xs

ltail [] = error "ListSeq.ltail: empty sequence"
ltail (x:xs) = xs

rview [] = fail "ListSeq.rview: empty sequence"
rview xs = return (rhead xs, rtail xs)

rheadM [] = fail "ListSeq.rheadM: empty sequence"
rheadM (x:xs) = rh x xs
  where rh y [] = return y
        rh y (x:xs) = rh x xs

rhead [] = error "ListSeq.rhead: empty sequence"
rhead (x:xs) = rh x xs
  where rh y [] = y
        rh y (x:xs) = rh x xs

rtailM [] = fail "ListSeq.rtailM: empty sequence"
rtailM (x:xs) = return (rt x xs)
  where rt y [] = []
        rt y (x:xs) = y : rt x xs

rtail [] = error "ListSeq.rtail: empty sequence"
rtail (x:xs) = rt x xs
  where rt y [] = []
        rt y (x:xs) = y : rt x xs

null = Prelude.null
size = length
concat = foldr append empty
reverse = Prelude.reverse

reverseOnto [] ys = ys
reverseOnto (x:xs) ys = reverseOnto xs (x:ys)

fromList xs = xs
toList xs = xs
map = Data.List.map

concatMap = Data.List.concatMap

foldr = Data.List.foldr
foldl = Data.List.foldl

foldr' f e [] = e
foldr' f e (x:xs) = f x $! foldr' f e xs

foldl' f e [] = e
foldl' f e (x:xs) = e `seq` foldl' f (f e x) xs

foldr1 f [] = error "ListSeq.foldr1: empty sequence"
foldr1 f xs = fr xs
  where fr [x]    = x
        fr (x:xs) = f x $ fr xs
        fr _ = error "ListSeq.foldr1: bug!"

foldr1' f [] = error "ListSeq.foldr1': empty sequence"
foldr1' f xs = fr xs
  where fr [x]    = x
        fr (x:xs) = f x $! fr xs
        fr _  = error "ListSeq.foldr1': bug!"

foldl1 f [] = error "ListSeq.foldl1: empty sequence"
foldl1 f (x:xs) = foldl f x xs

foldl1' f [] = error "ListSeq.foldl1': empty sequence"
foldl1' f (x:xs) = foldl' f x xs

reducer f e [] = e
reducer f e xs = f (reduce1 f xs) e

reducer' f e [] = e
reducer' f e xs = (f $! (reduce1' f xs)) $! e

reducel f e [] = e
reducel f e xs = f e (reduce1 f xs)

reducel' f e [] = e
reducel' f e xs = (f $! e) $! (reduce1' f xs)

reduce1 f [] = error "ListSeq.reduce1: empty sequence"
reduce1 f [x] = x
reduce1 f (x1 : x2 : xs) = reduce1 f (f x1 x2 : pairup xs)
  where pairup (x1 : x2 : xs) = f x1 x2 : pairup xs
        pairup xs = xs
  -- can be improved using a counter and bit ops!

reduce1' f [] = error "ListSeq.reduce1': empty sequence"
reduce1' f [x] = x
reduce1' f (x1 : x2 : xs) = x1 `seq` x2 `seq` reduce1' f (f x1 x2 : pairup xs)
  where pairup (x1 : x2 : xs) = x1 `seq` x2 `seq` (f x1 x2 : pairup xs)
        pairup xs = xs

copy n x | n <= 0 = []
         | otherwise = x : copy (n-1) x
  -- depends on n to be unboxed, should test this!

inBounds i xs
  | i >= 0    = not (null (drop i xs))
  | otherwise = False

lookup i xs = ID.runIdentity (lookupM i xs)

lookupM i xs
  | i < 0 = fail "ListSeq.lookup: not found"
  | otherwise = case drop i xs of
                  [] -> fail "ListSeq.lookup: not found"
                  (x:_) -> return x

lookupWithDefault d i xs
  | i < 0 = d
  | otherwise = case drop i xs of
                  [] -> d
                  (x:_) -> x

update i y xs 
    | i < 0     = xs
    | otherwise = upd i xs
  where upd _ [] = []
        upd i (x:xs)
          | i > 0     = x : upd (i - 1) xs
          | otherwise = y : xs

adjust f i xs 
    | i < 0     = xs
    | otherwise = adj i xs
  where adj _ [] = []
        adj i (x:xs)
          | i > 0     = x : adj (i - 1) xs
          | otherwise = f x : xs

mapWithIndex f = mapi 0
  where mapi i [] = []
        mapi i (x:xs) = f i x : mapi (succ i) xs

foldrWithIndex f e = foldi 0
  where foldi i [] = e
        foldi i (x:xs) = f i x (foldi (succ i) xs)

foldrWithIndex' f e = foldi 0
  where foldi i [] = e
        foldi i (x:xs) = f i x $! (foldi (succ i) xs)

foldlWithIndex f = foldi 0
  where foldi i e [] = e
        foldi i e (x:xs) = foldi (succ i) (f e i x) xs

foldlWithIndex' f = foldi 0
  where foldi i e [] = e
        foldi i e (x:xs) = e `seq` foldi (succ i) (f e i x) xs


take i xs | i <= 0 = []
          | otherwise = Data.List.take i xs

drop i xs | i <= 0 = xs
          | otherwise = Data.List.drop i xs

splitAt i xs | i <= 0 = ([], xs)
             | otherwise = Data.List.splitAt i xs

subseq i len xs = take len (drop i xs)
        
filter = Data.List.filter
partition = Data.List.partition
takeWhile = Data.List.takeWhile
dropWhile = Data.List.dropWhile
splitWhile = Data.List.span

zip = Data.List.zip
zip3 = Data.List.zip3
zipWith = Data.List.zipWith
zipWith3 = Data.List.zipWith3
unzip = Data.List.unzip
unzip3 = Data.List.unzip3

unzipWith f g = foldr consfg ([], [])
  where consfg a (bs, cs) = (f a : bs, g a : cs)
  -- could put ~ on tuple

unzipWith3 f g h = foldr consfgh ([], [], [])
  where consfgh a (bs, cs, ds) = (f a : bs, g a : cs, h a : ds)
  -- could put ~ on tuple

-- no invariants
structuralInvariant = const True

-- declare the instance

instance S.Sequence [] where
  {empty = empty; singleton = singleton; lcons = lcons; rcons = rcons;
   append = append; null = null;
   lview = lview; lhead = lhead; ltail = ltail;
   lheadM = lheadM; ltailM = ltailM;
   rview = rview; rhead = rhead; rtail = rtail;
   rheadM = rheadM; rtailM = rtailM;
   size = size; concat = concat; reverse = reverse; map = map;
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   concatMap = concatMap; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; reducer = reducer; reducel = reducel;
   reduce1 = reduce1; reducel' = reducel'; reducer' = reducer';
   reduce1' = reduce1'; copy = copy; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust; mapWithIndex = mapWithIndex; 
   foldrWithIndex = foldrWithIndex; foldrWithIndex' = foldrWithIndex';
   foldlWithIndex = foldlWithIndex; foldlWithIndex' = foldlWithIndex';
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   structuralInvariant = structuralInvariant; instanceName s = moduleName}

