-- |
--   Module      :  Data.Edison.Seq.SimpleQueue
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Simple Queues.  All operations have running times as listed in
--   "Data.Edison.Seq" except for the following:
--
--   * rcons, fromList   @O( 1 )@
--
--   * lview, ltail*   @O( 1 )@ if single threaded, @O( n )@ otherwise
--
--   * inBounds, lookup, update, drop, splitAt  @O( n )@
--
--   /References:/
--
--   * Chris Okasaki. /Purely Functional Data Structures/. 1998.
--     Section 5.2.
--
--   * F. Warren Burton. \"An efficient functional implementation of FIFO queues\".
--     /Information Processing Letters/, 14(5):205-206, July 1982.

module Data.Edison.Seq.SimpleQueue (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

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
    strict, strictWith,

    -- * Unit testing
    structuralInvariant,

    -- * Documentation
    moduleName
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import qualified Control.Applicative as App

import qualified Data.Edison.Seq as S ( Sequence(..) )
import Data.Edison.Seq.Defaults
import qualified Data.Edison.Seq.ListSeq as L
import Data.Monoid
import Data.Semigroup as SG
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Test.QuickCheck

-- signatures for exported functions
moduleName     :: String
empty          :: Seq a
singleton      :: a -> Seq a
lcons          :: a -> Seq a -> Seq a
rcons          :: a -> Seq a -> Seq a
append         :: Seq a -> Seq a -> Seq a
lview          :: (Fail.MonadFail m) => Seq a -> m (a, Seq a)
lhead          :: Seq a -> a
lheadM         :: (Fail.MonadFail m) => Seq a -> m a
ltail          :: Seq a -> Seq a
ltailM         :: (Fail.MonadFail m) => Seq a -> m (Seq a)
rview          :: (Fail.MonadFail m) => Seq a -> m (a, Seq a)
rhead          :: Seq a -> a
rheadM         :: (Fail.MonadFail m) => Seq a -> m a
rtail          :: Seq a -> Seq a
rtailM         :: (Fail.MonadFail m) => Seq a -> m (Seq a)
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
lookupM        :: (Fail.MonadFail m) => Int -> Seq a -> m a
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

moduleName = "Data.Edison.Seq.SimpleQueue"


data Seq a = Q [a] [a]
  -- invariant: front empty only if rear also empty

-- not exported
makeQ :: [a] -> [a] -> Seq a
makeQ [] ys = Q (L.reverse ys) []
makeQ xs ys = Q xs ys

empty = Q [] []
singleton x = Q [x] []
lcons x (Q xs ys) = Q (x:xs) ys

rcons y (Q [] _) = Q [y] []
rcons y (Q xs ys) = Q xs (y:ys)

append (Q xs1 ys1) (Q xs2 ys2) =
    Q (xs1 ++ L.reverseOnto ys1 xs2) ys2

lview (Q [] _) = fail "SimpleQueue.lview: empty sequence"
lview (Q [x] ys) = return (x, Q (L.reverse ys) [])
lview (Q (x:xs) ys) = return (x, Q xs ys)

lhead (Q [] _) = error "SimpleQueue.lhead: empty sequence"
lhead (Q (x:_) _) = x

lheadM (Q [] _) = fail "SimpleQueue.lheadM: empty sequence"
lheadM (Q (x:_) _) = return x

ltail (Q [_] ys) = Q (L.reverse ys) []
ltail (Q (_:xs) ys) = Q xs ys
ltail (Q [] _) = error "SimpleQueue.ltail: empty sequence"

ltailM (Q [_] ys) = return (Q (L.reverse ys) [])
ltailM (Q (_:xs) ys) = return (Q xs ys)
ltailM (Q [] _) = fail "SimpleQueue.ltailM: empty sequence"

rview (Q xs (y:ys)) = return (y, Q xs ys)
rview (Q xs []) =
  case L.rview xs of
    Nothing      -> fail "SimpleQueue.rview: empty sequence"
    Just (x,xs') -> return (x, Q xs' [])

rhead (Q _ (y:_)) = y
rhead (Q [] []) = error "SimpleQueue.rhead: empty sequence"
rhead (Q xs []) = L.rhead xs

rheadM (Q _ (y:_)) = return y
rheadM (Q [] []) = fail "SimpleQueue.rheadM: empty sequence"
rheadM (Q xs []) = return (L.rhead xs)

rtail (Q xs (_:ys)) = Q xs ys
rtail (Q [] []) = error "SimpleQueue.rtail: empty sequence"
rtail (Q xs []) = Q (L.rtail xs) []

rtailM (Q xs (_:ys)) = return (Q xs ys)
rtailM (Q [] []) = fail "SimpleQueue.rtailM: empty sequence"
rtailM (Q xs []) = return (Q (L.rtail xs) [])

null (Q [] _) = True
null _ = False

size (Q xs ys) = length xs + length ys

reverse (Q xs []) = Q (L.reverse xs) []
reverse (Q xs ys) = Q ys xs

reverseOnto (Q xs1 ys1) (Q xs2 ys2) =
    Q (ys1 ++ L.reverseOnto xs1 xs2) ys2

fromList xs = Q xs []

toList (Q xs []) = xs
toList (Q xs ys) = xs ++ L.reverse ys

map f (Q xs ys) = Q (L.map f xs) (L.map f ys)

-- local fn on lists
revfoldr :: (t -> t1 -> t1) -> t1 -> [t] -> t1
revfoldr _ e [] = e
revfoldr f e (x:xs) = revfoldr f (f x e) xs

revfoldr' :: (t -> a -> a) -> a -> [t] -> a
revfoldr' _ e [] = e
revfoldr' f e (x:xs) = e `seq` revfoldr' f (f x e) xs

-- local fn on lists
revfoldl :: (t -> t1 -> t) -> t -> [t1] -> t
revfoldl _ e [] = e
revfoldl f e (x:xs) = f (revfoldl f e xs) x

revfoldl' :: (a -> t -> a) -> a -> [t] -> a
revfoldl' _ e [] = e
revfoldl' f e (x:xs) = e `seq` f (revfoldl' f e xs) x

fold   f e (Q xs ys) = L.foldr f (L.foldr f e ys) xs
fold'  f e (Q xs ys) = L.foldl' (flip f) (L.foldl' (flip f) e ys) xs
fold1  = fold1UsingFold
fold1' = fold1'UsingFold'

foldr  f e (Q xs ys) = L.foldr  f (revfoldr  f e ys) xs
foldr' f e (Q xs ys) = L.foldr' f (revfoldr' f e ys) xs

foldl  f e (Q xs ys) = revfoldl  f (L.foldl  f e xs) ys
foldl' f e (Q xs ys) = revfoldl' f (L.foldl' f e xs) ys

foldr1  f (Q xs (y:ys)) = L.foldr f (revfoldr f y ys) xs
foldr1  _ (Q [] []) = error "SimpleQueue.foldr1: empty sequence"
foldr1  f (Q xs []) = L.foldr1 f xs

foldr1' f (Q xs (y:ys)) = L.foldr' f (revfoldr' f y ys) xs
foldr1' _ (Q [] []) = error "SimpleQueye.foldr1': empty sequence"
foldr1' f (Q xs []) = L.foldr1' f xs

foldl1  f (Q (x:xs) ys) = revfoldl f (L.foldl f x xs) ys
foldl1  _ (Q [] _) = error "SimpleQueue.foldl1: empty sequence"

foldl1' f (Q (x:xs) ys) = revfoldl' f (L.foldl' f x xs) ys
foldl1' _ (Q [] _) = error "SimpleQueue.foldl1': empty sequence"

filter p (Q xs ys) = makeQ (L.filter p xs) (L.filter p ys)

partition p (Q xs ys)
  = (makeQ xsT ysT, makeQ xsF ysF)
 where
   (xsT,xsF) = L.partition p xs
   (ysT,ysF) = L.partition p ys

strict s@(Q xs ys) = L.strict xs `seq` L.strict ys `seq` s
strictWith f s@(Q xs ys) = L.strictWith f xs `seq` L.strictWith f ys `seq` s

-- the remaining functions all use defaults

concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
reducer  = reducerUsingReduce1
reducer' = reducer'UsingReduce1'
reducel  = reducelUsingReduce1
reducel' = reducel'UsingReduce1'
reduce1  = reduce1UsingLists
reduce1' = reduce1'UsingLists
copy = copyUsingLists
inBounds = inBoundsUsingLookupM
lookup = lookupUsingLookupM
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingLookupM
update = updateUsingAdjust
adjust = adjustUsingLists
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
take = takeUsingLists
drop = dropUsingLists
splitAt = splitAtDefault
subseq = subseqDefault
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
unzip = unzipUsingLists
unzip3 = unzip3UsingLists
unzipWith = unzipWithUsingLists
unzipWith3 = unzipWith3UsingLists

-- invariant:
--   * front empty only if rear also empty

structuralInvariant (Q x y) = not (L.null x) || L.null y

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
   structuralInvariant = structuralInvariant; instanceName _ = moduleName}

instance Functor Seq where
  fmap = map

instance App.Alternative Seq where
  empty = empty
  (<|>) = append

instance App.Applicative Seq where
  pure = return
  x <*> y = do
     x' <- x
     y' <- y
     return (x' y')

instance Monad Seq where
  return = singleton
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  q1 == q2 = toList q1 == toList q2

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 ys <- arbitrary
                 return (if L.null xs then Q ys [] else Q xs ys)

instance CoArbitrary a => CoArbitrary (Seq a) where
  coarbitrary (Q xs ys) = coarbitrary xs . coarbitrary ys

instance Semigroup (Seq a) where
  (<>) = append
instance Monoid (Seq a) where
  mempty  = empty
  mappend = (SG.<>)
