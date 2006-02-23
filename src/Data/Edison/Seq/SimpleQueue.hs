-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | Simple Queues.  All operations have running times as listed in
--   "Data.Edison.Seq" except for the following:
--
--   * rcons  @O( 1 )@
--
--   * lview  @O( 1 )@ if single threaded, @O( n )@ otherwise
--
--   * ltail  @O( 1 )@ if single threaded, @O( n )@ otherwise
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
    empty,single,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,foldrWithIndex',foldlWithIndex',
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

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S ( Sequence(..) )
import Data.Edison.Seq.Defaults
import qualified Data.Edison.Seq.ListSeq as L
import Control.Monad
import Test.QuickCheck

-- signatures for exported functions
moduleName     :: String
empty          :: Seq a
single         :: a -> Seq a
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
structuralInvariant :: Seq a -> Bool

moduleName = "Data.Edison.Seq.SimpleQueue"


data Seq a = Q [a] [a]
  -- invariant: front empty only if rear also empty

-- not exported
makeQ [] ys = Q (L.reverse ys) []
makeQ xs ys = Q xs ys

empty = Q [] []
single x = Q [x] []
lcons x (Q xs ys) = Q (x:xs) ys

rcons y (Q [] _) = Q [y] []
rcons y (Q xs ys) = Q xs (y:ys)

append (Q xs1 ys1) (Q xs2 ys2) =
    Q (xs1 ++ L.reverseOnto ys1 xs2) ys2

lview (Q [] _) = fail "SimpleQueue.lview: empty sequence"
lview (Q [x] ys) = return (x, Q (L.reverse ys) [])
lview (Q (x:xs) ys) = return (x, Q xs ys)

lhead (Q [] _) = error "SimpleQueue.lhead: empty sequence"
lhead (Q (x:xs) _) = x

lheadM (Q [] _) = fail "SimpleQueue.lheadM: empty sequence"
lheadM (Q (x:xs) _) = return x

ltail (Q [x] ys) = Q (L.reverse ys) []
ltail (Q (x:xs) ys) = Q xs ys
ltail q@(Q [] _) = error "SimpleQueue.ltail: empty sequence"

ltailM (Q [x] ys) = return (Q (L.reverse ys) [])
ltailM (Q (x:xs) ys) = return (Q xs ys)
ltailM q@(Q [] _) = fail "SimpleQueue.ltailM: empty sequence"

rview (Q xs (y:ys)) = return (y, Q xs ys)
rview (Q xs []) =
  case L.rview xs of
    Nothing      -> fail "SimpleQueue.rview: empty sequence"
    Just (x,xs') -> return (x, Q xs' [])

rhead (Q xs (y:ys)) = y
rhead (Q [] []) = error "SimpleQueue.rhead: empty sequence"
rhead (Q xs []) = L.rhead xs

rheadM (Q xs (y:ys)) = return y
rheadM (Q [] []) = fail "SimpleQueue.rheadM: empty sequence"
rheadM (Q xs []) = return (L.rhead xs)

rtail (Q xs (y:ys)) = Q xs ys
rtail q@(Q [] []) = error "SimpleQueue.rtail: empty sequence"
rtail (Q xs []) = Q (L.rtail xs) []

rtailM (Q xs (y:ys)) = return (Q xs ys)
rtailM q@(Q [] []) = fail "SimpleQueue.rtailM: empty sequence"
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
revfoldr f e [] = e
revfoldr f e (x:xs) = revfoldr f (f x e) xs

revfoldr' f e [] = e
revfoldr' f e (x:xs) = e `seq` revfoldr' f (f x e) xs

-- local fn on lists
revfoldl f e [] = e
revfoldl f e (x:xs) = f (revfoldl f e xs) x

revfoldl' f e [] = e
revfoldl' f e (x:xs) = e `seq` f (revfoldl' f e xs) x


foldr  f e (Q xs ys) = L.foldr  f (revfoldr  f e ys) xs
foldr' f e (Q xs ys) = L.foldr' f (revfoldr' f e ys) xs

foldl  f e (Q xs ys) = revfoldl  f (L.foldl  f e xs) ys
foldl' f e (Q xs ys) = revfoldl' f (L.foldl' f e xs) ys

foldr1  f (Q xs (y:ys)) = L.foldr f (revfoldr f y ys) xs
foldr1  f (Q [] []) = error "SimpleQueue.foldr1: empty sequence"
foldr1  f (Q xs []) = L.foldr1 f xs

foldr1' f (Q xs (y:ys)) = L.foldr' f (revfoldr' f y ys) xs
foldr1' f (Q [] []) = error "SimpleQueye.foldr1': empty sequence"
foldr1' f (Q xs []) = L.foldr1' f xs

foldl1  f (Q (x:xs) ys) = revfoldl f (L.foldl f x xs) ys
foldl1  f (Q [] _) = error "SimpleQueue.foldl1: empty sequence"

foldl1' f (Q (x:xs) ys) = revfoldl' f (L.foldl' f x xs) ys
foldl1' f (Q [] _) = error "SimpleQueue.foldl1': empty sequence"

filter p (Q xs ys) = makeQ (L.filter p xs) (L.filter p ys)

partition p (Q xs ys)
  = (makeQ xsT ysT, makeQ xsF ysF)
 where
   (xsT,xsF) = L.partition p xs
   (ysT,ysF) = L.partition p ys

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
  {empty = empty; single = single; lcons = lcons; rcons = rcons;
   append = append; lview = lview; lhead = lhead; ltail = ltail;
   lheadM = lheadM; ltailM = ltailM; rheadM = rheadM; rtailM = rtailM;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   map = map; concatMap = concatMap; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; reducer = reducer; reducer' = reducer';
   reducel = reducel; reducel' = reducel'; reduce1 = reduce1; 
   reduce1' = reduce1'; copy = copy; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust;
   mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldrWithIndex' = foldrWithIndex';
   foldlWithIndex = foldlWithIndex; foldlWithIndex' = foldlWithIndex';
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   structuralInvariant = structuralInvariant; instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = single
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  q1 == q2 = toList q1 == toList q2

instance Show a => Show (Seq a) where
  show q = show (toList q)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 ys <- arbitrary
                 return (if L.null xs then Q ys [] else Q xs ys)

  coarbitrary (Q xs ys) = coarbitrary xs . coarbitrary ys
