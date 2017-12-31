-- |
--   Module      :  Data.Edison.Seq.BankersQueue
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   This module implements Banker's Queues. It has the standard running
--   times from "Data.Edison.Seq" except for the following:
--
--   * rcons, size, inBounds   @O( 1 )@
--
--   /References:/
--
--   * Chris Okasaki, /Purely Functional Data Structures/,
--     1998, sections 6.3.2 and 8.4.1.
--
--   * Chris Okasaki, \"Simple and efficient purely functional
--     queues and deques\", /Journal of Function Programming/
--     5(4):583-592, October 1995.

module Data.Edison.Seq.BankersQueue (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence operations
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
import Control.Monad.Identity
import Test.QuickCheck

-- signatures for exported functions
moduleName     :: String
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

moduleName = "Data.Edison.Seq.BankersQueue"


data Seq a = Q !Int [a] [a] !Int

-- invariant: front at least as long as rear
structuralInvariant (Q x f r y) =
    length f == x && length r == y && x >= y


-- not exported
makeQ :: Int -> [a] -> [a] -> Int -> Seq a
makeQ i xs ys j
  | j > i     = Q (i + j) (xs ++ L.reverse ys) [] 0
  | otherwise = Q i xs ys j

empty = Q 0 [] [] 0
singleton x = Q 1 [x] [] 0
lcons x (Q i xs ys j) = Q (i+1) (x:xs) ys j
rcons y (Q i xs ys j) = makeQ i xs (y:ys) (j+1)

append (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (xs1 ++ L.reverseOnto ys1 xs2) ys2 j2

lview (Q _ [] _ _) = fail "BankersQueue.lview: empty sequence"
lview (Q i (x:xs) ys j) = return (x, makeQ (i-1) xs ys j)

lhead (Q _ [] _ _) = error "BankersQueue.lhead: empty sequence"
lhead (Q _ (x:_) _ _) = x

lheadM (Q _ [] _ _) = fail "BankersQueue.lheadM: empty sequence"
lheadM (Q _ (x:_) _ _) = return x

ltail (Q i (_:xs) ys j) = makeQ (i-1) xs ys j
ltail _ = error "BankersQueue.ltail: empty sequence"

ltailM (Q i (_:xs) ys j) = return (makeQ (i-1) xs ys j)
ltailM _ = fail "BankersQueue.ltail: empty sequence"

rview (Q i xs (y:ys) j) = return (y, Q i xs ys (j-1))
rview (Q i xs [] _) =
  case L.rview xs of
    Nothing      -> fail "BankersQueue.rview: empty sequence"
    Just (x,xs') -> return (x, Q (i-1) xs' [] 0)

rhead (Q _ _ (y:_) _) = y
rhead (Q _ [] [] _) = error "BankersQueue.rhead: empty sequence"
rhead (Q _ xs [] _) = L.rhead xs

rheadM (Q _ _ (y:_) _) = return y
rheadM (Q _ [] [] _) = fail "BankersQueue.rheadM: empty sequence"
rheadM (Q _ xs [] _) = return (L.rhead xs)

rtail (Q i xs (_:ys) j) = Q i xs ys (j-1)
rtail (Q _ [] [] _) = error "BankersQueue.rtail: empty sequence"
rtail (Q i xs [] _) = Q (i-1) (L.rtail xs) [] 0

rtailM (Q i xs (_:ys) j) = return (Q i xs ys (j-1))
rtailM (Q _ [] [] _) = fail "BankersQueue.rtailM: empty sequence"
rtailM (Q i xs [] _) = return (Q (i-1) (L.rtail xs) [] 0)

null (Q i _ _ _) = (i == 0)
size (Q i _ _ j) = i + j
reverse (Q i xs ys j) = makeQ j ys xs i

reverseOnto (Q i1 xs1 ys1 j1) (Q i2 xs2 ys2 j2) =
    Q (i1 + j1 + i2) (ys1 ++ L.reverseOnto xs1 xs2) ys2 j2

fromList xs = Q (length xs) xs [] 0

toList (Q _ xs ys j)
  | j == 0 = xs
  | otherwise = xs ++ L.reverse ys

map f (Q i xs ys j) = Q i (L.map f xs) (L.map f ys) j

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

revfoldl' :: (b -> t -> b) -> b -> [t] -> b
revfoldl' _ e [] = e
revfoldl' f e (x:xs) = (\z -> f z x) $! (revfoldl f e xs)

fold  f e (Q _ xs ys _) = L.foldr f (L.foldr f e ys) xs
fold' f e (Q _ xs ys _) = (L.foldl' (flip f) $! (L.foldl' (flip f) e ys)) xs
fold1  = fold1UsingFold
fold1' = fold1'UsingFold'

foldr  f e (Q _ xs ys _) = L.foldr  f (revfoldr  f e ys) xs
foldr' f e (Q _ xs ys _) = L.foldr' f (revfoldr' f e ys) xs
foldl  f e (Q _ xs ys _) = revfoldl  f (L.foldl  f e xs) ys
foldl' f e (Q _ xs ys _) = revfoldl' f (L.foldl' f e xs) ys

foldr1 f (Q _ xs (y:ys) _) = L.foldr f (revfoldr f y ys) xs
foldr1 f (Q i xs [] _)
  | i == 0 = error "BankersQueue.foldr1: empty sequence"
  | otherwise = L.foldr1 f xs

foldr1' f (Q _ xs (y:ys) _) = L.foldr' f (revfoldr' f y ys) xs
foldr1' f (Q i xs [] _)
  | i == 0 = error "BankersQueue.foldr1': empty sequence"
  | otherwise = L.foldr1' f xs

foldl1 f (Q _ (x:xs) ys _) = revfoldl f (L.foldl f x xs) ys
foldl1 _ _ = error "BankersQueue.foldl1: empty sequence"

foldl1' f (Q _ (x:xs) ys _) = revfoldl' f (L.foldl' f x xs) ys
foldl1' _ _ = error "BankersQueue.foldl1': empty sequence"

copy n x
  | n < 0     = empty
  | otherwise = Q n (L.copy n x) [] 0

-- reduce1: given sizes could do more effective job of dividing evenly!

lookup idx q = runIdentity (lookupM idx q)

lookupM idx (Q i xs ys j)
  | idx < i   = L.lookupM idx xs
  | otherwise = L.lookupM (j - (idx - i) - 1) ys

lookupWithDefault d idx (Q i xs ys j)
  | idx < i   = L.lookupWithDefault d idx xs
  | otherwise = L.lookupWithDefault d (j - (idx - i) - 1) ys

update idx e q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.update idx e xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.update k' e ys) j

adjust f idx q@(Q i xs ys j)
  | idx < i = if idx < 0 then q
             else Q i (L.adjust f idx xs) ys j
  | otherwise = let k' = j - (idx - i) - 1
                in if k' < 0 then q
                   else Q i xs (L.adjust f k' ys) j

{-
could do
  mapWithIndex   :: (Int -> a -> b) -> s a -> s b
  foldrWithIndex :: (Int -> a -> b -> b) -> b -> s a -> b
  foldlWithIndex :: (b -> Int -> a -> b) -> b -> s a -> b
but don't bother for now
-}

take len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then empty
    else Q len (L.take len xs) [] 0
  else let len' = len - i in
    if len' >= j then q
    else Q i xs (L.drop (j - len') ys) len'

drop len q@(Q i xs ys j) =
  if len <= i then
    if len <= 0 then q
    else makeQ (i - len) (L.drop len xs) ys j
  else let len' = len - i in
    if len' >= j then empty
    else Q (j - len') (L.reverse (L.take (j - len') ys)) [] 0
  -- could write more efficient version of reverse (take ...)

splitAt idx q@(Q i xs ys j) =
  if idx <= i then
    if idx <= 0 then (empty, q)
    else let (xs',xs'') = L.splitAt idx xs
         in (Q idx xs' [] 0, makeQ (i - idx) xs'' ys j)
  else let idx' = idx - i in
    if idx' >= j then (q, empty)
    else let (ys', ys'') = L.splitAt (j - idx') ys
         in (Q i xs ys'' idx', Q (j - idx') (L.reverse ys') [] 0)
      -- could do splitAt followed by reverse more efficiently...


strict l@(Q _ xs ys _) = L.strict xs `seq` L.strict ys `seq` l
strictWith f l@(Q _ xs ys _) = L.strictWith f xs `seq` L.strictWith f ys `seq` l

-- the remaining functions all use defaults

concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
reducer' = reducer'UsingReduce1'
reducel' = reducel'UsingReduce1'
reduce1' = reduce1'UsingLists
inBounds = inBoundsUsingSize
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
subseq = subseqDefault
filter = filterUsingLists
partition = partitionUsingLists
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
   reducer = reducer; reducer' = reducer';
   reducel = reducel; reducel' = reducel'; reduce1 = reduce1; reduce1' = reduce1';
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
  q1 == q2 =
    (size q1 == size q2) && (toList q1 == toList q2)

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary =
    do xs <- arbitrary
       ys <- arbitrary
       return (let i = L.size xs
                   j = L.size ys
               in if i >= j then Q i xs ys j else Q j ys xs i)

instance CoArbitrary a => CoArbitrary (Seq a) where
  coarbitrary (Q _ xs ys _) = coarbitrary xs . coarbitrary ys

instance Semigroup (Seq a) where
  (<>) = append
instance Monoid (Seq a) where
  mempty  = empty
  mappend = (SG.<>)
