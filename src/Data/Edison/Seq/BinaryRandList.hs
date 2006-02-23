-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | Binary Random-Access lists.  All functions have the standard running
--   times from "Data.Edison.Seq" except the following:
--
--  * lookup   @O( log n )@
--
--  * update   @O( log n )@
--
--    /References:/
--
--  * Chris Okasaki. /Purely Functional Data Structures/. 1998.
--    Section 10.1.2.

module Data.Edison.Seq.BinaryRandList (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence Operations
    empty,single,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
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

import Control.Monad.Identity
import Data.Maybe

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S ( Sequence(..) ) 
import Data.Edison.Seq.Defaults
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
inBounds       :: Seq a -> Int -> Bool
lookup         :: Seq a -> Int -> a
lookupM        :: (Monad m) => Seq a -> Int -> m a
lookupWithDefault :: a -> Seq a -> Int -> a
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

moduleName = "Data.Edison.Seq.BinaryRandList"


data Seq a = E | Even (Seq (a,a)) | Odd a (Seq (a,a))    deriving (Eq)

-- not exported, rewrite as bit ops?
--even n = (n `mod` 2) == 0
--odd n  = (n `mod` 2) <> 0
half n = n `div` 2

mkEven E = E
mkEven ps = Even ps

empty = E
single x = Odd x E

lcons x E = Odd x E
lcons x (Even ps) = Odd x ps
lcons x (Odd y ps) = Even (lcons (x,y) ps)

append xs E = xs
append xs ys@(Even pys) =
  case xs of
    E -> ys
    Even pxs -> Even (append pxs pys)
    Odd x pxs -> Odd x (append pxs pys)
append xs ys@(Odd _ _) = foldr lcons ys xs

copy n x 
    | n <= 0 = E
    | otherwise = cp n x
  where cp :: Int -> a -> Seq a
        cp n x
          | odd n = Odd x (cp (half n) (x,x))
          | n == 0 = E
          | otherwise = Even (cp (half n) (x,x))

lview E = fail "BinaryRandList.lview: empty sequence"
lview (Even ps) = case lview ps of
                    Just ((x,y), ps') -> return (x, Odd y ps')
                    Nothing -> error "BinaryRandList.lview: bug!"
lview (Odd x ps) = return (x, mkEven ps)

lhead E = error "BinaryRandList.lhead: empty sequence"
lhead (Even ps) = fst (lhead ps)
lhead (Odd x ps) = x

lheadM E = fail "BinaryRandList.lheadM: empty sequence"
lheadM (Even ps) = return (fst (lhead ps))
lheadM (Odd x ps) = return (x)

ltail E = error "BinaryRandList.ltail: empty sequence"
ltail (Even ps) = case lview ps of
                    Just ((x,y), ps') -> Odd y ps'
                    Nothing -> error "BinaryRandList.ltail: bug!"
ltail (Odd x ps) = mkEven ps

ltailM E = fail "BinaryRandList.ltailM: empty sequence"
ltailM (Even ps) = case lview ps of
                      Just ((x,y), ps') -> return (Odd y ps')
                      Nothing -> error "BinaryRandList.ltailM: bug!"
ltailM (Odd x ps) = return (mkEven ps)

rhead E = error "BinaryRandList.rhead: empty sequence"
rhead (Even ps) = snd (rhead ps)
rhead (Odd x E) = x
rhead (Odd x ps) = snd (rhead ps)

rheadM E = fail "BinaryRandList.rheadM: empty sequence"
rheadM (Even ps) = return (snd (rhead ps))
rheadM (Odd x E) = return x
rheadM (Odd x ps) = return (snd (rhead ps))


null E = True
null _ = False

size E = 0
size (Even ps) = 2 * size ps
size (Odd x ps) = 1 + 2 * size ps

map f E = E
map f (Even ps)  = Even (map (\(x,y) -> (f x,f y)) ps)
map f (Odd x ps) = Odd (f x) (map (\(x,y) -> (f x,f y)) ps)

foldr f e E = e
foldr f e (Even ps)  = foldr (\(x,y) e -> f x (f y e)) e ps
foldr f e (Odd x ps) = f x (foldr (\(x,y) e -> f x (f y e)) e ps)

foldr' f e E = e
foldr' f e (Even ps)  = foldr' (\(x,y) e -> f x $! f y $! e) e ps
foldr' f e (Odd x ps) = f x $! (foldr' (\(x,y) e -> f x $! f y $! e) e ps)

foldl f e E = e
foldl f e (Even ps)  = foldl (\e (x,y) -> f (f e x) y) e ps
foldl f e (Odd x ps) = foldl (\e (x,y) -> f (f e x) y) (f e x) ps

foldl' f e E = e
foldl' f e (Even ps)  = foldl' (\e (x,y) -> f (f e x) y) e ps
foldl' f e (Odd x ps) = e `seq` foldl' (\e (x,y) -> e `seq` (\z -> f z y) $! (f e x)) (f e x) ps

reduce1 f E = error "BinaryRandList.reduce1: empty seq"
reduce1 f (Even ps)  = reduce1 f (map (uncurry f) ps)
reduce1 f (Odd x E)  = x
reduce1 f (Odd x ps) = f x (reduce1 f (map (uncurry f) ps))

reduce1' f E = error "BinaryRandList.reduce1': empty seq"
reduce1' f (Even ps)  = reduce1' f (map (uncurry f) ps)
reduce1' f (Odd x E)  = x
reduce1' f (Odd x ps) = (f $! x) $! (reduce1' f (map (uncurry f) ps))


inBounds xs i = (i >= 0) && inb xs i
  where inb :: Seq a -> Int -> Bool
        inb E i = False
        inb (Even ps) i = inb ps (half i)
        inb (Odd x ps) i = (i == 0) || inb ps (half (i-1))

lookup xs i = runIdentity (lookupM xs i)

lookupM xs i
    | i < 0     = fail "BinaryRandList.lookup: bad subscript"
    | otherwise = lookFun nothing xs i return
    where
    	nothing = fail "BinaryRandList.lookup: not found"

lookupWithDefault d xs i
    | i < 0 = d
    | otherwise = lookFun d xs i id

-- not exported
lookFun :: b -> Seq a -> Int -> (a -> b) -> b
lookFun d E i f = d
lookFun d (Even ps) i f
  | even i = lookFun d ps (half i) (f . fst)
  | otherwise = lookFun d ps (half i) (f . snd)
lookFun d (Odd x ps) i f
  | odd i = lookFun d ps (half (i-1)) (f . fst)
  | i == 0 = f x
  | otherwise = lookFun d ps (half (i-1)) (f . snd)

adjust f i xs
    | i < 0 = xs
    | otherwise = adj f i xs
  where adj :: (a -> a) -> Int -> Seq a -> Seq a
        adj f i E = E
        adj f i (Even ps)
          | even i = Even (adj (mapFst f) (half i) ps)
          | otherwise = Even (adj (mapSnd f) (half i) ps)
        adj f i (Odd x ps)
          | odd i = Odd x (adj (mapFst f) (half (i-1)) ps)
          | i == 0 = Odd (f x) ps
          | otherwise = Odd x (adj (mapSnd f) (half (i-1)) ps)

-- not exported
mapFst f (x,y) = (f x,y)
mapSnd f (x,y) = (x,f y)

take n xs = if n <= 0 then E else tak n xs
  where tak :: Int -> Seq a -> Seq a
        tak 0 xs = E
        tak i E = E
        tak i (Even ps)
          | even i = Even (tak (half i) ps)
        tak i (Odd x ps)
          | odd i = Odd x (tak (half (i-1)) ps)
        tak i xs = takeUsingLists i xs

-- drop is O(log^2 n) instead of O(log n)??
drop n xs = if n <= 0 then xs else drp n xs
  where drp :: Int -> Seq a -> Seq a
        drp 0 xs = xs
        drp i E = E
        drp i (Even ps)
          | even i = mkEven (drp (half i) ps)
          | otherwise = fromMaybe empty (ltailM (mkEven (drp (half i) ps)))
        drp i (Odd _ ps)
          | odd i = mkEven (drp (half (i-1)) ps)
          | otherwise = fromMaybe empty (ltailM (mkEven (drp (half (i-1)) ps)))

-- structural invariants are enforced by the type system
structuralInvariant = const True

-- the remaining functions all use defaults

rcons = rconsUsingFoldr
rview = rviewDefault
rtail = rtailUsingLview
rtailM = rtailMUsingLview
concat = concatUsingFoldr
reverse = reverseUsingReverseOnto
reverseOnto = reverseOntoUsingFoldl
fromList = fromListUsingCons
toList = toListUsingFoldr
concatMap = concatMapUsingFoldr
foldr1 = foldr1UsingLview
foldr1' = foldr1'UsingLview
foldl1 = foldl1UsingFoldl
foldl1' = foldl1'UsingFoldl'
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reducer' = reducer'UsingReduce1'
reducel' = reducel'UsingReduce1'
update = updateUsingAdjust
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
splitAt = splitAtDefault
filter = filterUsingFoldr
partition = partitionUsingFoldr
subseq = subseqDefault
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview

-- for zips, could optimize by calculating which one is shorter and
-- retaining its shape

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
  {empty = empty; single = single; lcons = lcons; rcons = rcons;
   append = append; lview = lview; lhead = lhead; ltail = ltail;
   lheadM = lheadM; ltailM = ltailM; rheadM = rheadM; rtailM = rtailM;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   map = map; concatMap = concatMap; foldr = foldr; foldr' = foldr';
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; reducer = reducer;
   reducer' = reducer'; reducel = reducel; reducel' = reducel';
   reduce1 = reduce1; reduce1' = reduce1'; copy = copy; 
   inBounds = inBounds; lookup = lookup;
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

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = single
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

-- instance Eq (Seq a) is derived

instance Show a => Show (Seq a) where
  show xs = show (toList xs)

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary 
                 return (fromList xs)

  coarbitrary E = variant 0
  coarbitrary (Even ps) = variant 1 . coarbitrary ps
  coarbitrary (Odd x ps) = variant 2 . coarbitrary x . coarbitrary ps
