-- |
--   Module      :  Data.Edison.Seq.RandList
--   Copyright   :  Copyright (c) 1998-1999, 2008 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Random-Access Lists.  All operations are as listed in "Data.Edison.Seq"
--   except the following:
--
--   * rhead*, size  @O( log n )@
--
--   * copy, inBounds    @O( log i )@
--
--   * lookup*, update, adjust, drop @O( min( i, log n ) )@
--
--   * subseq            @O( min( i, log n ) + len )@
--
--   /References:/
--
--   * Chris Okasaki. /Purely Functional Data Structures/. 1998.
--     Section 9.3.1.
--
--   * Chris Okasaki. \"Purely Functional Random Access Lists\".  FPCA'95,
--     pages 86-95.

module Data.Edison.Seq.RandList (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

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
    moduleName
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import qualified Control.Applicative as App

import qualified Data.Edison.Seq as S( Sequence(..) )
import Data.Edison.Seq.Defaults
import qualified Control.Monad.Fail as MF
import Control.Monad.Identity
import Data.Monoid
import Data.Semigroup as SG
import Test.QuickCheck

-- signatures for exported functions
moduleName     :: String
empty          :: Seq a
singleton      :: a -> Seq a
lcons          :: a -> Seq a -> Seq a
rcons          :: a -> Seq a -> Seq a
append         :: Seq a -> Seq a -> Seq a
lview          :: (MF.MonadFail m) => Seq a -> m (a, Seq a)
lhead          :: Seq a -> a
lheadM         :: (MF.MonadFail m) => Seq a -> m a
ltail          :: Seq a -> Seq a
ltailM         :: (MF.MonadFail m) => Seq a -> m (Seq a)
rview          :: (MF.MonadFail m) => Seq a -> m (a, Seq a)
rhead          :: Seq a -> a
rheadM         :: (MF.MonadFail m) => Seq a -> m a
rtail          :: Seq a -> Seq a
rtailM         :: (MF.MonadFail m) => Seq a -> m (Seq a)
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
lookupM        :: (MF.MonadFail m) => Int -> Seq a -> m a
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
moduleName = "Data.Edison.Seq.RandList"


data Tree a = L a | T a (Tree a) (Tree a)   deriving (Eq)
data Seq a = E | C !Int (Tree a) (Seq a)    deriving (Eq)

half :: Int -> Int
half n = n `quot` 2  -- use a shift?

empty = E
singleton x = C 1 (L x) E

lcons x (C i s (C j t xs'))
    | i == j = C (1 + i + j) (T x s t) xs'
lcons x xs = C 1 (L x) xs

copy n x = if n <= 0 then E else buildTrees (1::Int) (L x)
  where buildTrees j t
          | j > n     = takeTrees n (half j) (child t) E
          | otherwise = buildTrees (1 + j + j) (T x t t)

        takeTrees i j t xs
          | i >= j = takeTrees (i - j) j t (C j t xs)
          | i > 0  = takeTrees i (half j) (child t) xs
          | otherwise = xs

        child (T _ _ t) = t
        child _ = error "RandList.copy: bug!"

lview E = fail "RandList.lview: empty sequence"
lview (C _ (L x) xs) = return (x, xs)
lview (C i (T x s t) xs) = return (x, C j s (C j t xs))
  where j = half i

lhead E = error "RandList.lhead: empty sequence"
lhead (C _ (L x) _) = x
lhead (C _ (T x _ _) _) = x

lheadM E = fail "RandList.lheadM: empty sequence"
lheadM (C _ (L x) _) = return x
lheadM (C _ (T x _ _) _) = return x

ltail E = error "RandList.ltail: empty sequence"
ltail (C _ (L _) xs) = xs
ltail (C i (T _ s t) xs) = C j s (C j t xs)
  where j = half i

ltailM E = fail "RandList.ltailM: empty sequence"
ltailM (C _ (L _) xs) = return xs
ltailM (C i (T _ s t) xs) = return (C j s (C j t xs))
  where j = half i

rhead E = error "RandList.rhead: empty sequence"
rhead (C _ t E) = treeLast t
  where treeLast (L x) = x
        treeLast (T _ _ t) = treeLast t
rhead (C _ _ xs) = rhead xs

rheadM E = fail "RandList.rhead: empty sequence"
rheadM (C _ t E) = return(treeLast t)
  where treeLast (L x) = x
        treeLast (T _ _ t) = treeLast t
rheadM (C _ _ xs) = rheadM xs


null E = True
null _ = False

size xs = sz xs
  where sz E = (0::Int)
        sz (C j _ xs) = j + sz xs

reverseOnto E ys = ys
reverseOnto (C _ t xs) ys = reverseOnto xs (revTree t ys)
  where revTree (L x) ys = lcons x ys
        revTree (T x s t) ys = revTree t (revTree s (lcons x ys))

map _ E = E
map f (C j t xs) = C j (mapTree f t) (map f xs)
  where mapTree f (L x) = L (f x)
        mapTree f (T x s t) = T (f x) (mapTree f s) (mapTree f t)

fold  = foldr
fold' f = foldl' (flip f)
fold1  = fold1UsingFold
fold1' = fold1'UsingFold'

foldr _ e E = e
foldr f e (C _ t xs) = foldTree t (foldr f e xs)
  where foldTree (L x) e = f x e
        foldTree (T x s t) e = f x (foldTree s (foldTree t e))

foldr' _ e E = e
foldr' f e (C _ t xs) = foldTree t $! (foldr' f e xs)
  where foldTree (L x) e = f x $! e
        foldTree (T x s t) e = f x $! (foldTree s $! (foldTree t $! e))

foldl _ e E = e
foldl f e (C _ t xs) = foldl f (foldTree e t) xs
  where foldTree e (L x) = f e x
        foldTree e (T x s t) = foldTree (foldTree (f e x) s) t

foldl' _ e E = e
foldl' f e (C _ t xs) = (foldl f $! (foldTree e t)) xs
  where foldTree e (L x) = e `seq` f e x
        foldTree e (T x s t) = e `seq` (foldTree $! (foldTree (f e x) s)) t

reduce1 f xs = case lview xs of
                 Nothing      -> error "RandList.reduce1: empty seq"
                 Just (x, xs) -> red1 x xs
  where red1 x E = x
        red1 x (C _ t xs) = red1 (redTree x t) xs

        redTree x (L y) = f x y
        redTree x (T y s t) = redTree (redTree (f x y) s) t

reduce1' f xs = case lview xs of
                  Nothing      -> error "RandList.reduce1': empty seq"
                  Just (x, xs) -> red1 x xs
  where red1 x E = x
        red1 x (C _ t xs) = (red1 $! (redTree x t)) xs

        redTree x (L y) = x `seq` y `seq` f x y
        redTree x (T y s t) = x `seq` y `seq` (redTree $! (redTree (f x y) s)) t


inBounds i xs = inb xs i
  where inb E _ = False
        inb (C j _ xs) i
          | i < j     = (i >= 0)
          | otherwise = inb xs (i - j)

lookup i xs = runIdentity (lookupM i xs)

lookupM i xs = look xs i
  where look E _ = fail "RandList.lookup bad subscript"
        look (C j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (L x) i
            | i == 0    = return x
            | otherwise = nothing
        lookTree j (T x s t) i
            | i > k  = lookTree k t (i - 1 - k)
            | i /= 0 = lookTree k s (i - 1)
            | otherwise = return x
          where k = half j
        nothing = fail "RandList.lookup: not found"

lookupWithDefault d i xs = look xs i
  where look E _ = d
        look (C j t xs) i
            | i < j     = lookTree j t i
            | otherwise = look xs (i - j)

        lookTree _ (L x) i
            | i == 0    = x
            | otherwise = d
        lookTree j (T x s t) i
            | i > k   = lookTree k t (i - 1 - k)
            | i /= 0  = lookTree k s (i - 1)
            | otherwise = x
          where k = half j

update i y xs = upd i xs
  where upd _ E = E
        upd i (C j t xs)
            | i < j     = C j (updTree i j t) xs
            | otherwise = C j t (upd (i - j) xs)

        updTree i _ t@(L _)
            | i == 0    = L y
            | otherwise = t
        updTree i j (T x s t)
            | i > k   = T x s (updTree (i - 1 - k) k t)
            | i /= 0  = T x (updTree (i - 1) k s) t
            | otherwise = T y s t
          where k = half j

adjust f i xs = adj i xs
  where adj _ E = E
        adj i (C j t xs)
            | i < j     = C j (adjTree i j t) xs
            | otherwise = C j t (adj (i - j) xs)

        adjTree i _ t@(L x)
            | i == 0    = L (f x)
            | otherwise = t
        adjTree i j (T x s t)
            | i > k  = T x s (adjTree (i - 1 - k) k t)
            | i /= 0 = T x (adjTree (i - 1) k s) t
            | otherwise = T (f x) s t
          where k = half j

drop n xs = if n < 0 then xs else drp n xs
  where drp _ E = E
        drp i (C j t xs)
            | i < j     = drpTree i j t xs
            | otherwise = drp (i - j) xs

        drpTree 0 j t xs = C j t xs
        drpTree _ _ (L _) _ = error "RandList.drop: bug.  Impossible case!"
        drpTree i j (T _ s t) xs
            | i > k     = drpTree (i - 1 - k) k t xs
            | otherwise = drpTree (i - 1) k s (C k t xs)
          where k = half j

strict s@E = s
strict s@(C _ t xs) = strictTree t `seq` strict xs `seq` s

strictTree :: Tree t -> Tree t
strictTree t@(L _) = t
strictTree t@(T _ l r) = strictTree l `seq` strictTree r `seq` t

strictWith _ s@E = s
strictWith f s@(C _ t xs) = strictWithTree f t `seq` strictWith f xs `seq` s

strictWithTree :: (t -> a) -> Tree t -> Tree t
strictWithTree f t@(L x) = f x `seq` t
strictWithTree f t@(T x l r) = f x `seq` strictWithTree f l `seq` strictWithTree f r `seq` t


-- the remaining functions all use defaults

rcons = rconsUsingFoldr
append = appendUsingFoldr
rview = rviewDefault
rtail = rtailUsingLview
rtailM = rtailMUsingLview
concat = concatUsingFoldr
reverse = reverseUsingReverseOnto
fromList = fromListUsingCons
toList = toListUsingFoldr
concatMap = concatMapUsingFoldr
foldr1  = foldr1UsingLview
foldr1' = foldr1'UsingLview
foldl1  = foldl1UsingFoldl
foldl1' = foldl1'UsingFoldl'
reducer  = reducerUsingReduce1
reducer' = reducer'UsingReduce1'
reducel  = reducelUsingReduce1
reducel' = reducel'UsingReduce1'
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
take = takeUsingLists
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

-- invariants:
--   * list of complete binary trees in non-decreasing
--     order by size
--   * first argument to 'C' is the number
--     of nodes in the tree
structuralInvariant :: Seq t -> Bool
structuralInvariant E = True
structuralInvariant (C x t s) = x > 0 && checkTree x t && checkSeq x s

   where checkTree 1 (L _) = True
         checkTree w (T _ l r) =
             let w' = (w - 1) `div` 2
             in w' > 0 && checkTree w' l && checkTree w' r
         checkTree _ _ = False

         checkSeq _ E = True
         checkSeq x (C y t s) =
             x <= y && checkTree y t && checkSeq y s


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

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 return (fromList xs)

instance CoArbitrary a => CoArbitrary (Seq a) where
  coarbitrary xs = coarbitrary (toList xs)

instance Semigroup (Seq a) where
  (<>) = append
instance Monoid (Seq a) where
  mempty  = empty
  mappend = (SG.<>)
