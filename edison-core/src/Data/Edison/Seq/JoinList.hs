-- |
--   Module      :  Data.Edison.Seq.JoinList
--   Copyright   :  Copyright (c) 1998-1999 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   Join lists. All running times are as listed in "Data.Edison.Seq" except
--   for the following:
--
--   * rcons, append         @O( 1 )@
--
--   * ltail*, lview         @O( 1 )@    when used single-threaded, @O( n )@ otherwise
--
--   * lhead*                @O( n )@
--
--   * inBounds, lookup      @O( n )@
--
--   * copy                  @O( log i )@
--
--   * concat                @O( n1 )@
--
--   * concatMap, (>>=)      @O( n * t )@, where @n@ is the length of the input sequence and
--                                         @t@ is the running time of @f@

module Data.Edison.Seq.JoinList (
    -- * Sequence Type
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- * Sequence Operations
    empty,singleton,lcons,rcons,append,lview,lhead,ltail,rview,rhead,rtail,
    lheadM,ltailM,rheadM,rtailM,
    null,size,concat,reverse,reverseOnto,fromList,toList,map,concatMap,
    fold,fold',fold1,fold1',foldr,foldr',foldl,foldl',foldr1,foldr1',foldl1,foldl1',
    reducer,reducer',reducel,reducel',reduce1,reduce1',
    copy,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
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

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S ( Sequence(..) )
import Data.Edison.Seq.Defaults
import Control.Monad
import Data.Monoid
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

moduleName = "Data.Edison.Seq.JoinList"

data Seq a = E | L a | A (Seq a) (Seq a)
  -- invariant: E never a child of A

half :: Int -> Int
half n = n `div` 2

empty = E
singleton = L

lcons x E = L x
lcons x xs = A (L x) xs

rcons x E = L x
rcons x xs = A xs (L x)

append E ys = ys
append xs E = xs
append xs ys = A xs ys


-- path reversal on lview/ltail

lview E = fail "JoinList.lview: empty sequence"
lview (L x) = return (x, E)
lview (A xs ys) = lvw xs ys
  where lvw E zs = error "JoinList.lvw: bug"
        lvw (L x) zs = return (x, zs)
        lvw (A xs ys) zs = lvw xs (A ys zs)

lhead E = error "JoinList.lhead: empty sequence"
lhead (L x) = x
lhead (A xs ys) = lhead xs

lheadM E = fail "JoinList.lheadM: empty sequence"
lheadM (L x) = return x
lheadM (A xs ys) = lheadM xs

ltail E = error "JoinList.ltail: empty sequence"
ltail (L x) = E
ltail (A xs ys) = ltl xs ys
  where ltl E zs = error "JoinList.ltl: bug"
        ltl (L x) zs = zs
        ltl (A xs ys) zs = ltl xs (A ys zs)

ltailM E = fail "JoinList.ltailM: empty sequence"
ltailM (L x) = return E
ltailM (A xs ys) = return (ltl xs ys)
  where ltl E zs = error "JoinList.ltl: bug"
        ltl (L x) zs = zs
        ltl (A xs ys) zs = ltl xs (A ys zs)


-- Don't want to do plain path reversal on rview/rtail because of expectation
-- that left accesses are more common, so we would prefer to keep the left
-- spine short.

rview E = fail "JoinLis.rview: empty sequence"
rview (L x) = return (x, E)
rview (A xs ys) = rvw xs ys
  where rvw xs (A ys (A zs s)) = rvw (A xs (A ys zs)) s
        rvw xs (A ys (L x)) = return (x, A xs ys)
        rvw xs (L x) = return (x, xs)
        rvw xs _ = error "JoinList.rvw: bug"
 
rhead E = error "JoinList.rhead: empty sequence"
rhead (L x) = x
rhead (A xs ys) = rhead ys

rheadM E = fail "JoinList.rheadM: empty sequence"
rheadM (L x) = return x
rheadM (A xs ys) = rheadM ys

rtail E = error "JoinList.rtail: empty sequence"
rtail (L x) = E
rtail (A xs ys) = rtl xs ys
  where rtl xs (A ys (A zs s)) = A (A xs ys) (rtl zs s)
        rtl xs (A ys (L _)) = A xs ys
        rtl xs (L x) = xs
        rtl xs _ = error "JoinList.rtl: bug"

rtailM E = fail "JoinList.rtailM: empty sequence"
rtailM (L x) = return E
rtailM (A xs ys) = return (rtl xs ys)
  where rtl xs (A ys (A zs s)) = A (A xs ys) (rtl zs s)
        rtl xs (A ys (L _)) = A xs ys
        rtl xs (L x) = xs
        rtl xs _ = error "JoinList.rtl: bug"

null E = True
null _ = False

size xs = sz xs (0::Int)
  where sz E n = n
        sz (L x) n = n + (1::Int)
        sz (A xs ys) n = sz xs (sz ys n)

reverse (A xs ys) = A (reverse ys) (reverse xs)
reverse xs = xs -- L x or E

toList xs = tol xs []
  where tol E rest = rest
        tol (L x) rest = x:rest
        tol (A xs ys) rest = tol xs (tol ys rest)

map f E = E
map f (L x) = L (f x)
map f (A xs ys) = A (map f xs) (map f ys)

fold   = foldr
fold'  = foldr'
fold1  = fold1UsingFold
fold1' = fold1'UsingFold'

foldr f e E = e
foldr f e (L x) = f x e
foldr f e (A xs ys) = foldr f (foldr f e ys) xs

foldr' f e E = e
foldr' f e (L x) = f x $! e
foldr' f e (A xs ys) = (foldr' f $! (foldr' f e ys)) xs

foldl f e E = e
foldl f e (L x) = f e x
foldl f e (A xs ys) = foldl f (foldl f e xs) ys

foldl' f e E = e
foldl' f e (L x) = e `seq` f e x
foldl' f e (A xs ys) = e `seq` foldl' f (foldl' f e xs) ys

foldr1 f E = error "JoinList.foldr1: empty sequence"
foldr1 f (L x) = x
foldr1 f (A xs ys) = foldr f (foldr1 f ys) xs

foldr1' f E = error "JoinLis.foldr1': empty sequence"
foldr1' f (L x) = x
foldr1' f (A xs ys) = foldr' f (foldr1' f ys) xs

foldl1 f E = error "JoinList.foldl1: empty sequence"
foldl1 f (L x) = x
foldl1 f (A xs ys) = foldl f (foldl1 f xs) ys

foldl1' f E = error "JoinList.foldl1': empty sequence"
foldl1' f (L x) = x
foldl1' f (A xs ys) = foldl' f (foldl1' f xs) ys

copy n x 
    | n <= 0 = E
    | otherwise = cpy n x
  where cpy n x  -- n > 0
          | even n = let xs = cpy (half n) x
                     in A xs xs
          | n == 1 = L x
          | otherwise = let xs = cpy (half n) x
                        in A (L x) (A xs xs)


strict s@E = s
strict s@(L x) = s
strict s@(A l r) = strict l `seq` strict r `seq` s

strictWith f s@E = s
strictWith f s@(L x) = f x `seq` s
strictWith f s@(A l r) = strictWith f l `seq` strictWith f l `seq` s

-- invariants:
--   * 'E' is never a child of 'A'

structuralInvariant E = True
structuralInvariant s = check s
  where check E = False
        check (L _) = True
        check (A s1 s2) = check s1 && check s2


concat = concatUsingFoldr
reverseOnto = reverseOntoUsingReverse
fromList = fromListUsingCons
concatMap = concatMapUsingFoldr

reducer  = reducerUsingReduce1
reducer' = reducer'UsingReduce1'
reducel  = reducelUsingReduce1
reducel' = reducel'UsingReduce1'
reduce1  = reduce1UsingLists
reduce1' = reduce1'UsingLists

inBounds = inBoundsUsingDrop
lookup = lookupUsingDrop
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingDrop

update = updateUsingSplitAt
adjust = adjustUsingSplitAt

mapWithIndex = mapWithIndexUsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists

take = takeUsingLview
drop = dropUsingLtail
splitAt = splitAtUsingLview
subseq = subseqDefault
        
filter = filterUsingLview
partition = partitionUsingFoldr
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview

zip = zipUsingLview
zip3 = zip3UsingLview
zipWith = zipWithUsingLview
zipWith3 = zipWith3UsingLview

unzip = unzipUsingFoldr
unzip3 = unzip3UsingFoldr
unzipWith = unzipWithUsingFoldr
unzipWith3 = unzipWith3UsingFoldr

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
   reducel' = reducel'; reduce1 = reduce1;  reduce1' = reduce1';
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
   structuralInvariant = structuralInvariant; instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = singleton
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  xs == ys = toList xs == toList ys

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = sized arbTree
    where arbTree 0 = return E
          arbTree 1 = liftM L arbitrary
          arbTree n =
            frequency [(1, liftM L arbitrary),
                       (4, liftM2 A (arbTree (n `div` 2))
                                    (arbTree (n `div` 2)))]

  coarbitrary E = variant 0
  coarbitrary (L x) = variant 1 . coarbitrary x
  coarbitrary (A xs ys) = variant 2 . coarbitrary xs . coarbitrary ys

instance Monoid (Seq a) where
  mempty  = empty
  mappend = append
