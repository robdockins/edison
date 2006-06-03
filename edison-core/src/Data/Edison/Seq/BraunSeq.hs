-- |
--   Module      :  Data.Edison.Seq.BraunSeq
--   Copyright   :  Copyright (c) 1998-1999 Chris Okasaki
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   One-sided Braun sequences.  All running times are as listed in 
--   "Data.Edison.Seq" except the following:
--
--   * lview, lcons, ltail*   @O( log n )@
--
--   * rcons, rview, rhead*, rtail*, size   @O( log^2 n )@
--
--   * copy, inBounds, lookup*, update, adjust  @O( log i )@
--
--   * append            @O( n1 log n2 )@
--
--   * concat            @O( n + m log m )@
--
--   * drop, splitAt     @O( i log n )@
--
--   * subseq            @O( i log n + len )@
--
--   * reverseOnto       @O( n1 log n2 )@
--
--   * concatMap, (>>=)  @O( n * t + m log m )@, where @n@ is the length of the input sequence
--                                               @m@ is the length of the output sequence and @t@
--                                               is the running time of @f@
--
--   By keeping track of the size, we could get rcons, rview, rhead*, and rtail*
--   down to @O(log n)@ as well; furthermore, size would be @O( 1 )@.
--
--   /References:/
--
--   * Rob Hoogerwoord. \"A symmetric set of efficient list operations\".
--     /Journal of Functional Programming/, 2(4):505--513, 1992.
--
--   * Rob Hoogerwoord. \"A Logarithmic Implementation of Flexible Arrays\".
--     /Mathematics of Program Construction/ (MPC'92), pages 191-207.
--
--   * Chris Okasaki. \"Three algorithms on Braun Trees\".  
--     /Journal of Function Programming/ 7(6):661-666. Novemebr 1997.

module Data.Edison.Seq.BraunSeq (
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

import Control.Monad
import Control.Monad.Identity
import Data.Maybe
import Data.Monoid
import Test.QuickCheck

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S ( Sequence(..) )
import Data.Edison.Seq.Defaults
import qualified Data.Edison.Seq.ListSeq as L


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

moduleName = "Data.Edison.Seq.BraunSeq"


data Seq a = E | B a (Seq a) (Seq a)    deriving (Eq)

half :: Int -> Int
half n = n `quot` 2  -- use a shift?

empty = E
singleton x = B x E E

lcons x E = singleton x
lcons x (B y a b) = B x (lcons y b) a

rcons y ys = insAt (size ys) ys
  where insAt 0 _ = singleton y
        insAt i (B x a b)
          | odd i     = B x (insAt (half i) a) b
          | otherwise = B x a (insAt (half i - 1) b)
        insAt _ _ = error "BraunSeq.rcons: bug.  Impossible case!"

append xs E = xs
append xs ys = app (size xs) xs ys
  where app 0 xs ys = ys
        app n xs E = xs
        app n (B x a b) (B y c d)
            | odd n     = B x (app m a (lcons y d)) (app m b c)
            | otherwise = B x (app m a c) (app (m-1) b (lcons y d))
          where m = half n
        app _ _ _ = error "BraunSeq.append: bug!"
  -- how does it compare to converting to/from lists?

lview E = fail "BraunSeq.lview: empty sequence"
lview (B x a b) = return (x, combine a b)

-- not exported
combine E _ = E
combine (B x a b) c = B x c (combine a b)

lhead E = error "BraunSeq.lhead: empty sequence"
lhead (B x a b) = x

lheadM E = fail "BraunSeq.lheadM: empty sequence"
lheadM (B x a b) = return x

ltail E = error "BraunSeq.ltail: empty sequence"
ltail (B x a b) = combine a b

ltailM E = fail "BraunSeq.ltailM: empty sequence"
ltailM (B x a b) = return (combine a b)

-- not exported
-- precondition: i >= 0
delAt 0 _ = E
delAt i (B x a b)
  | odd i     = B x (delAt (half i) a) b
  | otherwise = B x a (delAt (half i - 1) b)
delAt _ _ = error "BraunSeq.delAt: bug.  Impossible case!"

rview E = fail "BraunSeq.rview: empty sequence"
rview xs = return (lookup m xs, delAt m xs)
  where m = size xs - 1

rhead E = error "BraunSeq.rhead: empty sequence"
rhead xs = lookup (size xs - 1) xs

rheadM E = fail  "BraunSeq.rheadM: empty sequence"
rheadM xs = return (lookup (size xs - 1) xs)

rtail E = error "BraunSeq.rtail: empty sequence"
rtail xs = delAt (size xs - 1) xs

rtailM E = fail "BraunSeq.rtailM: empty sequence"
rtailM xs = return (delAt (size xs - 1) xs)

null E = True
null _ = False

size E = 0
size (B x a b) = 1 + n + n + diff n a
  where n = size b

        diff 0 E = 0
        diff 0 (B x a b) = 1
        diff i (B x a b)
          | odd i     = diff (half i) a
          | otherwise = diff (half i - 1) b
        diff _ _ = error "BraunSeq.size: bug. Impossible case in diff!"

reverse xs = rev00 (size xs) xs
  where
    rev00 n xs
      | n <= 1 = xs
    rev00 n (B x a b)
      | odd n     = let a'      = rev00 m a
                        (x',b') = rev11 m x b      in B x' a' b'
      | otherwise = let (x',a') = rev01 m a
                        b'      = rev10 (m-1) x b  in B x' b' a'
      where m = half n
    rev00 _ _ = error "BraunSeq.reverse: bug!"

    rev11 n x E = (x,E)
    rev11 n x (B y a b)
      | odd n     = let (x',a') = rev11 m x a
                        (y',b') = rev11 m y b      in (y', B x' b' a')
      | otherwise = let (x',a') = rev11 m x a
                        (y',b') = rev11 (m-1) y b  in (x', B y' a' b')
      where m = half n

    rev01 n E = error "BraunSeq.reverse: bug!"
    rev01 n (B x a b)
      | n == 1    = (x, E)
      | odd n     = let (y',a') = rev01 m a
                        (x',b') = rev11 m x b      in (x', B y' b' a')
      | otherwise = let (y',a') = rev01 m a
                        (x',b') = rev11 (m-1) x b  in (y', B x' a' b')
      where m = half n

    rev10 n x E = B x E E
    rev10 n x (B y a b)
      | odd n     = let a'      = rev10 m x a
                        (y',b') = rev11 m y b      in B y' a' b'
      | otherwise = let (x',a') = rev11 m x a
                        b'      = rev10 (m-1) y b  in B x' b' a'
      where m = half n

fromList = L.lhead . L.foldr build [E] . rows 1
  where rows k [] = []
        rows k xs = (k, ys) : rows (k+k) zs
          where (ys,zs) = L.splitAt k xs

        build (k,xs) ts = zipWithB xs ts1 ts2
          where (ts1, ts2) = L.splitAt k ts

        zipWithB [] _ _ = []
        zipWithB (x:xs) [] _ = singleton x : L.map singleton xs
        zipWithB (x:xs) (t:ts) [] = B x t E : zipWithB xs ts []
        zipWithB (x:xs) (t1:ts1) (t2:ts2) = B x t1 t2 : zipWithB xs ts1 ts2

toList E = []
toList t = tol [t]
  where tol [] = []
        tol ts = xs ++ tol (ts1 ++ ts2)
          where xs = L.map root ts
                (ts1,ts2) = children ts

                children [] = ([],[])
                children (B x E _ : ts) = ([],[])
                children (B x a E : ts) = (a : leftChildren ts, [])
                children (B x a b : ts) = (a : ts1, b : ts2)
                  where (ts1, ts2) = children ts
                children _ = error "BraunSeq.toList: bug!"

                leftChildren [] = []
                leftChildren (B x E _ : ts) = []
                leftChildren (B x a b : ts) = a : leftChildren ts
                leftChildren _ = error "BraunSeq.toList: bug!"

                root (B x a b) = x
                root _ = error "BraunSeq.toList: bug!"

                left (B x a b) = a
                left _ = error "BraunSeq.toList: bug!"

map f E = E
map f (B x a b) = B (f x) (map f a) (map f b)

copy n x = if n <= 0 then empty else fst (copy2 n)
  where copy2 n
            | odd n     = (B x a a, B x b a)
            | n == 0    = (E, singleton x)
            | otherwise = (B x b a, B x b b)
          where (a, b) = copy2 (half (n-1))

inBounds i xs = (i >= 0) && inb xs i
  where inb E i = False
        inb (B x a b) i
          | odd i     = inb a (half i)
          | i == 0    = True
          | otherwise = inb b (half i - 1)

lookup i xs = runIdentity (lookupM i xs)

lookupM i xs
  | i < 0     = fail "BraunSeq.lookupM: bad subscript"
  | otherwise = look xs i
  where look E i = nothing
        look (B x a b) i
          | odd i     = look a (half i)
          | i == 0    = return x
          | otherwise = look b (half i - 1)
        nothing = fail "BraunSeq.lookupM: not found"

lookupWithDefault d i xs = if i < 0 then d
                           else look xs i
  where look E i = d
        look (B x a b) i
          | odd i     = look a (half i)
          | i == 0    = x
          | otherwise = look b (half i - 1)

update i y xs = if i < 0 then xs else upd i xs
  where upd i E = E
        upd i (B x a b)
          | odd i     = B x (upd (half i) a) b
          | i == 0    = B y a b
          | otherwise = B x a (upd (half i - 1) b)

adjust f i xs = if i < 0 then xs else adj i xs
  where adj i E = E
        adj i (B x a b)
          | odd i     = B x (adj (half i) a) b
          | i == 0    = B (f x) a b
          | otherwise = B x a (adj (half i - 1) b)

mapWithIndex f xs = mwi 0 1 xs
  where mwi i d E = E
        mwi i d (B x a b) = B (f i x) (mwi (i+d) dd a) (mwi (i+dd) dd b)
          where dd = d+d

take n xs = if n <= 0 then E else ta n xs
  where ta n E = E
        ta n (B x a b)
            | odd n     = B x (ta m a) (ta m b)
            | n == 0    = E
            | otherwise = B x (ta m a) (ta (m-1) b)
          where m = half n

drop n xs = if n <= 0 then xs else dr n xs
  where dr n E = E
        dr n t@(B x a b)
            | odd n     = combine (dr m a) (dr m b)
            | n == 0    = t
            | otherwise = combine (dr (m-1) b) (dr m a)
          where m = half n

zip (B x a b) (B y c d) = B (x,y) (zip a c) (zip b d)
zip _ _ = E

zip3 (B x a b) (B y c d) (B z e f) = B (x,y,z) (zip3 a c e) (zip3 b d f)
zip3 _ _ _ = E

zipWith f (B x a b) (B y c d) = B (f x y) (zipWith f a c) (zipWith f b d)
zipWith f _ _ = E

zipWith3 fn (B x a b) (B y c d) (B z e f) = 
    B (fn x y z) (zipWith3 fn a c e) (zipWith3 fn b d f)
zipWith3 fn _ _ _ = E

unzip E = (E, E)
unzip (B (x,y) a b) = (B x a1 b1, B y a2 b2)
  where (a1,a2) = unzip a
        (b1,b2) = unzip b

unzip3 E = (E, E, E)
unzip3 (B (x,y,z) a b) = (B x a1 b1, B y a2 b2, B z a3 b3)
  where (a1,a2,a3) = unzip3 a
        (b1,b2,b3) = unzip3 b

unzipWith f g E = (E, E)
unzipWith f g (B x a b) = (B (f x) a1 b1, B (g x) a2 b2)
  where (a1,a2) = unzipWith f g a
        (b1,b2) = unzipWith f g b

unzipWith3 f g h E = (E, E, E)
unzipWith3 f g h (B x a b) = (B (f x) a1 b1, B (g x) a2 b2, B (h x) a3 b3)
  where (a1,a2,a3) = unzipWith3 f g h a
        (b1,b2,b3) = unzipWith3 f g h b


strict s@E = s
strict s@(B x l r) = strict l `seq` strict r `seq` s

strictWith f s@E = s
strictWith f s@(B x l r) = f x `seq` strictWith f l `seq` strictWith f r `seq` s

-- invariants:
--   * Left subtree is exactily the same size as the right
--     subtree, or one element larger

structuralInvariant E         = True
structuralInvariant (B _ l r) = isJust (check l r)

  where check E           E           = Just 1
        check (B _ E E)   E           = Just 2
	check (B _ l1 l2) (B _ r1 r2) = do
           x <- check l1 l2
           y <- check r1 r2
           if (x == y) || (x == y + 1)
              then return (x+y+1)
              else fail "unbalanced tree"
        check _ _ = fail "unbalanced tree"


-- the remaining functions all use defaults

concat = concatUsingFoldr
reverseOnto = reverseOntoUsingReverse
concatMap = concatMapUsingFoldr
fold = foldrUsingLists
fold' f = foldl'UsingLists (flip f)
fold1 = fold1UsingFold
fold1' = fold1'UsingFold'
foldr = foldrUsingLists
foldr' = foldr'UsingLists
foldl = foldlUsingLists
foldl' = foldl'UsingLists
foldr1 = foldr1UsingLists
foldr1' = foldr1'UsingLists
foldl1 = foldl1UsingLists
foldl1' = foldl1UsingLists
reducer = reducerUsingReduce1
reducer' = reducer'UsingReduce1'
reducel = reducelUsingReduce1
reducel' = reducel'UsingReduce1'
reduce1 = reduce1UsingLists
reduce1' = reduce1'UsingLists
foldrWithIndex  = foldrWithIndexUsingLists
foldrWithIndex' = foldrWithIndex'UsingLists
foldlWithIndex  = foldlWithIndexUsingLists
foldlWithIndex' = foldlWithIndex'UsingLists
splitAt = splitAtDefault
subseq = subseqDefault
filter = filterUsingLists
partition = partitionUsingLists
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview


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
   structuralInvariant = structuralInvariant; instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = singleton
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

-- instance Eq (Seq a) is derived

instance Ord a => Ord (Seq a) where
  compare = defaultCompare

instance Show a => Show (Seq a) where
  showsPrec = showsPrecUsingToList

instance Read a => Read (Seq a) where
  readsPrec = readsPrecUsingFromList

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = arbitrary >>= (return . fromList)
  coarbitrary xs = coarbitrary (toList xs)

instance Monoid (Seq a) where
  mempty  = empty
  mappend = append
