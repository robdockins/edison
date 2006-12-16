-- |
--   Module      :  Data.Edison.Seq.Defaults
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  internal (unstable)
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   This module provides default implementations of many of
--   the sequence operations.  It is used to fill in implementations
--   and is not intended for end users.

module Data.Edison.Seq.Defaults where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Control.Monad.Identity
import Data.Char (isSpace)

import Data.Edison.Prelude
import Data.Edison.Seq
import qualified Data.Edison.Seq.ListSeq as L

rconsUsingAppend :: Sequence s => a -> s a -> s a
rconsUsingAppend x s = append s (singleton x)

rconsUsingFoldr :: Sequence s => a -> s a -> s a
rconsUsingFoldr x s = foldr lcons (singleton x) s

appendUsingFoldr :: Sequence s => s a -> s a -> s a
appendUsingFoldr s t | null t = s
                            | otherwise = foldr lcons t s

rviewDefault :: (Monad m, Sequence s) => s a -> m (a, s a)
rviewDefault xs
  | null xs   = fail $ instanceName xs ++ ".rview: empty sequence"
  | otherwise = return (rhead xs, rtail xs)


rtailUsingLview :: (Sequence s) => s a -> s a
rtailUsingLview xs = 
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".rtail: empty sequence"
      Just (x, xs) -> rt x xs
  where rt x xs =
          case lview xs of
            Nothing      -> empty
            Just (y, ys) -> lcons x (rt y ys)

rtailMUsingLview :: (Monad m,Sequence s) => s a -> m (s a)
rtailMUsingLview xs = 
    case lview xs of
      Nothing      -> fail $ instanceName xs ++ ".rtailM: empty sequence"
      Just (x, xs) -> return (rt x xs)
  where rt x xs =
          case lview xs of
            Nothing      -> empty
            Just (y, ys) -> lcons x (rt y ys)



concatUsingFoldr :: Sequence s => s (s a) -> s a
concatUsingFoldr = foldr append empty

reverseUsingReverseOnto :: Sequence s => s a -> s a
reverseUsingReverseOnto s = reverseOnto s empty

reverseUsingLists :: Sequence s => s a -> s a
reverseUsingLists = fromList . L.reverse . toList

reverseOntoUsingFoldl :: Sequence s => s a -> s a -> s a
reverseOntoUsingFoldl xs ys = foldl (flip lcons) ys xs

reverseOntoUsingReverse :: Sequence s => s a -> s a -> s a
reverseOntoUsingReverse = append . reverse

fromListUsingCons :: Sequence s => [a] -> s a
fromListUsingCons = L.foldr lcons empty

toListUsingFoldr :: Sequence s => s a -> [a]
toListUsingFoldr = foldr (:) []

mapUsingFoldr :: Sequence s => (a -> b) -> s a -> s b
mapUsingFoldr f = foldr (lcons . f) empty

concatMapUsingFoldr :: Sequence s => (a -> s b) -> s a -> s b
concatMapUsingFoldr f = foldr (append . f) empty

foldrUsingLists :: Sequence s => (a -> b -> b) -> b -> s a -> b
foldrUsingLists f e xs = L.foldr f e (toList xs)

foldr'UsingLists :: Sequence s => (a -> b -> b) -> b -> s a -> b
foldr'UsingLists f e xs = L.foldr' f e (toList xs)

foldlUsingLists :: Sequence s => (b -> a -> b) -> b -> s a -> b
foldlUsingLists f e xs = L.foldl f e (toList xs)

foldl'UsingLists :: Sequence s => (b -> a -> b) -> b -> s a -> b
foldl'UsingLists f e xs = L.foldl' f e (toList xs)

foldr1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldr1UsingLists f xs = L.foldr1 f (toList xs)

foldr1'UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldr1'UsingLists f xs = L.foldr1' f (toList xs)

foldl1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldl1UsingLists f xs = L.foldl1 f (toList xs)

foldl1'UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
foldl1'UsingLists f xs = L.foldl1' f (toList xs)

fold1UsingFold :: Sequence s => (a -> a -> a) -> s a -> a
fold1UsingFold f xs =
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".fold1: empty sequence"
      Just (x, xs) -> fold f x xs

fold1'UsingFold' :: Sequence s => (a -> a -> a) -> s a -> a
fold1'UsingFold' f xs =
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".fold1': empty sequence"
      Just (x, xs) -> fold' f x xs

foldr1UsingLview :: Sequence s => (a -> a -> a) -> s a -> a
foldr1UsingLview f xs = 
    case lview xs of
      Nothing      -> error $ instanceName xs ++ ".foldr1: empty sequence"
      Just (x, xs) -> fr1 x xs
  where fr1 x xs =
          case lview xs of
            Nothing     -> x
            Just (y,ys) -> f x (fr1 y ys)

foldr1'UsingLview :: Sequence s => (a -> a -> a) -> s a -> a
foldr1'UsingLview f xs =
     case lview xs of
        Nothing     -> error $ instanceName xs ++ ".foldr1': empty sequence"
        Just (x,xs) -> fr1 x xs
  where fr1 x xs = 
          case lview xs of
             Nothing     -> x
             Just (y,ys) -> f x $! (fr1 y ys)

foldl1UsingFoldl :: Sequence s => (a -> a -> a) -> s a -> a
foldl1UsingFoldl f xs = 
    case lview xs of
      Nothing     -> error $ instanceName xs ++ ".foldl1: empty sequence"
      Just (x,xs) -> foldl f x xs

foldl1'UsingFoldl' :: Sequence s => (a -> a -> a) -> s a -> a
foldl1'UsingFoldl' f xs =
    case lview xs of
      Nothing     -> error $ instanceName xs ++ ".foldl1': empty sequence"
      Just (x,xs) -> foldl' f x xs

reducerUsingReduce1 :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducerUsingReduce1 f e s
  | null s = e
  | otherwise = f (reduce1 f s) e

reducer'UsingReduce1' :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducer'UsingReduce1' f e s
  | null s = e
  | otherwise = f (reduce1' f s) e

reducelUsingReduce1 :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducelUsingReduce1 f e s
  | null s = e
  | otherwise = f e (reduce1 f s)

reducel'UsingReduce1' :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducel'UsingReduce1' f e s
  | null s = e
  | otherwise = f e (reduce1' f s)

reduce1UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
reduce1UsingLists f s = L.reduce1 f (toList s)

reduce1'UsingLists :: Sequence s => (a -> a -> a) -> s a -> a
reduce1'UsingLists f s = L.reduce1' f (toList s)

copyUsingLists :: Sequence s => Int -> a -> s a
copyUsingLists n x = fromList (L.copy n x)


inBoundsUsingDrop :: Sequence s => Int -> s a -> Bool
inBoundsUsingDrop i s = 
  i >= 0 && not (null (drop i s))

inBoundsUsingLookupM :: Sequence s => Int -> s a -> Bool
inBoundsUsingLookupM i s =
  case lookupM i s of
    Just x  -> True
    Nothing -> False

inBoundsUsingSize :: Sequence s => Int -> s a -> Bool
inBoundsUsingSize i s = i >= 0 && i < size s

lookupUsingLookupM :: Sequence s => Int -> s a -> a
lookupUsingLookupM i s = runIdentity (lookupM i s)

lookupUsingDrop :: Sequence s => Int -> s a -> a
lookupUsingDrop i s
  | i < 0 || null s' = error $ instanceName s ++ ".lookup: bad subscript"
  | otherwise = lhead s'
  where s' = drop i s

lookupWithDefaultUsingLookupM :: Sequence s => a -> Int -> s a -> a
lookupWithDefaultUsingLookupM d i s =
  case lookupM i s of
    Nothing -> d
    Just x  -> x

lookupWithDefaultUsingDrop :: Sequence s => a -> Int -> s a -> a
lookupWithDefaultUsingDrop d i s
  | i < 0 || null s' = d
  | otherwise = lhead s'
  where s' = drop i s

lookupMUsingDrop :: (Monad m, Sequence s) => Int -> s a -> m a
lookupMUsingDrop i s
  -- XXX better error message!
  | i < 0 || null s' = fail $ instanceName s
  			++ ".lookupMUsingDrop: empty sequence"
  | otherwise        = return (lhead s')
  where s' = drop i s

filterUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingLview p xs =
  case lview xs of
    Nothing     -> empty
    Just (x,xs) -> if p x then lcons x (filter p xs) else filter p xs

filterUsingLists :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingLists p xs =
  fromList (L.filter p (toList xs))

filterUsingFoldr :: Sequence s => (a -> Bool) -> s a -> s a
filterUsingFoldr p = foldr pcons empty
  where pcons x xs = if p x then lcons x xs else xs

partitionUsingLists :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
partitionUsingLists p xs =
  let (ys,zs) = L.partition p (toList xs)
  in (fromList ys, fromList zs)

partitionUsingFoldr :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
partitionUsingFoldr p = foldr pcons (empty, empty)
  where pcons x (xs, xs') = if p x then (lcons x xs, xs') else (xs, lcons x xs')

updateUsingAdjust :: Sequence s => Int -> a -> s a -> s a
updateUsingAdjust i y = adjust (const y) i

updateUsingSplitAt :: Sequence s => Int -> a -> s a -> s a
updateUsingSplitAt i x xs
  | i < 0 = xs
  | otherwise = let (ys,zs) = splitAt i xs
                in if null zs then xs else append ys (lcons x (ltail zs))

adjustUsingLists :: Sequence s => (a -> a) -> Int -> s a -> s a
adjustUsingLists f i xs = fromList (L.adjust f i (toList xs))

adjustUsingSplitAt :: Sequence s => (a -> a) -> Int -> s a -> s a
adjustUsingSplitAt f i xs
  | i < 0 = xs
  | otherwise = let (ys,zs) = splitAt i xs
                in case lview zs of
                     Nothing      -> xs
                     Just (z,zs') -> append ys (lcons (f z) zs')

{-
insertAtUsingLists :: Sequence s => Int -> a -> s a -> s a
insertAtUsingLists i x xs = 
  fromList (L.insertAt i x (toList xs))

insertAtUsingSplitAt :: Sequence s => Int -> a -> s a -> s a
insertAtUsingSplitAt i x xs
  | (xs_before, xs_after) <- splitAt i xs =
      append xs_before (lcons x xs_after)

deleteAtUsingLists :: Sequence s => Int -> s a -> s a
deleteAtUsingLists i xs = fromList (L.deleteAt i (toList xs))

deleteAtUsingSplitAt :: Sequence s => Int -> s a -> s a
deleteAtUsingSplitAt i xs
  | (xs_before, xs_after) <- splitAt i xs =
      append xs_before (ltail xs_after)
-}

mapWithIndexUsingLists :: Sequence s => (Int -> a -> b) -> s a -> s b
mapWithIndexUsingLists f xs = fromList (L.mapWithIndex f (toList xs))

foldrWithIndexUsingLists :: 
  Sequence s => (Int -> a -> b -> b) -> b -> s a -> b
foldrWithIndexUsingLists f e xs = L.foldrWithIndex f e (toList xs)

foldrWithIndex'UsingLists :: 
  Sequence s => (Int -> a -> b -> b) -> b -> s a -> b
foldrWithIndex'UsingLists f e xs = L.foldrWithIndex' f e (toList xs)

foldlWithIndexUsingLists :: 
  Sequence s => (b -> Int -> a -> b) -> b -> s a -> b
foldlWithIndexUsingLists f e xs = L.foldlWithIndex f e (toList xs)

foldlWithIndex'UsingLists :: 
  Sequence s => (b -> Int -> a -> b) -> b -> s a -> b
foldlWithIndex'UsingLists f e xs = L.foldlWithIndex' f e (toList xs)

takeUsingLists :: Sequence s => Int -> s a -> s a
takeUsingLists i s = fromList (L.take i (toList s))

takeUsingLview :: Sequence s => Int -> s a -> s a
takeUsingLview i xs
  | i <= 0 = empty
  | otherwise = case lview xs of
                  Nothing -> empty
                  Just (x,xs') -> lcons x (take (i-1) xs')

dropUsingLists :: Sequence s => Int -> s a -> s a
dropUsingLists i s = fromList (L.drop i (toList s))

dropUsingLtail :: Sequence s => Int -> s a -> s a
dropUsingLtail i xs
  | i <= 0 || null xs = xs
  | otherwise = dropUsingLtail (i-1) (ltail xs)

splitAtDefault :: Sequence s => Int -> s a -> (s a, s a)
splitAtDefault i s = (take i s, drop i s)

splitAtUsingLview :: Sequence s => Int -> s a -> (s a, s a)
splitAtUsingLview i xs
  | i <= 0 = (empty,xs)
  | otherwise = case lview xs of
                  Nothing      -> (empty,empty)
                  Just (x,xs') -> (lcons x ys,zs)
                    where (ys,zs) = splitAtUsingLview (i-1) xs'

subseqDefault :: Sequence s => Int -> Int -> s a -> s a
subseqDefault i len xs = take len (drop i xs)

takeWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
takeWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> lcons x (takeWhileUsingLview p xs')
    _                  -> empty

dropWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> s a
dropWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> dropWhileUsingLview p xs'
    _                  -> xs

splitWhileUsingLview :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
splitWhileUsingLview p xs =
  case lview xs of
    Just (x,xs') | p x -> let (front, back) = splitWhileUsingLview p xs'
                          in (lcons x front, back)
    _                  -> (empty, xs)

zipUsingLview :: Sequence s => s a -> s b -> s (a,b)
zipUsingLview xs ys =
  case lview xs of
    Nothing -> empty
    Just (x,xs') ->
      case lview ys of
        Nothing -> empty
        Just (y,ys') -> lcons (x,y) (zipUsingLview xs' ys')

zip3UsingLview :: Sequence s => s a -> s b -> s c -> s (a,b,c)
zip3UsingLview xs ys zs =
  case lview xs of
    Nothing -> empty
    Just (x,xs') ->
      case lview ys of
        Nothing -> empty
        Just (y,ys') ->
          case lview zs of
            Nothing -> empty
            Just (z,zs') -> lcons (x,y,z) (zip3UsingLview xs' ys' zs')

zipWithUsingLview :: Sequence s => (a -> b -> c) -> s a -> s b -> s c
zipWithUsingLview f xs ys =
  case lview xs of
    Nothing -> empty
    Just (x,xs') ->
      case lview ys of
        Nothing -> empty
        Just (y,ys') -> lcons (f x y) (zipWithUsingLview f xs' ys')

zipWith3UsingLview :: 
  Sequence s => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3UsingLview f xs ys zs =
  case lview xs of
    Nothing -> empty
    Just (x,xs') ->
      case lview ys of
        Nothing -> empty
        Just (y,ys') ->
          case lview zs of
            Nothing -> empty
            Just (z,zs') -> lcons (f x y z) (zipWith3UsingLview f xs' ys' zs')

zipUsingLists :: Sequence s => s a -> s b -> s (a,b)
zipUsingLists xs ys = fromList (L.zip (toList xs) (toList ys))

zip3UsingLists :: Sequence s => s a -> s b -> s c -> s (a,b,c)
zip3UsingLists xs ys zs = 
  fromList (L.zip3 (toList xs) (toList ys) (toList zs))

zipWithUsingLists :: Sequence s => (a -> b -> c) -> s a -> s b -> s c
zipWithUsingLists f xs ys =
  fromList (L.zipWith f (toList xs) (toList ys))

zipWith3UsingLists :: 
  Sequence s => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3UsingLists f xs ys zs =
  fromList (L.zipWith3 f (toList xs) (toList ys) (toList zs))

unzipUsingLists :: Sequence s => s (a,b) -> (s a, s b)
unzipUsingLists xys =
  case L.unzip (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

unzipUsingFoldr :: Sequence s => s (a,b) -> (s a, s b)
unzipUsingFoldr = foldr pcons (empty,empty) 
  where pcons (x,y) (xs,ys) = (lcons x xs, lcons y ys)

unzip3UsingLists :: Sequence s => s (a,b,c) -> (s a, s b, s c)
unzip3UsingLists xyzs =
  case L.unzip3 (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

unzip3UsingFoldr :: Sequence s => s (a,b,c) -> (s a, s b, s c)
unzip3UsingFoldr = foldr tcons (empty,empty,empty)
  where tcons (x,y,z) (xs,ys,zs) = (lcons x xs, lcons y ys, lcons z zs)

unzipWithUsingLists :: 
  Sequence s => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWithUsingLists f g xys =
  case L.unzipWith f g (toList xys) of
    (xs, ys) -> (fromList xs, fromList ys)

unzipWithUsingFoldr :: 
  Sequence s => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWithUsingFoldr f g = foldr pcons (empty,empty) 
  where pcons e (xs,ys) = (lcons (f e) xs, lcons (g e) ys)

unzipWith3UsingLists :: 
  Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3UsingLists f g h xyzs =
  case L.unzipWith3 f g h (toList xyzs) of
    (xs, ys, zs) -> (fromList xs, fromList ys, fromList zs)

unzipWith3UsingFoldr :: 
  Sequence s => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3UsingFoldr f g h = foldr tcons (empty,empty,empty) 
  where tcons e (xs,ys,zs) = (lcons (f e) xs, lcons (g e) ys, lcons (h e) zs)

showsPrecUsingToList :: (Show a,Sequence s) => Int -> s a -> ShowS
showsPrecUsingToList i xs rest
   | i == 0    = concat [    instanceName xs,".fromList "] ++ showsPrec 10 (toList xs) rest
   | otherwise = concat ["(",instanceName xs,".fromList "] ++ showsPrec 10 (toList xs) (')':rest)

readsPrecUsingFromList :: (Read a,Sequence s) => Int -> ReadS (s a)
readsPrecUsingFromList i xs =
   let result = maybeParens p xs
       p xs = tokenMatch ((instanceName x)++".fromList") xs
                >>= readsPrec 10
                >>= \(l,rest) -> return (fromList l,rest)

       -- play games with the typechecker so we don't have to use
       -- extensions for scoped type variables
       ~[(x,_)] = result

   in result

defaultCompare :: (Ord a, Sequence s) => s a -> s a -> Ordering
defaultCompare a b =
   case (lview a, lview b) of
     (Nothing, Nothing) -> EQ
     (Nothing, _      ) -> LT
     (_      , Nothing) -> GT
     (Just (x,xs), Just (y,ys)) ->
	case compare x y of
           EQ -> defaultCompare xs ys
           c -> c


dropMatch :: (Eq a,MonadPlus m) => [a] -> [a] -> m [a]
dropMatch [] ys = return ys
dropMatch (x:xs) (y:ys)
    | x == y    = dropMatch xs ys
    | otherwise = mzero
dropMatch _ _   = mzero

tokenMatch :: MonadPlus m => String -> String -> m String
tokenMatch token str = dropMatch token (munch str) >>= return . munch
   where munch = dropWhile isSpace

readSParens :: ReadS a -> ReadS a
readSParens p xs = return xs
     >>= tokenMatch "("
     >>= p
     >>= \(x,xs') -> return xs'
     >>= tokenMatch ")"
     >>= \rest -> return (x,rest)

maybeParens :: ReadS a -> ReadS a
maybeParens p xs = readSParens p xs `mplus` p xs
