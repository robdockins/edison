-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Seq.Sequence (
    -- class definition + method wrappers
    module Data.Edison.Seq.Sequence

) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Control.Monad
import qualified Control.Monad.Identity as ID
import Data.Edison.Prelude

-- naming convention: instances of Sequence are named Seq whenever possible

class (Functor s, MonadPlus s) => Sequence s where
  -- in addition to Functor, Monad, and MonadPlus,
  -- sequences should also be instances of Eq and Show

----------------------------------------------------------------------
-- Constructors

  empty     :: s a
  single    :: a -> s a
    -- empty = <>
    -- single x = <x>

  lcons     :: a -> s a -> s a
  rcons     :: s a -> a -> s a
  append    :: s a -> s a -> s a
    -- cons x <x0,...,xn-1> = <x,x0,...,xn-1>
    -- snoc <x0,...,xn-1> x = <x0,...,xn-1,x>
    -- append <x0,...,xn-1> <y0,...,ym-1> = <x0,...,xn-1,y0,...,ym-1>

  fromList  :: [a] -> s a
    -- fromList [x0,...,xn-1] = <x0,...,xn-1>

  -- initialize a sequence
  copy      :: Int -> a -> s a          -- returns empty if size is negative
  -- copy n x = <x,...,x>  -- n copies

----------------------------------------------------------------------
-- Destructors

  -- view the left element
  lview     :: (Monad m) => s a -> m (a, s a)
  lhead     :: s a -> a                  -- signals error if sequence is empty
  lheadM    :: (Monad rm) => s a -> rm a -- fails if sequence is empty
  ltail     :: s a -> s a                -- signals error if sequence is empty
  ltailM    :: (Monad rm) => s a -> rm (s a) -- fails if sequence is empty

  lhead = ID.runIdentity . lheadM
  ltail = ID.runIdentity . ltailM

  -- view the right element
  rview     :: (Monad m) => s a -> m (s a, a)
  rhead     :: s a -> a                  -- signals error if sequence is empty
  rheadM    :: (Monad rm) => s a -> rm a -- fails if sequence is empty
  rtail     :: s a -> s a                -- signals error if sequence is empty
  rtailM    :: (Monad rm) => s a -> rm (s a) -- fails if sequence is empty

  rhead = ID.runIdentity . rheadM
  rtail = ID.runIdentity . rtailM

----------------------------------------------------------------------
-- Observers

  null      :: s a -> Bool
  size      :: s a -> Int
    -- null <x0,...,xn-1> = (n==0)
    -- size <x0,...,xn-1> = n

  toList    :: s a -> [a]
    -- toList <x0,...,xn-1> = [x0,...,xn-1]

----------------------------------------------------------------------
-- Concat and revers

  -- flattening a sequence
  concat    :: s (s a) -> s a
    -- concat xss = foldr append empty xss


  -- reversing a sequence
  reverse      :: s a -> s a
  reverseOnto  :: s a -> s a -> s a
    -- reverse <x0,...,xn-1> = <xn-1,...,x0>
    -- reverseOnto <x0,...,xn-1> <y0,...,ym-1> = <xn-1,...,x0,y0,...,ym-1>

----------------------------------------------------------------------
-- Maps and folds

  map        :: (a -> b) -> s a -> s b
  concatMap  :: (a -> s b) -> s a -> s b
    -- map f <x0,...,xn-1> = <f x0,...,f xn-1>
    -- concatMap f xs = concat (map f xs)

  foldr     :: (a -> b -> b) -> b -> s a -> b
  foldl     :: (b -> a -> b) -> b -> s a -> b
    -- foldr (+) c <x0,...,xn-1> = x0 + (x1 + ... + (xn-1 + c))
    -- foldl (+) c <x0,...,xn-1> = ((c + x0) + x1) + ... + xn-1

  foldr1    :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
  foldl1    :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
    -- foldr1 (+) <x0,...,xn-1>
    --   | n==0 = error "ModuleName.foldr1: empty sequence"
    --   | n>0  = x0 + (x1 + ... + xn-1)
    -- foldl1 (+) <x0,...,xn-1>
    --   | n==0 = error "ModuleName.foldl1: empty sequence"
    --   | n>0  = (x0 + x1) + ... + xn-1

  reducer   :: (a -> a -> a) -> a -> s a -> a
  reducel   :: (a -> a -> a) -> a -> s a -> a
  reduce1   :: (a -> a -> a) -> s a -> a  -- signals error if sequence is empty
    -- reduce is similar to fold, but combines elements in a balanced fashion
    -- the combining function should usually be associative
    --
    -- reducer (+) x xs = reduce1 (+) (cons x xs)
    -- reducel (+) x xs = reduce1 (+) (snoc xs x)
    --
    -- reduce1 (+) <x> = x
    -- reduce1 (+) <x0,...,xn-1> = 
    --     (reduce1 (+) <x0,...,xi>) + (reduce1 (+) <xi+1,...,xn-1>)
    --   for some i such that 0 <= i && i < n-1
    --
    -- Although the exact value of i is unspecified it tends toward n/2
    -- so that the depth of calls to + is at most logarithmic

----------------------------------------------------------------------
-- Subsequences

  take        :: Int -> s a -> s a
  drop        :: Int -> s a -> s a
  splitAt     :: Int -> s a -> (s a, s a)
    -- take i xs = fst (splitAt i xs)
    -- drop i xs = snd (splitAt i xs)
    --
    -- splitAt i xs
    --   | i < 0  = (<>           , <x0,...,xn-1>)
    --   | i < n  = (<x0,...,xi-1>, <xi,...,xn-1>)
    --   | i >= n = (<x0,...,xn-1>, <>           )
  
  subseq      :: Int -> Int -> s a -> s a
    -- args are index/length rather than start index/end index
    --
    -- subseq i len xs = take len (drop i xs)

----------------------------------------------------------------------
-- Predicate-based operations

  filter      :: (a -> Bool) -> s a -> s a
  partition   :: (a -> Bool) -> s a -> (s a, s a)
    -- filter p xs = foldr pcons empty xs
    --   where pcons x xs = if p x then cons x xs else xs
    --
    -- partition p xs = (filter p xs, filter (not . p) xs)

  takeWhile   :: (a -> Bool) -> s a -> s a
  dropWhile   :: (a -> Bool) -> s a -> s a
  splitWhile  :: (a -> Bool) -> s a -> (s a, s a)
    -- takeWhile p xs = fst (splitWhile p xs)
    -- dropWhile p xs = snd (splitWhile p xs)
    --
    -- splitWhile p <x0,...,xn-1> = (<x0,...,xi-1>, <xi,...,xn-1>)
    --   where i = min j such that p xj (or n if no such j)

----------------------------------------------------------------------
-- Index-based operations (zero-based)

  inBounds  :: s a -> Int -> Bool
    -- inBounds <x0,...,xn-1> i = (0 <= i && i < n)

  lookup    :: s a -> Int -> a         -- signals error if index out of bounds
  lookupM   :: (Monad m) => s a -> Int -> m a
  lookupWithDefault  :: a -> s a -> Int -> a
    -- lookup xs@<x0,...,xn-1> i 
    --   | inBounds xs = xi
    --   | otherwise = error "ModuleName.lookup: index out of bounds"
    -- lookupM xs@<x0,...,xn-1> i 
    --   | inBounds xs = Just xi
    --   | otherwise = Nothing
    -- lookupWithDefault d xs@<x0,...,xn-1> i 
    --   | inBounds xs = xi
    --   | otherwise = d

  lookup m k = ID.runIdentity (lookupM m k)

  update    :: Int -> a -> s a -> s a
  adjust    :: (a -> a) -> Int -> s a -> s a -- map a single element
    -- both return original sequence if index out of bounds
    --
    -- update i y xs@<x0,...,xn-1>
    --   | inBounds xs = <x0,...xi-1,y,xi+1,...,xn-1>
    --   | otherwise = xs
    -- adjust f i xs@<x0,...,xn-1>
    --   | inBounds xs = <x0,...xi-1,f xi,xi+1,...,xn-1>
    --   | otherwise = xs

  mapWithIndex    :: (Int -> a -> b) -> s a -> s b
  foldrWithIndex  :: (Int -> a -> b -> b) -> b -> s a -> b
  foldlWithIndex  :: (b -> Int -> a -> b) -> b -> s a -> b
    -- mapWithIndex f <x0,...,xn-1> = <f 0 x0,...,f (n-1) xn-1>
    -- foldrWithIndex f c <x0,...,xn-1> = 
    --   f 0 x0 (f 1 x1 (... (f (n-1) xn-1 c)))
    -- foldlWithIndex f c <x0,...,xn-1> =
    --   f (...(f (f c 0 x0) 1 x1)...) (n-1) xn-1)

----------------------------------------------------------------------
-- Zips and unzips

  zip         :: s a -> s b -> s (a,b)
  zip3        :: s a -> s b -> s c -> s (a,b,c)
    -- zip <x0,...,xn-1> <y0,...,ym-1> = <(x0,y0),...,(xj-1,yj-1)>
    --   where j = min {n,m}
    -- zip3 <x0,...,xn-1> <y0,...,ym-1> <z0,...,zk-1> = 
    --     <(x0,y0,z0),...,(xj-1,yj-1,zj-1)>
    --   where j = min {n,m,k}

  zipWith     :: (a -> b -> c) -> s a -> s b -> s c
  zipWith3    :: (a -> b -> c -> d) -> s a -> s b -> s c -> s d
    -- zipWith f xs ys = map (uncurry f) (zip xs ys)
    -- zipWith3 f xs ys zs = map (uncurry f) (zip3 xs ys zs)

  unzip       :: s (a,b) -> (s a, s b)
  unzip3      :: s (a,b,c) -> (s a, s b, s c)
    -- unzip xs = (map fst xs, map snd xs)
    -- unzip3 xs = (map fst3 xs, map snd3 xs, map thd3 xs)
    --   where fst3 (x,y,z) = x
    --         snd3 (x,y,z) = y
    --         thd3 (x,y,z) = z

  unzipWith   :: (a -> b) -> (a -> c) -> s a -> (s b, s c)
  unzipWith3  :: (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
    -- unzipWith f g xs = (map f xs, map g xs)
    -- unzipWith3 f g h xs = (map f xs, map g xs, map h xs)

----------------------------------------------------------------------
-- Documentation

  instanceName  :: s a -> String
    -- The name of the module implementing s.

----------------------------------------------------------------------
-- Other possible operations not currently included
{-
  insertAt :: Int -> a -> s a -> s a
    -- adds to front or rear if index out of bounds
    --
    -- insertAt i y xs@<x0,...,xn-1>
    --   | i < 0  = cons y xs
    --   | i >= n = snoc xs y
    --   | otherwise = <x0,...,xi-1,y,xi,...,xn-1> 

  deleteAt :: Int -> s a -> s a
    -- returns original sequence if index out of bounds
    --
    -- deleteAt i xs@<x0,...,xn-1>
    --   | i < 0  = xs
    --   | i >= n = xs
    --   | otherwise = <x0,...,xi-1,xi+1,...,xn-1> 

  insertAt i x s = append before (cons x after)
    where (before, after) = splitAt i s

  deleteAt i s = if i < 0 then s else append before (ltail after)
    where (before, after) = splitAt i s
-}

{-
class AsSequence t s | t -> s where
    asSequence :: t -> s

instance (Sequence s) => AsSequence s s where
    asSequence = id
-}
