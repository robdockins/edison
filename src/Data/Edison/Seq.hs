-- Copyright (c) 1998-1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

{- | The sequence abstraction is usually viewed as a hierarchy of ADTs
     including lists, queues, deques, catenable lists, etc. However, such
     a hierarchy is based on efficiency rather than functionality. For example,
     a list supports all the operations that a deque supports, even though
     some of the operations may be inefficient. Hence, in Edison, all sequence
     data structures are defined as instances of the single Sequence class:

     @   class (Functor s, MonadPlus s) => Sequence s@

     All sequences are also instances of 'Functor', 'Monad', and 'MonadPlus'.
     In addition, all sequences are expected to be instances of 'Eq' and 'Show',
     although this is not enforced (in fact, is not enforceable in any 
     reasonable way).

     We follow the naming convention that every module implementing sequences
     defines a type constructor named Seq.
-}
module Data.Edison.Seq (
  Sequence (..)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Control.Monad
import qualified Control.Monad.Identity as ID
import Data.Edison.Prelude

-- naming convention: instances of Sequence are named Seq whenever possible

-- | The 'Sequence' class defines an interface for datatypes which
--   implement sequences.  The specification for each function is given
--   as psudeocode below, except for a few obvious cases.
--   
--   Sequences are represented in psudecode between angle brackets:
--
-- > <x0,x1,x2...,xn-1>
--
--   Such that @x0@ is at the left or front of the sequence and
--   @xn-1@ is at the right or rear of the sequence.

class (Functor s, MonadPlus s) => Sequence s where
  -- in addition to Functor, Monad, and MonadPlus,
  -- sequences should also be instances of Eq and Show

----------------------------------------------------------------------
-- Constructors

  -- | The empty sequence
  -- 
  -- > empty = <>
  empty     :: s a

  -- | Create a singleton sequence
  --
  -- > single x = <x>
  single    :: a -> s a

  -- | Add a new element to the front\/left of a sequence
  --
  -- > lcons x <x0,...,xn-1> = <x,x0,...,xn-1>
  lcons     :: a -> s a -> s a

  -- | Add a new element to the right\/rear of a sequence
  --
  -- > rcons <x0,...,xn-1> x = <x0,...,xn-1,x>
  rcons     :: s a -> a -> s a

  -- | Append two sequence, with the first argument on the left
  --   and the second argument on the right.
  -- 
  -- > append <x0,...,xn-1> <y0,...,ym-1> = <x0,...,xn-1,y0,...,ym-1>
  append    :: s a -> s a -> s a

  -- | Convert a list into a sequence
  -- 
  -- > fromList [x0,...,xn-1] = <x0,...,xn-1>
  fromList  :: [a] -> s a

  -- | Create a sequence containing @n@ copies of the given element.
  --   Return 'empty' if @n\<0@.
  --
  --   @copy n x = \<x,...,x>@
  copy      :: Int -> a -> s a          -- returns empty if size is negative

----------------------------------------------------------------------
-- Destructors

  -- | Separate a sequence into its first (leftomst) element and the
  --   remaining sequence.  Calls 'fail' if the sequence is empty.
  lview     :: (Monad m) => s a -> m (a, s a)

  -- | Return the first element of a sequence.
  --   Signals an error if thesequence is empty.
  lhead     :: s a -> a

  -- | Returns the first element of a sequence. 
  --   Calls 'fail' if the sequence is empty.
  lheadM    :: (Monad m) => s a -> m a 

  -- | Delete the first element of the sequence.
  --   Signals error if sequence is empty.
  ltail     :: s a -> s a

  -- | Delete the first element of the sequence.
  --   Calls 'fail' if the sequence is empty.
  ltailM    :: (Monad m) => s a -> m (s a)

  lhead = ID.runIdentity . lheadM
  ltail = ID.runIdentity . ltailM

  -- | Separate a sequence into its last (rightmost) element and the
  --   remaining sequence.  Calls 'fail' if the sequence is empty.
  rview     :: (Monad m) => s a -> m (s a, a)

  -- | Return the last (rightmost) element of the sequence.
  --   Signals error if sequence is empty.
  rhead     :: s a -> a 

  -- | Returns the last element of the sequence.
  --    Calls 'fail' if the sequence is empty.
  rheadM    :: (Monad m) => s a -> m a

  -- | Delete the last (rightmost) element of the sequence.
  --   Signals an error if the sequence is empty.
  rtail     :: s a -> s a

  -- | Delete the last (rightmost) element of the sequence.
  --   Calls 'fail' of the sequence is empty
  rtailM    :: (Monad m) => s a -> m (s a)

  rhead = ID.runIdentity . rheadM
  rtail = ID.runIdentity . rtailM

----------------------------------------------------------------------
-- Observers

  -- | Returns 'True' if the sequence is empty and 'False' otherwise.
  -- 
  -- > null <x0,...,xn-1> = (n==0)
  null      :: s a -> Bool

  -- | Returns the length of a sequence.
  --
  -- > size <x0,...,xn-1> = n
  size      :: s a -> Int

  -- | Convert a sequence to a list.
  --
  -- > toList <x0,...,xn-1> = [x0,...,xn-1]
  toList    :: s a -> [a]

----------------------------------------------------------------------
-- Concat and revers

  -- | Flatten a sequence of sequences into a simple sequence.
  --
  -- > concat xss = foldr append empty xss
  concat    :: s (s a) -> s a

  -- | Referse the order of a sequence
  --
  -- > reverse <x0,...,xn-1> = <xn-1,...,x0>
  reverse      :: s a -> s a

  -- | Reverse a sequence onto the front of another sequence.
  --
  -- > reverseOnto <x0,...,xn-1> <y0,...,ym-1> = <xn-1,...,x0,y0,...,ym-1>
  reverseOnto  :: s a -> s a -> s a

----------------------------------------------------------------------
-- Maps and folds

  -- | Return the result of applying a function to
  --   every element of a sequence.
  --
  -- > map f <x0,...,xn-1> = <f x0,...,f xn-1>
  map        :: (a -> b) -> s a -> s b

  -- | Apply a sequence-producing function to every element
  --   of a sequence and flatten the result.  Note that 'concatMap'
  --   is the bind (>>=) operation of the sequence monad with the
  --   arguments in the reverse order.
  -- 
  -- > concatMap f xs = concat (map f xs)
  concatMap  :: (a -> s b) -> s a -> s b

  -- | Combine all the elements of a sequence into a single value,
  --   given a right-associative combining function and an intial
  --   value.
  -- 
  -- > foldr (+) c <x0,...,xn-1> = x0 + (x1 + ... + (xn-1 + c))
  foldr     :: (a -> b -> b) -> b -> s a -> b

  -- | Combine all the elements of a sequence into a single value,
  --   given a left-associative combining function and an initial
  --   value.
  --
  -- > foldl (+) c <x0,...,xn-1> = ((c + x0) + x1) + ... + xn-1
  foldl     :: (b -> a -> b) -> b -> s a -> b

  -- | Combine all the elements of a non-empty sequence into a
  --   single value, given a right-associative combining function.
  --   Signals an error if the sequence is empty.
  -- 
  -- > foldr1 (+) <x0,...,xn-1>
  -- >   | n==0 = error "ModuleName.foldr1: empty sequence"
  -- >   | n>0  = x0 + (x1 + ... + xn-1)
  foldr1    :: (a -> a -> a) -> s a -> a  

  -- | Combine all the elements of a non-empty sequence into
  --   a single value, given a left-associative combining function.
  --   Signals error if sequence is empty.
  --
  -- > foldl1 (+) <x0,...,xn-1>
  -- >  | n==0 = error "ModuleName.foldl1: empty sequence"
  -- >  | n>0  = (x0 + x1) + ... + xn-1
  foldl1    :: (a -> a -> a) -> s a -> a  

  -- | reduce is similar to fold, but combines elements in a balanced fashion.
  --   The combining function should usually be associative.  If the combining
  --   function is associative, the various reduce function yield the same
  --   results as the corresponding folds.
  --
  --   What is meant by \"in a balanced fashion\"?  We mean that
  --   @reduce1 (%) \<x0,x1,...,xn-1>@ equals some complete parenthsization of
  --   @x0 % x1 % ... % xn-1@ such that the nesting depth of parentheses
  --   is @O( log n )@.  The precice shape of this parenthesization is
  --   unspecified.
  --
  -- > reducer (+) x xs = reduce1 (+) (cons x xs)
  -- > reducel (+) x xs = reduce1 (+) (snoc xs x)
  --
  -- > reduce1 (+) <x> = x
  -- > reduce1 (+) <x0,...,xn-1> =
  -- >     (reduce1 (+) <x0,...,xi>) + (reduce1 (+) <xi+1,...,xn-1>)
  --
  --   for some i such that @ 0 \<= i && i \< n-1 @
  --
  --   Although the exact value of i is unspecified it tends toward @n\/2@
  --   so that the depth of calls to + is at most logarithmic.
  --
  --   Note that these are the only sequence operations for which different
  --   implementations are permitted to yield different answers.  Also
  --   note that a single implementation may choose different parenthisizations
  --   for different sequences, even if they are the same length.  This will
  --   typically happen when the sequences were constructed differently.
  --
  --   The canonical applications of the reduce functions are algorithms like
  --   mergesort where:
  --
  -- > mergesort xs = reducer merge empty (map single xs)

  reducer   :: (a -> a -> a) -> a -> s a -> a
  reducel   :: (a -> a -> a) -> a -> s a -> a

  -- | Signals error if sequence is empty.
  reduce1   :: (a -> a -> a) -> s a -> a  

----------------------------------------------------------------------
-- Subsequences

  -- | Extract a prefix of length @i@ from the sequence.  Return
  --   'empty' if @i@ is negative, or the entire sequence if @i@
  --   is too large.
  --
  -- > take i xs = fst (splitAt i xs)
  take        :: Int -> s a -> s a

  -- | Delete a prefix of length @i@ from a sequence.  Return
  --   the entire sequence if @i@ is negative, or 'empty' if
  --   @i@ is too large.
  --
  -- > drop i xs = snd (splitAt i xs)
  drop        :: Int -> s a -> s a
  
  -- | Split a sequence into a prefix of length @i@
  --   and the remining sequence.  Behaves the same
  --   as the corresponding calls to 'take' and 'drop'
  --   if @i@ is negative or too large.
  --
  -- > splitAt i xs
  -- >  | i < 0  = (<>           , <x0,...,xn-1>)
  -- >  | i < n  = (<x0,...,xi-1>, <xi,...,xn-1>)
  -- >  | i >= n = (<x0,...,xn-1>, <>           )
  splitAt     :: Int -> s a -> (s a, s a)


  -- | Extract a subsequence from a sequence.  The integer
  --   arguments are \"start index\" and \"length\" NOT
  --   \"start index\" and \"end index\".  Behaves the same
  --   as the corresponding calls to 'take' and 'drop' if the
  --   start index or length are negative or too large.
  --
  -- > subseq i len xs = take len (drop i xs)
  subseq      :: Int -> Int -> s a -> s a

----------------------------------------------------------------------
-- Predicate-based operations

  -- | Extract the elements of a sequence that satify the
  --   given predicate, retaining the relative ordering of
  --   elements from the original sequence.
  --
  -- > filter p xs = foldr pcons empty xs
  -- >      where pcons x xs = if p x then cons x xs else xs
  filter      :: (a -> Bool) -> s a -> s a

  -- | Separate the elements of a sequence into those that
  --   satisfy the given predicate and those that do not,
  --   retaining the relative ordering of elements from the
  --   original sequence.
  --
  -- > partition p xs = (filter p xs, filter (not . p) xs)
  partition   :: (a -> Bool) -> s a -> (s a, s a)

  -- | Extract the maximal prefix of elements satisfying the
  --   given predicate.
  --
  -- > takeWhile p xs = fst (splitWhile p xs)
  takeWhile   :: (a -> Bool) -> s a -> s a

  -- | Delete the maximal prefix of elements satifying the
  --   given predicate.
  --
  -- > dropWhile p xs = snd (splitWhile p xs)
  dropWhile   :: (a -> Bool) -> s a -> s a

  -- | Split a sequence into the maximal prefix of elements
  --   satisfying the given predicate, and the remaining sequence.
  --
  -- > splitWhile p <x0,...,xn-1> = (<x0,...,xi-1>, <xi,...,xn-1>)
  -- >   where i = min j such that p xj (or n if no such j)
  splitWhile  :: (a -> Bool) -> s a -> (s a, s a)


----------------------------------------------------------------------
-- Index-based operations (zero-based)

  -- | Test whether an index is valid for the given sequence.
  --
  -- > inBounds <x0,...,xn-1> i = (0 <= i && i < n)
  inBounds  :: s a -> Int -> Bool

  -- | Return the element at the given index.  
  --   Signals error if the index out of bounds
  --
  -- > lookup xs@<x0,...,xn-1> i 
  -- >   | inBounds xs = xi
  -- >   | otherwise = error "ModuleName.lookup: index out of bounds"
  lookup    :: s a -> Int -> a

  -- | Return the element at the given index.
  --   Calls 'fail' if the index is out of bounds.
  --
  -- > lookupM xs@<x0,...,xn-1> i 
  -- >   | inBounds xs = Just xi
  -- >   | otherwise = Nothing
  lookupM   :: (Monad m) => s a -> Int -> m a

  -- | Return the element at the given index, or the
  --   default argument if the index is out of bounds.
  --
  -- > lookupWithDefault d xs@<x0,...,xn-1> i 
  -- >   | inBounds xs = xi
  -- >   | otherwise = d
  lookupWithDefault  :: a -> s a -> Int -> a

  lookup m k = ID.runIdentity (lookupM m k)

  -- | Replace the element at the given index, or return
  --   the original sequence if the index is out of bounds.
  -- 
  -- > update i y xs@<x0,...,xn-1>
  -- >   | inBounds xs = <x0,...xi-1,y,xi+1,...,xn-1>
  -- >   | otherwise = xs
  update    :: Int -> a -> s a -> s a

  -- | Apply a function to the element at the given index, or
  --   return the original sequence if the index is out of bounds.
  -- 
  -- > adjust f i xs@<x0,...,xn-1>
  -- >   | inBounds xs = <x0,...xi-1,f xi,xi+1,...,xn-1>
  -- >   | otherwise = xs
  adjust    :: (a -> a) -> Int -> s a -> s a -- map a single element

  -- | Like 'map', but include the index with each element.
  --
  -- > mapWithIndex f <x0,...,xn-1> = <f 0 x0,...,f (n-1) xn-1>
  mapWithIndex    :: (Int -> a -> b) -> s a -> s b

  -- | Like 'foldr', but include the index with each element.
  --
  -- > foldrWithIndex f c <x0,...,xn-1> = 
  -- >     f 0 x0 (f 1 x1 (... (f (n-1) xn-1 c)))
  foldrWithIndex  :: (Int -> a -> b -> b) -> b -> s a -> b

  -- | Like 'foldl', but include the index with each element.
  --
  -- > foldlWithIndex f c <x0,...,xn-1> =
  -- >     f (...(f (f c 0 x0) 1 x1)...) (n-1) xn-1)
  foldlWithIndex  :: (b -> Int -> a -> b) -> b -> s a -> b

----------------------------------------------------------------------
-- Zips and unzips


  -- | Combine two sequences into a sequence of pairs.  If the
  --   sequences are different lengths, the excess elements of the
  --   longer sequence is discarded.
  --
  -- > zip <x0,...,xn-1> <y0,...,ym-1> = <(x0,y0),...,(xj-1,yj-1)>
  -- >     where j = min {n,m}
  zip         :: s a -> s b -> s (a,b)

  -- | Like 'zip', but combines three sequences into triples.
  --
  -- > zip3 <x0,...,xn-1> <y0,...,ym-1> <z0,...,zk-1> = 
  -- >      <(x0,y0,z0),...,(xj-1,yj-1,zj-1)>
  -- >    where j = min {n,m,k}
  zip3        :: s a -> s b -> s c -> s (a,b,c)

  -- | Combine two sequences into a single sequence by mapping
  --   a combining function across corresponding elements.  If
  --   the sequences are of different lengths, the excess elements
  --   of the longer sequence are discarded.
  --
  -- > zipWith f xs ys = map (uncurry f) (zip xs ys)
  zipWith     :: (a -> b -> c) -> s a -> s b -> s c

  -- | Like 'zipWith' but for a three-place function and three
  --   sequences.
  --
  -- > zipWith3 f xs ys zs = map (uncurry f) (zip3 xs ys zs)
  zipWith3    :: (a -> b -> c -> d) -> s a -> s b -> s c -> s d

  -- | Transpose a sequence of pairs into a pair of sequences.
  --
  -- > unzip xs = (map fst xs, map snd xs)
  unzip       :: s (a,b) -> (s a, s b)

  -- | Transpose a sequence of triples into a triple of sequences
  --
  -- > unzip3 xs = (map fst3 xs, map snd3 xs, map thd3 xs)
  -- >    where fst3 (x,y,z) = x
  -- >          snd3 (x,y,z) = y
  -- >          thd3 (x,y,z) = z
  unzip3      :: s (a,b,c) -> (s a, s b, s c)

  -- | Map two functions across every element of a sequence,
  --   yielding a pair of sequences
  --
  -- > unzipWith f g xs = (map f xs, map g xs)
  unzipWith   :: (a -> b) -> (a -> c) -> s a -> (s b, s c)

  -- | Map three functions across every element of a sequence,
  --   yielding a triple of sequences.
  --
  -- > unzipWith3 f g h xs = (map f xs, map g xs, map h xs)
  unzipWith3  :: (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)

----------------------------------------------------------------------
-- Documentation

  -- | The name of the module implementing s.
  instanceName  :: s a -> String


----------------------------------------------------------------------
-- Other possible operations not currently included
{-
  insertAt :: Int -> a -> s a -> s a
    -- adds to front or rear if index out of bounds
    --
    -- insertAt i y xs@<x0,...,xn-1>
    --    | i < 0  = cons y xs
    --    | i >= n = snoc xs y
    --    | otherwise = <x0,...,xi-1,y,xi,...,xn-1>

  deleteAt :: Int -> s a -> s a
    -- returns original sequence if index out of bounds
    --
    -- deleteAt i xs@<x0,...,xn-1>
    --    | i < 0  = xs
    --    | i >= n = xs
    --    | otherwise = <x0,...,xi-1,xi+1,...,xn-1>

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
