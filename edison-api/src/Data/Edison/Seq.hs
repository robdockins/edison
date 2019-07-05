-- |
--   Module      :  Data.Edison.Seq
--   Copyright   :  Copyright (c) 1998-1999 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   The sequence abstraction is usually viewed as a hierarchy of ADTs
--   including lists, queues, deques, catenable lists, etc. However, such
--   a hierarchy is based on efficiency rather than functionality. For example,
--   a list supports all the operations that a deque supports, even though
--   some of the operations may be inefficient. Hence, in Edison, all sequence
--   data structures are defined as instances of the single Sequence class:
--
--   @   class (Functor s, MonadPlus s) => Sequence s@
--
--   All sequences are also instances of 'Functor', 'Monad', and 'MonadPlus'.
--   In addition, all sequences are expected to be instances of @Eq@, @Show@,
--   and @Read@, although this is not enforced.
--
--   We follow the naming convention that every module implementing sequences
--   defines a type constructor named @Seq@.
--
--   For each method the \"default\" complexity is listed.  Individual
--   implementations may differ for some methods.  The documentation for
--   each implementation will list those methods for which the running time
--   differs from these.
--
--   A description of each Sequence function appears below.  In most cases
--   psudeocode is also provided. Obviously, the psudeocode is illustrative only.
--   
--   Sequences are represented in psudecode between angle brackets:
--
-- > <x0,x1,x2...,xn-1>
--
--   Such that @x0@ is at the left (front) of the sequence and
--   @xn-1@ is at the right (rear) of the sequence.

module Data.Edison.Seq (
-- * Superclass aliases
-- ** Functor aliases
  map
-- ** Monad aliases
, singleton
, concatMap
-- ** MonadPlus aliases
, empty
, append

-- * The Sequence class
, Sequence (..)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import Control.Monad
import qualified Control.Monad.Fail as MF
import Data.Monoid

import Data.Edison.Prelude


-- | Return the result of applying a function to
--   every element of a sequence.  Identical
--   to @fmap@ from @Functor@.
--
-- > map f <x0,...,xn-1> = <f x0,...,f xn-1>
--
-- /Axioms:/
--
-- * @map f empty = empty@
--
-- * @map f (lcons x xs) = lcons (f x) (map f xs)@
--
--   This function is always /unambiguous/.
--
--   Default running time: @O( t * n )@
--     where @t@ is the running time of @f@
map        :: Sequence s => (a -> b) -> s a -> s b
map = fmap


-- | Create a singleton sequence.  Identical to @return@
--   from @Monad@.
--
-- > singleton x = <x>
--
-- /Axioms:/
--
-- * @singleton x = lcons x empty = rcons x empty@
--
--   This function is always /unambiguous/.
--
--   Default running time: @O( 1 )@
singleton   :: Sequence s => a -> s a
singleton = return


-- | Apply a sequence-producing function to every element
--   of a sequence and flatten the result. 'concatMap'
--   is the bind @(>>=)@ operation of from @Monad@ with the
--   arguments in the reverse order.
-- 
-- > concatMap f xs = concat (map f xs)
--
-- /Axioms:/
--
-- * @concatMap f xs = concat (map f xs)@
--
--   This function is always /unambiguous/.
--
--   Default running time: @O( t * n + m )@
--     where @n@ is the length of the input sequence, @m@ is the
--     length of the output sequence, and @t@ is the running time of @f@
concatMap  :: Sequence s => (a -> s b) -> s a -> s b
concatMap = flip (>>=)


-- | The empty sequence.  Identical to @mzero@
--   from @MonadPlus@.
-- 
-- > empty = <>
--
--   This function is always /unambiguous/.
--  
--   Default running time: @O( 1 )@
empty       :: Sequence s => s a
empty = mzero


-- | Append two sequence, with the first argument on the left
--   and the second argument on the right.  Identical to @mplus@
--   from @MonadPlus@.
-- 
-- > append <x0,...,xn-1> <y0,...,ym-1> = <x0,...,xn-1,y0,...,ym-1>
--
-- /Axioms:/
--
-- * @append xs ys = foldr lcons ys xs@
--
--   This function is always /unambiguous/.
--
--   Default running time: @O( n1 )@
append    :: Sequence s => s a -> s a -> s a
append = mplus


-- | The 'Sequence' class defines an interface for datatypes which
--   implement sequences.  A description for each function is
--   given below.  
class (Functor s, MonadPlus s) => Sequence s where

  -- | Add a new element to the front\/left of a sequence
  --
  -- > lcons x <x0,...,xn-1> = <x,x0,...,xn-1>
  --
  -- /Axioms:/
  --
  -- * @lcons x xs = append (singleton x) xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  lcons     :: a -> s a -> s a

  -- | Add a new element to the right\/rear of a sequence
  --
  -- > rcons x <x0,...,xn-1> = <x0,...,xn-1,x>
  --
  -- /Axioms:/
  --
  -- * @rcons x xs = append xs (singleton x)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rcons     :: a -> s a -> s a


  -- | Convert a list into a sequence
  -- 
  -- > fromList [x0,...,xn-1] = <x0,...,xn-1>
  --
  -- /Axioms:/
  --
  -- * @fromList xs = foldr lcons empty xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  fromList  :: [a] -> s a

  -- | Create a sequence containing @n@ copies of the given element.
  --   Return 'empty' if @n\<0@.
  --
  --   @copy n x = \<x,...,x>@
  --
  -- /Axioms:/
  --
  -- * @n > 0    ==> copy n x = cons x (copy (n-1) x)@
  --
  -- * @n \<= 0   ==> copy n x = empty@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  copy      :: Int -> a -> s a

  -- | Separate a sequence into its first (leftmost) element and the
  --   remaining sequence.  Calls 'fail' if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @lview empty = fail@
  --
  -- * @lview (lcons x xs) = return (x,xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  lview     :: (MF.MonadFail m) => s a -> m (a, s a)

  -- | Return the first element of a sequence.
  --   Signals an error if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @lhead empty = undefined@
  --
  -- * @lhead (lcons x xs) = x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  lhead     :: s a -> a

  -- | Returns the first element of a sequence. 
  --   Calls 'fail' if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @lheadM empty = fail@
  --
  -- * @lheadM (lcons x xs) = return x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  lheadM    :: (MF.MonadFail m) => s a -> m a 

  -- | Delete the first element of the sequence.
  --   Signals error if sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @ltail empty = undefined@
  --
  -- * @ltail (lcons x xs) = xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  ltail     :: s a -> s a

  -- | Delete the first element of the sequence.
  --   Calls 'fail' if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @ltailM empty = fail@
  --
  -- * @ltailM (lcons x xs) = return xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  ltailM    :: (MF.MonadFail m) => s a -> m (s a)

  -- | Separate a sequence into its last (rightmost) element and the
  --   remaining sequence.  Calls 'fail' if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @rview empty = fail@
  --
  -- * @rview (rcons x xs) = return (x,xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rview     :: (MF.MonadFail m) => s a -> m (a, s a)

  -- | Return the last (rightmost) element of the sequence.
  --   Signals error if sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @rhead empty = undefined@
  --
  -- * @rhead (rcons x xs) = x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rhead     :: s a -> a 

  -- | Returns the last element of the sequence.
  --    Calls 'fail' if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @rheadM empty = fail@
  --
  -- * @rheadM (rcons x xs) = return x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rheadM    :: (MF.MonadFail m) => s a -> m a

  -- | Delete the last (rightmost) element of the sequence.
  --   Signals an error if the sequence is empty.
  --
  -- /Axioms:/
  --
  -- * @rtail empty = undefined@
  --
  -- * @rtail (rcons x xs) = xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rtail     :: s a -> s a

  -- | Delete the last (rightmost) element of the sequence.
  --   Calls 'fail' of the sequence is empty
  --
  -- /Axioms:/
  --
  -- * @rtailM empty = fail@
  --
  -- * @rtailM (rcons x xs) = return xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  rtailM    :: (MF.MonadFail m) => s a -> m (s a)

  -- | Returns 'True' if the sequence is empty and 'False' otherwise.
  -- 
  -- > null <x0,...,xn-1> = (n==0)
  --
  -- /Axioms:/
  --
  -- * @null xs = (size xs == 0)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( 1 )@
  null      :: s a -> Bool

  -- | Returns the length of a sequence.
  --
  -- > size <x0,...,xn-1> = n
  --
  -- /Axioms:/
  --
  -- * @size empty = 0@
  --
  -- * @size (lcons x xs) = 1 + size xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  size      :: s a -> Int

  -- | Convert a sequence to a list.
  --
  -- > toList <x0,...,xn-1> = [x0,...,xn-1]
  --
  -- /Axioms:/
  --
  -- * @toList empty = []@
  --
  -- * @toList (lcons x xs) = x : toList xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  toList    :: s a -> [a]

  -- | Flatten a sequence of sequences into a simple sequence.
  --
  -- > concat xss = foldr append empty xss
  --
  -- /Axioms:/
  --
  -- * @concat xss = foldr append empty xss@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n + m )@
  --    where @n@ is the length of the input sequence and @m@ is
  --    length of the output sequence.
  concat    :: s (s a) -> s a

  -- | Reverse the order of a sequence
  --
  -- > reverse <x0,...,xn-1> = <xn-1,...,x0>
  --
  -- /Axioms:/
  --
  -- * @reverse empty = empty@
  --
  -- * @reverse (lcons x xs) = rcons x (reverse xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  reverse      :: s a -> s a

  -- | Reverse a sequence onto the front of another sequence.
  --
  -- > reverseOnto <x0,...,xn-1> <y0,...,ym-1> = <xn-1,...,x0,y0,...,ym-1>
  --
  -- /Axioms:/
  --
  -- * @reverseOnto xs ys = append (reverse xs) ys@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n1 )@
  reverseOnto  :: s a -> s a -> s a


  -- | Combine all the elements of a sequence into a single value,
  --   given a combining function and an initial value.  The order
  --   in which the elements are applied to the combining function
  --   is unspecified.  @fold@ is one of the few ambiguous sequence
  --   functions.
  --
  --   /Axioms:/
  --
  --   * @fold f c empty = c@
  --
  --   * @f is fold-commutative ==> fold f = foldr f = foldl f@
  --
  --   @fold f@ is /unambiguous/ iff @f@ is fold-commutative.
  --
  --   Default running type: @O( t * n )@
  --     where @t@ is the running tome of @f@.
  fold     :: (a -> b -> b) -> b -> s a -> b

  -- | A strict variant of 'fold'.  @fold'@ is one of the few ambiguous
  --   sequence functions.
  --
  --   /Axioms:/
  --
  --   * @forall a. f a _|_ = _|_ ==> fold f x xs = fold' f x xs@
  --
  --   @fold f@ is /unambiguous/ iff @f@ is fold-commutative.
  --
  --   Default running type: @O( t * n )@
  --     where @t@ is the running tome of @f@.
  fold'    :: (a -> b -> b) -> b -> s a -> b

  -- | Combine all the elements of a non-empty sequence into a
  --   single value, given a combining function. Signals an error
  --   if the sequence is empty.
  --
  --   /Axioms:/
  --
  --   * @f is fold-commutative ==> fold1 f = foldr1 f = foldl1 f@
  --
  --   @fold1 f@ is /unambiguous/ iff @f@ is fold-commutative.
  --
  --   Default running type: @O( t * n )@
  --     where @t@ is the running tome of @f@.
  fold1    :: (a -> a -> a) -> s a -> a

  -- | A strict variant of 'fold1'.
  --
  -- /Axioms:/
  --
  -- * @forall a. f a _|_ = _|_ ==> fold1' f xs = fold1 f xs@
  --
  --   @fold1' f@ is /unambiguous/ iff @f@ is fold-commutative.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  fold1'   :: (a -> a -> a) -> s a -> a

  -- | Combine all the elements of a sequence into a single value,
  --   given a combining function and an initial value.  The function
  --   is applied with right nesting.
  -- 
  -- > foldr (%) c <x0,...,xn-1> = x0 % (x1 % ( ... % (xn-1 % c)))
  --
  -- /Axioms:/
  --
  -- * @foldr f c empty = c@
  --
  -- * @foldr f c (lcons x xs) = f x (foldr f c xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldr     :: (a -> b -> b) -> b -> s a -> b

  -- | Strict variant of 'foldr'.  
  --
  -- /Axioms:/
  --
  -- * @forall a. f a _|_ = _|_ ==> foldr f x xs = foldr' f x xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldr'    :: (a -> b -> b) -> b -> s a -> b

  -- | Combine all the elements of a sequence into a single value,
  --   given a combining function and an initial value.  The function
  --   is applied with left nesting.
  --
  -- > foldl (%) c <x0,...,xn-1> = (((c % x0) % x1) % ... ) % xn-1
  --
  -- /Axioms:/
  --
  -- * @foldl f c empty = c@
  --
  -- * @foldl f c (lcons x xs) = foldl f (f c x) xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldl     :: (b -> a -> b) -> b -> s a -> b

  -- | Strict variant of 'foldl'.
  -- 
  -- /Axioms:/
  --
  -- * forall a. f _|_ a = _|_ ==> foldl f z xs = foldl' f z xs
  --
  --   This function is always /unambiguous/.
  -- 
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldl'    :: (b -> a -> b) -> b -> s a -> b

  -- | Combine all the elements of a non-empty sequence into a
  --   single value, given a combining function.  The function
  --   is applied with right nesting. Signals an error if the
  --   sequence is empty.
  -- 
  -- > foldr1 (+) <x0,...,xn-1>
  -- >   | n==0 = error "ModuleName.foldr1: empty sequence"
  -- >   | n>0  = x0 + (x1 + ... + xn-1)
  --
  -- /Axioms:/
  --
  -- * @foldr1 f empty = undefined@
  --
  -- * @foldr1 f (rcons x xs) = foldr f x xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldr1    :: (a -> a -> a) -> s a -> a  

  -- | Strict variant of 'foldr1'.
  --
  -- /Axioms:/
  --
  -- * forall a. f a _|_ = _|_ ==> foldr1 f xs = foldr1' f xs
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldr1'   :: (a -> a -> a) -> s a -> a

  -- | Combine all the elements of a non-empty sequence into
  --   a single value, given a combining function.  The function
  --   is applied with left nesting. Signals an error if the
  --   sequence is empty.
  --
  -- > foldl1 (+) <x0,...,xn-1>
  -- >  | n==0 = error "ModuleName.foldl1: empty sequence"
  -- >  | n>0  = (x0 + x1) + ... + xn-1
  --
  -- /Axioms:/
  --
  -- * @foldl1 f empty = undefined@
  --
  -- * @foldl1 f (lcons x xs) = foldl f x xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldl1    :: (a -> a -> a) -> s a -> a  

  -- | Strict variant of 'foldl1'.
  -- 
  -- /Axioms:/
  --
  -- * forall a. f _|_ a = _|_ ==> foldl1 f xs = foldl1' f xs
  --
  --   This function is always /unambiguous/.
  -- 
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldl1'   :: (a -> a -> a) -> s a -> a

  -- | See 'reduce1' for additional notes.
  --
  -- > reducer f x xs = reduce1 f (cons x xs)
  --
  -- /Axioms:/
  --
  -- * @reducer f c xs = foldr f c xs@ for associative @f@
  -- 
  --   @reducer f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reducer   :: (a -> a -> a) -> a -> s a -> a

  -- | Strict variant of 'reducer'.
  --
  --   See 'reduce1' for additional notes.
  --
  -- /Axioms:/
  --
  -- * @forall a. f a _|_ = _|_ && forall a. f _|_ a = _|_ ==>
  --          reducer f x xs = reducer' f x xs@
  --
  --   @reducer' f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reducer'  :: (a -> a -> a) -> a -> s a -> a

  -- | See 'reduce1' for additional notes.
  --
  -- > reducel f x xs = reduce1 f (rcons x xs)
  --
  -- /Axioms:/
  --
  -- * @reducel f c xs = foldl f c xs@ for associative @f@
  --
  --   @reducel f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reducel   :: (a -> a -> a) -> a -> s a -> a

  -- | Strict variant of 'reducel'.
  --
  --   See 'reduce1' for additional notes.
  --
  -- /Axioms:/
  --
  -- * @forall a. f a _|_ = _|_ && forall a. f _|_ a = _|_ ==>
  --          reducel f x xs = reducel' f x xs@
  --
  --   @reducel' f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reducel'  :: (a -> a -> a) -> a -> s a -> a

  -- | A reduce is similar to a fold, but combines elements in a balanced fashion.
  --   The combining function should usually be associative.  If the combining
  --   function is associative, the various reduce functions yield the same
  --   results as the corresponding folds.
  --
  --   What is meant by \"in a balanced fashion\"?  We mean that
  --   @reduce1 (%) \<x0,x1,...,xn-1>@ equals some complete parenthesization of
  --   @x0 % x1 % ... % xn-1@ such that the nesting depth of parentheses
  --   is @O( log n )@.  The precise shape of this parenthesization is
  --   unspecified.
  -- 
  -- > reduce1 f <x> = x
  -- > reduce1 f <x0,...,xn-1> =
  -- >     f (reduce1 f <x0,...,xi>) (reduce1 f <xi+1,...,xn-1>)
  --
  --   for some @i@ such that @ 0 \<= i && i \< n-1 @
  --
  --   Although the exact value of i is unspecified it tends toward @n\/2@
  --   so that the depth of calls to @f@ is at most logarithmic.
  --
  --   Note that @reduce@* are some of the only sequence operations for which
  --   different implementations are permitted to yield different answers.  Also
  --   note that a single implementation may choose different parenthisizations
  --   for different sequences, even if they are the same length.  This will
  --   typically happen when the sequences were constructed differently.
  --
  --   The canonical applications of the reduce functions are algorithms like
  --   merge sort where:
  --
  -- > mergesort xs = reducer merge empty (map singleton xs)
  --
  --
  -- /Axioms:/
  --
  -- * @reduce1 f empty = undefined@
  --
  -- * @reduce1 f xs = foldr1 f xs = foldl1 f xs@ for associative @f@
  --
  --   @reduce1 f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reduce1   :: (a -> a -> a) -> s a -> a  

  -- | Strict variant of 'reduce1'.
  --
  -- /Axioms:/
  --
  -- * @forall a. f a _|_ = _|_ && forall a. f _|_ a = _|_ ==>
  --          reduce1 f xs = reduce1' f xs@
  --
  --   @reduce1' f@ is unambiguous iff @f@ is an associative function.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  reduce1'  :: (a -> a -> a) -> s a -> a

  -- | Extract a prefix of length @i@ from the sequence.  Return
  --   'empty' if @i@ is negative, or the entire sequence if @i@
  --   is too large.
  --
  -- > take i xs = fst (splitAt i xs)
  --
  -- /Axioms:/
  --
  -- * @i \< 0        ==> take i xs = empty@
  --
  -- * @i > size xs  ==> take i xs = xs@
  --
  -- * @size xs == i ==> take i (append xs ys) = xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  take        :: Int -> s a -> s a

  -- | Delete a prefix of length @i@ from a sequence.  Return
  --   the entire sequence if @i@ is negative, or 'empty' if
  --   @i@ is too large.
  --
  -- > drop i xs = snd (splitAt i xs)
  --
  -- /Axioms:/
  --
  -- * @i \< 0        ==> drop i xs = xs@
  --
  -- * @i > size xs  ==> drop i xs = empty@
  --
  -- * @size xs == i ==> drop i (append xs ys) = ys@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  drop        :: Int -> s a -> s a
  
  -- | Split a sequence into a prefix of length @i@
  --   and the remaining sequence.  Behaves the same
  --   as the corresponding calls to 'take' and 'drop'
  --   if @i@ is negative or too large.
  --
  -- > splitAt i xs
  -- >  | i < 0  = (<>           , <x0,...,xn-1>)
  -- >  | i < n  = (<x0,...,xi-1>, <xi,...,xn-1>)
  -- >  | i >= n = (<x0,...,xn-1>, <>           )
  --
  -- /Axioms:/
  --
  -- * @splitAt i xs = (take i xs,drop i xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  splitAt     :: Int -> s a -> (s a, s a)

  -- | Extract a subsequence from a sequence.  The integer
  --   arguments are \"start index\" and \"length\" NOT
  --   \"start index\" and \"end index\".  Behaves the same
  --   as the corresponding calls to 'take' and 'drop' if the
  --   start index or length are negative or too large.
  --
  -- > subseq i len xs = take len (drop i xs)
  --
  -- /Axioms:/
  --
  -- * @subseq i len xs = take len (drop i xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i + len )@
  subseq      :: Int -> Int -> s a -> s a

  -- | Extract the elements of a sequence that satisfy the
  --   given predicate, retaining the relative ordering of
  --   elements from the original sequence.
  --
  -- > filter p xs = foldr pcons empty xs
  -- >      where pcons x xs = if p x then cons x xs else xs
  --
  -- /Axioms:/
  --
  -- * @filter p empty = empty@
  --
  -- * @filter p (lcons x xs) = if p x 
  --       then lcons x (filter p xs)
  --       else filter p xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @p@
  filter      :: (a -> Bool) -> s a -> s a

  -- | Separate the elements of a sequence into those that
  --   satisfy the given predicate and those that do not,
  --   retaining the relative ordering of elements from the
  --   original sequence.
  --
  -- > partition p xs = (filter p xs, filter (not . p) xs)
  --
  -- /Axioms:/
  --
  -- * @partition p xs = (filter p xs, filter (not . p) xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @p@
  partition   :: (a -> Bool) -> s a -> (s a, s a)

  -- | Extract the maximal prefix of elements satisfying the
  --   given predicate.
  --
  -- > takeWhile p xs = fst (splitWhile p xs)
  --
  -- /Axioms:/
  --
  -- * @takeWhile p empty = empty@
  --
  -- * @takeWhile p (lcons x xs) = if p x
  --       then lcons x (takeWhile p xs)
  --       else empty@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @p@
  takeWhile   :: (a -> Bool) -> s a -> s a

  -- | Delete the maximal prefix of elements satisfying the
  --   given predicate.
  --
  -- > dropWhile p xs = snd (splitWhile p xs)
  --
  -- /Axioms:/
  --
  -- * @dropWhile p empty = empty@
  --
  -- * @dropWhile p (lcons x xs) = if p x
  --      then dropWhile p xs
  --      else lcons x xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @p@
  dropWhile   :: (a -> Bool) -> s a -> s a

  -- | Split a sequence into the maximal prefix of elements
  --   satisfying the given predicate, and the remaining sequence.
  --
  -- > splitWhile p <x0,...,xn-1> = (<x0,...,xi-1>, <xi,...,xn-1>)
  -- >   where i = min j such that p xj (or n if no such j)
  --
  -- /Axioms:/
  --
  -- * @splitWhile p xs = (takeWhile p xs,dropWhile p xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @p@
  splitWhile  :: (a -> Bool) -> s a -> (s a, s a)

  -- | Test whether an index is valid for the given sequence. All indexes
  --   are 0 based.
  --
  -- > inBounds i <x0,...,xn-1> = (0 <= i && i < n)
  --
  -- /Axioms:/
  --
  -- * @inBounds i xs = (0 \<= i && i \< size xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  inBounds  :: Int -> s a -> Bool

  -- | Return the element at the given index.  All indexes are 0 based.
  --   Signals error if the index out of bounds.
  --
  -- > lookup i xs@<x0,...,xn-1>
  -- >   | inBounds i xs = xi
  -- >   | otherwise = error "ModuleName.lookup: index out of bounds"
  --
  -- /Axioms:/
  --
  -- * @not (inBounds i xs)  ==> lookup i xs = undefined@
  --
  -- * @size xs == i ==> lookup i (append xs (lcons x ys)) = x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  lookup    :: Int -> s a -> a

  -- | Return the element at the given index.  All indexes are 0 based.
  --   Calls 'fail' if the index is out of bounds.
  --
  -- > lookupM i xs@<x0,...,xn-1>
  -- >   | inBounds i xs = Just xi
  -- >   | otherwise = Nothing
  --
  -- /Axioms:/
  --
  -- * @not (inBounds i xs) ==> lookupM i xs = fail@
  --
  -- * @size xs == i ==> lookupM i (append xs (lcons x ys)) = return x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  lookupM   :: (MF.MonadFail m) => Int -> s a -> m a

  -- | Return the element at the given index, or the
  --   default argument if the index is out of bounds.  All indexes are
  --   0 based.
  --
  -- > lookupWithDefault d i xs@<x0,...,xn-1>
  -- >   | inBounds i xs = xi
  -- >   | otherwise = d
  --
  -- /Axioms:/
  --
  -- * @not (inBounds i xs) ==> lookupWithDefault d i xs = d@
  --
  -- * @size xs == i ==> lookupWithDefault d i (append xs (lcons x ys)) = x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  lookupWithDefault  :: a -> Int -> s a -> a

  -- | Replace the element at the given index, or return
  --   the original sequence if the index is out of bounds.
  --   All indexes are 0 based.
  -- 
  -- > update i y xs@<x0,...,xn-1>
  -- >   | inBounds i xs = <x0,...xi-1,y,xi+1,...,xn-1>
  -- >   | otherwise = xs
  --
  -- /Axioms:/
  --
  -- * @not (inBounds i xs) ==> update i y xs = xs@
  --
  -- * @size xs == i ==> update i y (append xs (lcons x ys)) =
  --      append xs (lcons y ys)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i )@
  update    :: Int -> a -> s a -> s a

  -- | Apply a function to the element at the given index, or
  --   return the original sequence if the index is out of bounds.
  --   All indexes are 0 based.
  -- 
  -- > adjust f i xs@<x0,...,xn-1>
  -- >   | inBounds i xs = <x0,...xi-1,f xi,xi+1,...,xn-1>
  -- >   | otherwise = xs
  --
  -- /Axioms:/
  --
  -- * @not (inBounds i xs) ==> adjust f i xs = xs@
  --
  -- * @size xs == i ==> adjust f i (append xs (lcons x ys)) =
  --      append xs (cons (f x) ys)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( i + t )@
  --     where @t@ is the running time of @f@
  adjust    :: (a -> a) -> Int -> s a -> s a -- map a single element

  -- | Like 'map', but include the index with each element.
  --   All indexes are 0 based.
  --
  -- > mapWithIndex f <x0,...,xn-1> = <f 0 x0,...,f (n-1) xn-1>
  --
  -- /Axioms:/
  --
  -- * @mapWithIndex f empty = empty@
  --
  -- * @mapWithIndex f (rcons x xs) = rcons (f (size xs) x) (mapWithIndex f xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  mapWithIndex    :: (Int -> a -> b) -> s a -> s b

  -- | Like 'foldr', but include the index with each element.
  --   All indexes are 0 based.
  --
  -- > foldrWithIndex f c <x0,...,xn-1> = 
  -- >     f 0 x0 (f 1 x1 (... (f (n-1) xn-1 c)))
  --
  -- /Axioms:/
  --
  -- * @foldrWithIndex f c empty = c@
  --
  -- * @foldrWithIndex f c (rcons x xs) =
  --      foldrWithIndex f (f (size xs) x c) xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldrWithIndex  :: (Int -> a -> b -> b) -> b -> s a -> b

  -- | Strict variant of 'foldrWithIndex'.
  --
  -- /Axioms:/
  --
  -- * @forall i a. f i a _|_ = _|_ ==> foldrWithIndex f x xs = 
  --       foldrWithIndex' f x xs@
  --
  --   This function is always /unambiguous/.
  -- 
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldrWithIndex' :: (Int -> a -> b -> b) -> b -> s a -> b

  -- | Like 'foldl', but include the index with each element.
  --   All indexes are 0 based.
  --
  -- > foldlWithIndex f c <x0,...,xn-1> =
  -- >     f (...(f (f c 0 x0) 1 x1)...) (n-1) xn-1)
  --
  -- /Axioms:/
  --
  -- * @foldlWithIndex f c empty = c@
  --
  -- * @foldlWithIndex f c (rcons x xs) =
  --      f (foldlWithIndex f c xs) (size xs) x@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldlWithIndex  :: (b -> Int -> a -> b) -> b -> s a -> b

  -- | Strict variant of 'foldlWithIndex'.
  --
  -- /Axioms:/
  --
  -- * @forall i a. f _|_ i a = _|_ ==> foldlWithIndex f x xs = 
  --       foldlWithIndex' f x xs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the running time of @f@
  foldlWithIndex' :: (b -> Int -> a -> b) -> b -> s a -> b

  -- | Combine two sequences into a sequence of pairs.  If the
  --   sequences are different lengths, the excess elements of the
  --   longer sequence is discarded.
  --
  -- > zip <x0,...,xn-1> <y0,...,ym-1> = <(x0,y0),...,(xj-1,yj-1)>
  -- >     where j = min {n,m}
  --
  -- /Axioms:/
  --
  -- * @zip xs ys = zipWith (,) xs ys@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( min( n1, n2 ) )@
  zip         :: s a -> s b -> s (a,b)

  -- | Like 'zip', but combines three sequences into triples.
  --
  -- > zip3 <x0,...,xn-1> <y0,...,ym-1> <z0,...,zk-1> = 
  -- >      <(x0,y0,z0),...,(xj-1,yj-1,zj-1)>
  -- >    where j = min {n,m,k}
  --
  -- /Axioms:/
  --
  -- * @zip3 xs ys zs = zipWith3 (,,) xs ys zs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( min( n1, n2, n3 ) )@
  zip3        :: s a -> s b -> s c -> s (a,b,c)

  -- | Combine two sequences into a single sequence by mapping
  --   a combining function across corresponding elements.  If
  --   the sequences are of different lengths, the excess elements
  --   of the longer sequence are discarded.
  --
  -- > zipWith f xs ys = map (uncurry f) (zip xs ys)
  --
  -- /Axioms:/
  --
  -- * @zipWith f (lcons x xs) (lcons y ys) =
  --     lcons (f x y) (zipWith f xs ys)@
  --
  -- * @(null xs || null ys) ==> zipWith xs ys = empty@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * min( n1, n2 ) )@
  --     where @t@ is the running time of @f@
  zipWith     :: (a -> b -> c) -> s a -> s b -> s c

  -- | Like 'zipWith' but for a three-place function and three
  --   sequences.
  --
  -- > zipWith3 f xs ys zs = map (uncurry f) (zip3 xs ys zs)
  --
  -- /Axioms:/
  --
  -- * @zipWith3 (lcons x xs) (lcons y ys) (lcons z zs) =
  --      lcons (f x y z) (zipWith3 f xs ys zs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * min( n1, n2, n3 ) )@
  --     where @t@ is the running time of @f@
  zipWith3    :: (a -> b -> c -> d) -> s a -> s b -> s c -> s d

  -- | Transpose a sequence of pairs into a pair of sequences.
  --
  -- > unzip xs = (map fst xs, map snd xs)
  --
  -- /Axioms:/
  --
  -- * @unzip xys = unzipWith fst snd xys@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  unzip       :: s (a,b) -> (s a, s b)

  -- | Transpose a sequence of triples into a triple of sequences
  --
  -- > unzip3 xs = (map fst3 xs, map snd3 xs, map thd3 xs)
  -- >    where fst3 (x,y,z) = x
  -- >          snd3 (x,y,z) = y
  -- >          thd3 (x,y,z) = z
  --
  -- /Axioms:/
  --
  -- * @unzip3 xyzs = unzipWith3 fst3 snd3 thd3 xyzs@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  unzip3      :: s (a,b,c) -> (s a, s b, s c)

  -- | Map two functions across every element of a sequence,
  --   yielding a pair of sequences
  --
  -- > unzipWith f g xs = (map f xs, map g xs)
  --
  -- /Axioms:/
  --
  -- * @unzipWith f g xs = (map f xs, map g xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the maximum running time
  --     of @f@ and @g@
  unzipWith   :: (a -> b) -> (a -> c) -> s a -> (s b, s c)

  -- | Map three functions across every element of a sequence,
  --   yielding a triple of sequences.
  --
  -- > unzipWith3 f g h xs = (map f xs, map g xs, map h xs)
  --
  -- /Axioms:/
  --
  -- * @unzipWith3 f g h xs = (map f xs,map g xs,map h xs)@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( t * n )@
  --     where @t@ is the maximum running time
  --     of @f@, @g@, and @h@
  unzipWith3  :: (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)

  -- | Semanticly, this function is a partial identity function.  If the
  --   datastructure is infinite in size or contains exceptions or non-termination
  --   in the structure itself, then @strict@ will result in bottom.  Operationally,
  --   this function walks the datastructure forcing any closures.  Elements contained
  --   in the sequence are /not/ forced.
  --
  --   /Axioms:/
  --
  --   * @strict xs = xs@ OR @strict xs = _|_@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: @O( n )@
  strict :: s a -> s a

  -- | Similar to 'strict', this function walks the datastructure forcing closures.
  --   However, @strictWith@ will additionally apply the given function to the
  --   sequence elements, force the result using @seq@, and then ignore it.
  --   This function can be used to perform various levels of forcing on the
  --   sequence elements.  In particular:
  --
  -- > strictWith id xs
  --
  --   will force the spine of the datastructure and reduce each element to WHNF.
  --
  --   /Axioms:/
  --
  --   * forall @f :: a -> b@, @strictWith f xs = xs@ OR @strictWith f xs = _|_@
  --
  --   This function is always /unambiguous/.
  --
  --   Default running time: unbounded (forcing element closures can take arbitrairly long)
  strictWith :: (a -> b) -> s a -> s a

  -- | A method to facilitate unit testing.  Returns 'True' if the structural
  --   invariants of the implementation hold for the given sequence.  If
  --   this function returns 'False', it represents a bug in the implementation.
  structuralInvariant :: s a -> Bool

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
