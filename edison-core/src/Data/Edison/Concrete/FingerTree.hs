-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Edison.Concrete.FingerTree
-- Copyright   :  (c) Ross Paterson, Ralf Hinze 2006
-- License     :  BSD-style
-- Maintainer  :  robdockins AT fastmail DOT fm
-- Stability   :  internal (non-stable)
-- Portability :  non-portable (MPTCs and functional dependencies)
--
-- A general sequence representation with arbitrary annotations, for
-- use as a base for implementations of various collection types, as
-- described in section 4 of
--
--    * Ralf Hinze and Ross Paterson,
--      \"Finger trees: a simple general-purpose data structure\",
--      /Journal of Functional Programming/ 16:2 (2006) pp 197-217.
--      <http://www.soi.city.ac.uk/~ross/papers/FingerTree.html>
--
-- This data structure forms the basis of the "Data.Edison.Seq.FingerSeq"
-- sequence data structure.
--
-- An amortized running time is given for each operation, with /n/
-- referring to the length of the sequence.  These bounds hold even in
-- a persistent (shared) setting.
--
-----------------------------------------------------------------------------

{------------------------------------------------------------------

Copyright 2004, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

-----------------------------------------------------------------------------}


module Data.Edison.Concrete.FingerTree (
        FingerTree,
        Split(..),

        empty, singleton, lcons, rcons, append,
        fromList, toList, null, size, lview, rview,
        split, takeUntil, dropUntil, splitTree,
        reverse, mapTree, foldFT, reduce1, reduce1',
        strict, strictWith, structuralInvariant

        -- traverse'
        ) where

import Prelude hiding (null, reverse)
import Data.Monoid
import Test.QuickCheck

import Data.Edison.Prelude


infixr 5 `lcons`
infixl 5 `rcons0`

data Digit a
        = One a
        | Two a a
        | Three a a a
        | Four a a a a
        deriving Show

foldDigit :: (b -> b -> b) -> (a -> b) -> Digit a -> b
foldDigit mapp f (One a) = f a
foldDigit mapp f (Two a b) = f a `mapp` f b
foldDigit mapp f (Three a b c) = f a `mapp` f b `mapp` f c
foldDigit mapp f (Four a b c d) = f a `mapp` f b `mapp` f c `mapp` f d

reduceDigit :: (b -> b -> b) -> (a -> b) -> Digit a -> b
reduceDigit mapp f (One a) = f a
reduceDigit mapp f (Two a b) = f a `mapp` f b
reduceDigit mapp f (Three a b c) = f a `mapp` f b `mapp` f c
reduceDigit mapp f (Four a b c d) = (f a `mapp` f b) `mapp` (f c `mapp` f d)

digitToList :: Digit a -> [a] -> [a]
digitToList (One a)        xs = a : xs
digitToList (Two a b)      xs = a : b : xs
digitToList (Three a b c)  xs = a : b : c : xs
digitToList (Four a b c d) xs = a : b : c : d : xs

sizeDigit :: (a -> Int) -> Digit a -> Int
sizeDigit f (One x)        = f x
sizeDigit f (Two x y)      = f x + f y
sizeDigit f (Three x y z)  = f x + f y + f z
sizeDigit f (Four x y z w) = f x + f y + f z + f w

instance (Measured v a) => Measured v (Digit a) where
        measure = foldDigit mappend measure

data Node v a = Node2 !v a a | Node3 !v a a a
        deriving Show

sizeNode :: (a -> Int) -> Node v a -> Int
sizeNode f (Node2 _ x y)   = f x + f y
sizeNode f (Node3 _ x y z) = f x + f y + f z

foldNode :: (b -> b -> b) -> (a -> b) -> Node v a -> b
foldNode mapp f (Node2 _ a b)   = f a `mapp` f b
foldNode mapp f (Node3 _ a b c) = f a `mapp` f b `mapp` f c

nodeToList :: Node v a -> [a] -> [a]
nodeToList (Node2 _ a b)   xs = a : b : xs
nodeToList (Node3 _ a b c) xs = a : b : c : xs

node2        ::  (Measured v a) => a -> a -> Node v a
node2 a b    =   Node2 (measure a `mappend` measure b) a b

node3        ::  (Measured v a) => a -> a -> a -> Node v a
node3 a b c  =   Node3 (measure a `mappend` measure b `mappend` measure c) a b c

instance (Monoid v) => Measured v (Node v a) where
        measure (Node2 v _ _)    =  v
        measure (Node3 v _ _ _)  =  v

nodeToDigit :: Node v a -> Digit a
nodeToDigit (Node2 _ a b) = Two a b
nodeToDigit (Node3 _ a b c) = Three a b c


-- | Finger trees with element type @a@, annotated with measures of type @v@.
-- The operations enforce the constraint @'Measured' v a@.
data FingerTree v a
        = Empty
        | Single a
        | Deep !v !(Digit a) (FingerTree v (Node v a)) !(Digit a)

deep ::  (Measured v a) =>
         Digit a -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deep pr m sf  =   Deep ((measure pr `mappendVal` m) `mappend` measure sf) pr m sf

structuralInvariant :: (Eq v, Measured v a) => FingerTree v a -> Bool
structuralInvariant Empty      = True
structuralInvariant (Single _) = True
structuralInvariant (Deep v pr m sf) =
     v == foldDigit mappend measure pr `mappend`
          foldFT    mempty mappend (foldNode mappend measure) m `mappend`
          foldDigit mappend measure sf

instance (Measured v a) => Measured v (FingerTree v a) where
        measure Empty           =  mempty
        measure (Single x)      =  measure x
        measure (Deep v _ _ _)  =  v

sizeFT :: (a -> Int) -> FingerTree v a -> Int
sizeFT f Empty            = 0
sizeFT f (Single x)       = f x
sizeFT f (Deep _ d1 m d2) = sizeDigit f d1 + sizeFT (sizeNode f) m + sizeDigit f d2

size :: FingerTree v a -> Int
size = sizeFT (const 1)

foldFT :: b -> (b -> b -> b) -> (a -> b) -> FingerTree v a -> b
foldFT mz mapp _ Empty      = mz
foldFT mz mapp f (Single x) = f x
foldFT mz mapp f (Deep _ pr m sf) =
             foldDigit  mapp f pr `mapp` foldFT mz mapp (foldNode mapp f) m `mapp` foldDigit mapp f sf

ftToList :: FingerTree v a -> [a] -> [a]
ftToList Empty xs             = xs
ftToList (Single a) xs        = a : xs
ftToList (Deep _ d1 ft d2) xs = digitToList d1 (foldr nodeToList [] . ftToList ft $ []) ++ (digitToList d2 xs)

toList :: FingerTree v a -> [a]
toList ft = ftToList ft []

reduce1_aux :: (b -> b -> b) -> (a -> b) -> Digit a -> FingerTree v (Node v a) -> Digit a -> b
reduce1_aux mapp f pr Empty sf = 
     (reduceDigit mapp f pr) `mapp`
     (reduceDigit mapp f sf)

reduce1_aux mapp f pr (Single x) sf =
     (reduceDigit mapp f pr) `mapp`
     (foldNode mapp f x)     `mapp`
     (reduceDigit mapp f sf)

reduce1_aux mapp f pr (Deep _ pr' m sf') sf =
     (reduceDigit mapp f pr) `mapp` 
     (reduce1_aux mapp
        (foldNode mapp f)
            pr' m sf')       `mapp`
     (reduceDigit mapp f sf)

reduce1 :: (a -> a -> a) -> FingerTree v a -> a
reduce1 mapp Empty             = error "FingerTree.reduce1: empty tree"
reduce1 mapp (Single x)        = x
reduce1 mapp (Deep _ pr m sf)  = reduce1_aux mapp id pr m sf

reduce1' :: (a -> a -> a) -> FingerTree v a -> a 
reduce1' mapp Empty            = error "FingerTree.reduce1': empty tree"
reduce1' mapp (Single x)       = x
reduce1' mapp (Deep _ pr m sf) = reduce1_aux mapp' id pr m sf
  where mapp' x y = x `seq` y `seq` mapp x y


strict :: FingerTree v a -> FingerTree v a
strict xs = foldFT () seq (const ()) xs `seq` xs

strictWith :: (a -> b) -> FingerTree v a -> FingerTree v a
strictWith f xs = foldFT () seq (\x -> f x `seq` ()) xs `seq` xs

instance (Measured v a, Eq a) => Eq (FingerTree v a) where
        xs == ys = toList xs == toList ys

instance (Measured v a, Ord a) => Ord (FingerTree v a) where
        compare xs ys = compare (toList xs) (toList ys)

instance (Measured v a, Show a) => Show (FingerTree v a) where
        showsPrec p xs = showParen (p > 10) $
                showString "fromList " . shows (toList xs)

mapTree :: (Measured v2 a2) =>
        (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
mapTree _ Empty = Empty
mapTree f (Single x) = Single (f x)
mapTree f (Deep _ pr m sf) =
        deep (mapDigit f pr) (mapTree (mapNode f) m) (mapDigit f sf)

mapNode :: (Measured v2 a2) =>
        (a1 -> a2) -> Node v1 a1 -> Node v2 a2
mapNode f (Node2 _ a b) = node2 (f a) (f b)
mapNode f (Node3 _ a b c) = node3 (f a) (f b) (f c)

mapDigit :: (a -> b) -> Digit a -> Digit b
mapDigit f (One a) = One (f a)
mapDigit f (Two a b) = Two (f a) (f b)
mapDigit f (Three a b c) = Three (f a) (f b) (f c)
mapDigit f (Four a b c d) = Four (f a) (f b) (f c) (f d)


{-
-- | Like 'traverse', but with a more constrained type.
traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
        (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverse' = traverseTree

traverseTree :: (Measured v2 a2, Applicative f) =>
        (a1 -> f a2) -> FingerTree v1 a1 -> f (FingerTree v2 a2)
traverseTree _ Empty = pure Empty
traverseTree f (Single x) = Single <$> f x
traverseTree f (Deep _ pr m sf) =
        deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf

traverseNode :: (Measured v2 a2, Applicative f) =>
        (a1 -> f a2) -> Node v1 a1 -> f (Node v2 a2)
traverseNode f (Node2 _ a b) = node2 <$> f a <*> f b
traverseNode f (Node3 _ a b c) = node3 <$> f a <*> f b <*> f c

traverseDigit :: (Applicative f) => (a -> f b) -> Digit a -> f (Digit b)
traverseDigit f (One a) = One <$> f a
traverseDigit f (Two a b) = Two <$> f a <*> f b
traverseDigit f (Three a b c) = Three <$> f a <*> f b <*> f c
traverseDigit f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d
-}

-- | /O(1)/. The empty sequence.
empty :: Measured v a => FingerTree v a
empty = Empty

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> FingerTree v a
singleton = Single

-- | /O(n)/. Create a sequence from a finite list of elements.
fromList :: (Measured v a) => [a] -> FingerTree v a
fromList = foldr lcons Empty

-- | /O(1)/. Add an element to the left end of a sequence.
lcons :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a `lcons` Empty         =  Single a
a `lcons` Single b              =  deep (One a) Empty (One b)
a `lcons` Deep _ (Four b c d e) m sf = m `seq`
        deep (Two a b) (node3 c d e `lcons` m) sf
a `lcons` Deep _ pr m sf        =  deep (consDigit a pr) m sf

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d
consDigit _ _ = error "FingerTree.consDigit: bug!"

-- | /O(1)/. Add an element to the right end of a sequence.
rcons ::  (Measured v a) => a -> FingerTree v a -> FingerTree v a
rcons = flip rcons0

rcons0 :: (Measured v a) => FingerTree v a -> a -> FingerTree v a
Empty `rcons0` a                =  Single a
Single a `rcons0` b             =  deep (One a) Empty (One b)
Deep _ pr m (Four a b c d) `rcons0` e = m `seq`
        deep pr (m `rcons0` node3 a b c) (Two d e)
Deep _ pr m sf `rcons0` x       =  deep pr m (snocDigit sf x)

snocDigit :: Digit a -> a -> Digit a
snocDigit (One a) b = Two a b
snocDigit (Two a b) c = Three a b c
snocDigit (Three a b c) d = Four a b c d
snocDigit _ _ = error "FingerTree.snocDigit: bug!"

-- | /O(1)/. Is this the empty sequence?
null :: (Measured v a) => FingerTree v a -> Bool
null Empty = True
null _ = False

-- | /O(1)/. Analyse the left end of a sequence.
lview :: (Measured v a, Monad m) => FingerTree v a -> m (a,FingerTree v a)
lview Empty                 =  fail "FingerTree.lview: empty tree"
lview (Single x)            =  return (x, Empty)
lview (Deep _ (One x) m sf) =  return . (,) x $
        case lview m of
          Nothing     -> digitToTree sf
          Just (a,m') -> deep (nodeToDigit a) m' sf

lview (Deep _ pr m sf)      =  return (lheadDigit pr, deep (ltailDigit pr) m sf)

lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d
ltailDigit _ = error "FingerTree.ltailDigit: bug!"

-- | /O(1)/. Analyse the right end of a sequence.
rview :: (Measured v a, Monad m) => FingerTree v a -> m (a, FingerTree v a)
rview Empty                  = fail "FingerTree.rview: empty tree"
rview (Single x)             = return (x, Empty)
rview (Deep _ pr m (One x))  = return . (,) x $
        case rview m of
           Nothing      -> digitToTree pr
           Just (a,m')  -> deep pr m' (nodeToDigit a)

rview (Deep _ pr m sf)       =  return (rheadDigit sf, deep pr m (rtailDigit sf))


rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c
rtailDigit _ = error "FingerTree.rtailDigit: bug!"

digitToTree :: (Measured v a) => Digit a -> FingerTree v a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)


-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
append :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
append =  appendTree0

appendTree0 :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
appendTree0 Empty xs =
        xs
appendTree0 xs Empty =
        xs
appendTree0 (Single x) xs =
        x `lcons` xs
appendTree0 xs (Single x) =
        xs `rcons0` x
appendTree0 (Deep _ pr1 m1 sf1) (Deep _ pr2 m2 sf2) =
        deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2

addDigits0 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits0 m1 (One a) (One b) m2 =
        appendTree1 m1 (node2 a b) m2
addDigits0 m1 (One a) (Two b c) m2 =
        appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (One a) (Three b c d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (One a) (Four b c d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (One c) m2 =
        appendTree1 m1 (node3 a b c) m2
addDigits0 m1 (Two a b) (Two c d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Two a b) (Three c d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Two a b) (Four c d e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (One d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits0 m1 (Three a b c) (Two d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Three a b c) (Three d e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Three a b c) (Four d e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (One e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits0 m1 (Four a b c d) (Two e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits0 m1 (Four a b c d) (Three e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits0 m1 (Four a b c d) (Four e f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2

appendTree1 :: (Measured v a) => FingerTree v a -> a -> FingerTree v a -> FingerTree v a
appendTree1 Empty a xs =
        a `lcons` xs
appendTree1 xs a Empty =
        xs `rcons0` a
appendTree1 (Single x) a xs =
        x `lcons` (a `lcons` xs)
appendTree1 xs a (Single x) =
        xs `rcons0` a `rcons0` x
appendTree1 (Deep _ pr1 m1 sf1) a (Deep _ pr2 m2 sf2) =
        deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2

addDigits1 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits1 m1 (One a) b (One c) m2 =
        appendTree1 m1 (node3 a b c) m2
addDigits1 m1 (One a) b (Two c d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (One a) b (Three c d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (One a) b (Four c d e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (One d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits1 m1 (Two a b) c (Two d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Two a b) c (Three d e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Two a b) c (Four d e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (One e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits1 m1 (Three a b c) d (Two e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Three a b c) d (Three e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Three a b c) d (Four e f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (One f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits1 m1 (Four a b c d) e (Two f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits1 m1 (Four a b c d) e (Three f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits1 m1 (Four a b c d) e (Four f g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2

appendTree2 :: (Measured v a) => FingerTree v a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree2 Empty a b xs =
        a `lcons` (b `lcons` xs)
appendTree2 xs a b Empty =
        xs `rcons0` a `rcons0` b
appendTree2 (Single x) a b xs =
        x `lcons` (a `lcons` (b `lcons` xs))
appendTree2 xs a b (Single x) =
        xs `rcons0` a `rcons0` b `rcons0` x
appendTree2 (Deep _ pr1 m1 sf1) a b (Deep _ pr2 m2 sf2) =
        deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2

addDigits2 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits2 m1 (One a) b c (One d) m2 =
        appendTree2 m1 (node2 a b) (node2 c d) m2
addDigits2 m1 (One a) b c (Two d e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (One a) b c (Three d e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (One a) b c (Four d e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (One e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits2 m1 (Two a b) c d (Two e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Two a b) c d (Three e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Two a b) c d (Four e f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (One f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits2 m1 (Three a b c) d e (Two f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Three a b c) d e (Three f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Three a b c) d e (Four f g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (One g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits2 m1 (Four a b c d) e f (Two g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits2 m1 (Four a b c d) e f (Three g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits2 m1 (Four a b c d) e f (Four g h i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2

appendTree3 :: (Measured v a) => FingerTree v a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree3 Empty a b c xs =
        a `lcons` (b `lcons` (c `lcons` xs))
appendTree3 xs a b c Empty =
        xs `rcons0` a `rcons0` b `rcons0` c
appendTree3 (Single x) a b c xs =
        x `lcons` (a `lcons` (b `lcons` (c `lcons` xs)))
appendTree3 xs a b c (Single x) =
        xs `rcons0` a `rcons0` b `rcons0` c `rcons0` x
appendTree3 (Deep _ pr1 m1 sf1) a b c (Deep _ pr2 m2 sf2) =
        deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2

addDigits3 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits3 m1 (One a) b c d (One e) m2 =
        appendTree2 m1 (node3 a b c) (node2 d e) m2
addDigits3 m1 (One a) b c d (Two e f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (One a) b c d (Three e f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (One a) b c d (Four e f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (One f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits3 m1 (Two a b) c d e (Two f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Two a b) c d e (Three f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Two a b) c d e (Four f g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (One g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits3 m1 (Three a b c) d e f (Two g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Three a b c) d e f (Three g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Three a b c) d e f (Four g h i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (One h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits3 m1 (Four a b c d) e f g (Two h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits3 m1 (Four a b c d) e f g (Three h i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits3 m1 (Four a b c d) e f g (Four h i j k) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2

appendTree4 :: (Measured v a) => FingerTree v a -> a -> a -> a -> a -> FingerTree v a -> FingerTree v a
appendTree4 Empty a b c d xs =
        a `lcons` b `lcons` c `lcons` d `lcons` xs
appendTree4 xs a b c d Empty =
        xs `rcons0` a `rcons0` b `rcons0` c `rcons0` d
appendTree4 (Single x) a b c d xs =
        x `lcons` a `lcons` b `lcons` c `lcons` d `lcons` xs
appendTree4 xs a b c d (Single x) =
        xs `rcons0` a `rcons0` b `rcons0` c `rcons0` d `rcons0` x
appendTree4 (Deep _ pr1 m1 sf1) a b c d (Deep _ pr2 m2 sf2) =
        deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2

addDigits4 :: (Measured v a) => FingerTree v (Node v a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree v (Node v a) -> FingerTree v (Node v a)
addDigits4 m1 (One a) b c d e (One f) m2 =
        appendTree2 m1 (node3 a b c) (node3 d e f) m2
addDigits4 m1 (One a) b c d e (Two f g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (One a) b c d e (Three f g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (One a) b c d e (Four f g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (One g) m2 =
        appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
addDigits4 m1 (Two a b) c d e f (Two g h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Two a b) c d e f (Three g h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Two a b) c d e f (Four g h i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (One h) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
addDigits4 m1 (Three a b c) d e f g (Two h i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Three a b c) d e f g (Three h i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Three a b c) d e f g (Four h i j k) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (One i) m2 =
        appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
addDigits4 m1 (Four a b c d) e f g h (Two i j) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
addDigits4 m1 (Four a b c d) e f g h (Three i j k) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
addDigits4 m1 (Four a b c d) e f g h (Four i j k l) m2 =
        appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2


-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure changes from 'False' to 'True'.
split ::  (Measured v a) =>
          (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _p Empty  =  (Empty, Empty)
split p xs
  | p (measure xs) =  (l, x `lcons` r)
  | otherwise   =  (xs, Empty)
  where Split l x r = splitTree p mempty xs

takeUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p  =  fst . split p

dropUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p  =  snd . split p

data Split t a = Split t a t

splitTree ::    (Measured v a) =>
                (v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v a) a
splitTree _ _ Empty = error "FingerTree.splitTree: bug!"
splitTree _p _i (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
  | p vpr       =  let  Split l x r     =  splitDigit p i pr
                   in   Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vm        =  let  Split ml xs mr  =  splitTree p vpr m
                        Split l x r     =  splitNode p (vpr `mappendVal` ml) xs
                   in   Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise   =  let  Split l x r     =  splitDigit p vm sf
                   in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where vpr     =  i    `mappend`  measure pr
        vm      =  vpr  `mappendVal` m

mappendVal :: (Measured v a) => v -> FingerTree v a -> v
mappendVal v Empty = v
mappendVal v t = v `mappend` measure t

deepL          ::  (Measured v a) =>
        Maybe (Digit a) -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nothing m sf      =   case lview m of
        Nothing     ->  digitToTree sf
        Just (a,m') ->  deep (nodeToDigit a) m' sf
deepL (Just pr) m sf    =   deep pr m sf

deepR          ::  (Measured v a) =>
        Digit a -> FingerTree v (Node v a) -> Maybe (Digit a) -> FingerTree v a
deepR pr m Nothing      =   case rview m of
        Nothing     ->  digitToTree pr
        Just (a,m') ->  deep pr m' (nodeToDigit a)
deepR pr m (Just sf)    =   deep pr m sf

splitNode :: (Measured v a) => (v -> Bool) -> v -> Node v a ->
                Split (Maybe (Digit a)) a
splitNode p i (Node2 _ a b)
  | p va        = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where va      = i `mappend` measure a
splitNode p i (Node3 _ a b c)
  | p va        = Split Nothing a (Just (Two b c))
  | p vab       = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where va      = i `mappend` measure a
        vab     = va `mappend` measure b

splitDigit :: (Measured v a) => (v -> Bool) -> v -> Digit a ->
                Split (Maybe (Digit a)) a
splitDigit p i (One a) = i `seq` Split Nothing a Nothing
splitDigit p i (Two a b)
  | p va        = Split Nothing a (Just (One b))
  | otherwise   = Split (Just (One a)) b Nothing
  where va      = i `mappend` measure a
splitDigit p i (Three a b c)
  | p va        = Split Nothing a (Just (Two b c))
  | p vab       = Split (Just (One a)) b (Just (One c))
  | otherwise   = Split (Just (Two a b)) c Nothing
  where va      = i `mappend` measure a
        vab     = va `mappend` measure b
splitDigit p i (Four a b c d)
  | p va        = Split Nothing a (Just (Three b c d))
  | p vab       = Split (Just (One a)) b (Just (Two c d))
  | p vabc      = Split (Just (Two a b)) c (Just (One d))
  | otherwise   = Split (Just (Three a b c)) d Nothing
  where va      = i `mappend` measure a
        vab     = va `mappend` measure b
        vabc    = vab `mappend` measure c


-- | /O(n)/. The reverse of a sequence.
reverse :: (Measured v a) => FingerTree v a -> FingerTree v a
reverse = reverseTree id

reverseTree :: (Measured v2 a2) => (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
reverseTree _ Empty = Empty
reverseTree f (Single x) = Single (f x)
reverseTree f (Deep _ pr m sf) =
        deep (reverseDigit f sf) (reverseTree (reverseNode f) m) (reverseDigit f pr)

reverseNode :: (Measured v2 a2) => (a1 -> a2) -> Node v1 a1 -> Node v2 a2
reverseNode f (Node2 _ a b) = node2 (f b) (f a)
reverseNode f (Node3 _ a b c) = node3 (f c) (f b) (f a)

reverseDigit :: (a -> b) -> Digit a -> Digit b
reverseDigit f (One a) = One (f a)
reverseDigit f (Two a b) = Two (f b) (f a)
reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)



instance (Arbitrary a) => Arbitrary (Digit a) where
  arbitrary = oneof 
              [ arbitrary       >>= \x         -> return (One x)
              , two arbitrary   >>= \(x,y)     -> return (Two x y)
              , three arbitrary >>= \(x,y,z)   -> return (Three x y z)
              , four arbitrary  >>= \(x,y,z,w) -> return (Four x y z w)
              ]

  coarbitrary p = case p of
      One x        -> variant 0 . coarbitrary x
      Two x y      -> variant 1 . coarbitrary x . coarbitrary y
      Three x y z  -> variant 2 . coarbitrary x . coarbitrary y
                      . coarbitrary z
      Four x y z w -> variant 3 . coarbitrary x . coarbitrary y
                      . coarbitrary z . coarbitrary w


instance (Measured v a, Arbitrary a) => Arbitrary (Node v a) where
  arbitrary = oneof
              [ two arbitrary   >>= \(x,y)     -> return (node2 x y)
              , three arbitrary >>= \(x,y,z)   -> return (node3 x y z)
              ]

  coarbitrary p = case p of
       Node2 _ x y   -> variant 0 . coarbitrary x . coarbitrary y
       Node3 _ x y z -> variant 1 . coarbitrary x . coarbitrary y . coarbitrary z


instance (Measured v a, Arbitrary a) => Arbitrary (FingerTree v a) where
  arbitrary = oneof
               [ return Empty
               , arbitrary >>= return . Single
               , do 
                   pf <- arbitrary
                   m  <- arbitrary
                   sf <- arbitrary
                   return (deep pf m sf)
               ]

  coarbitrary p = case p of
         Empty          -> variant 0
         Single x       -> variant 1 . coarbitrary x
         Deep _ sf m pf -> variant 2 . coarbitrary sf . coarbitrary m . coarbitrary pf
