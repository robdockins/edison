-----------------------------------------------------------------------------
-- |
-- Module      :  Data.FingerTree
-- Copyright   :  (c) Ross Paterson, Ralf Hinze 2006
-- License     :  BSD-style
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
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
-- For a directly usable sequence type, see "Data.Sequence", which is
-- a specialization of this structure.
--
-- An amortized running time is given for each operation, with /n/
-- referring to the length of the sequence.  These bounds hold even in
-- a persistent (shared) setting.
--
-- /Note/: Many of these operations have the same names as similar
-- operations on lists in the "Prelude".  The ambiguity may be resolved
-- using either qualification or the @hiding@ clause.
--
-----------------------------------------------------------------------------

module Data.Edison.Concrete.FingerTree (
	FingerTree,
	Measured(..),
	-- * Construction
	empty, singleton,
        (<|), (|>), (><),
        fromList,
        -- * Deconstruction
        null,
        ViewL(..), ViewR(..), viewl, viewr,
        split, takeUntil, dropUntil,
        -- * Transformation
        reverse,
        fmap', traverse'
        ) where

import Prelude hiding (null, reverse)

import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Data.Monoid
import Data.Foldable (Foldable(foldMap), toList)
import Data.Traversable (Traversable(traverse))

infixr 5 ><
infixr 5 <|, :<
infixl 5 |>, :>

-- | View of the left end of a sequence.
data ViewL s a
        = EmptyL        -- ^ empty sequence
        | a :< s a      -- ^ leftmost element and the rest of the sequence
        deriving (Eq, Ord, Show, Read)

-- | View of the right end of a sequence.
data ViewR s a
        = EmptyR        -- ^ empty sequence
        | s a :> a      -- ^ the sequence minus the rightmost element,
                        -- and the rightmost element
        deriving (Eq, Ord, Show, Read)

instance Functor s => Functor (ViewL s) where
        fmap f EmptyL             = EmptyL
        fmap f (x :< xs)        = f x :< fmap f xs

instance Functor s => Functor (ViewR s) where
        fmap f EmptyR             = EmptyR
        fmap f (xs :> x)        = fmap f xs :> f x

-- Explicit Digit type (Exercise 1)

data Digit a
        = One a
        | Two a a
        | Three a a a
        | Four a a a a
        deriving Show

instance Foldable Digit where
        foldMap f (One a) = f a
        foldMap f (Two a b) = f a `mappend` f b
        foldMap f (Three a b c) = f a `mappend` f b `mappend` f c
        foldMap f (Four a b c d) = f a `mappend` f b `mappend` f c `mappend` f d

-------------------
-- 4.1 Measurements
-------------------

-- | Things that can be measured.
class (Monoid v) => Measured v a | a -> v where
        measure :: a -> v

instance (Measured v a) => Measured v (Digit a) where
        measure =  foldMap measure

---------------------------
-- 4.2 Caching measurements
---------------------------

data Node v a = Node2 !v a a | Node3 !v a a a
        deriving Show

instance Foldable (Node v) where
        foldMap f (Node2 _ a b) = f a `mappend` f b
        foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c

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

instance (Measured v a) => Measured v (FingerTree v a) where
        measure Empty           =  mempty
        measure (Single x)      =  measure x
        measure (Deep v _ _ _)  =  v

instance Foldable (FingerTree v) where
        foldMap _ Empty = mempty
        foldMap f (Single x) = f x
        foldMap f (Deep _ pr m sf) =
                foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf

instance (Measured v a, Eq a) => Eq (FingerTree v a) where
        xs == ys = toList xs == toList ys

instance (Measured v a, Ord a) => Ord (FingerTree v a) where
        compare xs ys = compare (toList xs) (toList ys)

instance (Measured v a, Show a) => Show (FingerTree v a) where
        showsPrec p xs = showParen (p > 10) $
                showString "fromList " . shows (toList xs)

-- | Like 'fmap', but with a more constrained type.
fmap' :: (Measured v1 a1, Measured v2 a2) =>
        (a1 -> a2) -> FingerTree v1 a1 -> FingerTree v2 a2
fmap' = mapTree

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

-- | Like 'traverse', but with a more constrained type.
<traverse' :: (Measured v1 a1, Measured v2 a2, Applicative f) =>
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

-----------------------------------------------------
-- 4.3 Construction, deconstruction and concatenation
-----------------------------------------------------

-- | /O(1)/. The empty sequence.
empty :: Measured v a => FingerTree v a
empty = Empty

-- | /O(1)/. A singleton sequence.
singleton :: Measured v a => a -> FingerTree v a
singleton = Single

-- | /O(n)/. Create a sequence from a finite list of elements.
fromList :: (Measured v a) => [a] -> FingerTree v a
fromList = foldr (<|) Empty

-- | /O(1)/. Add an element to the left end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: (Measured v a) => a -> FingerTree v a -> FingerTree v a
a <| Empty		=  Single a
a <| Single b		=  deep (One a) Empty (One b)
a <| Deep _ (Four b c d e) m sf = m `seq`
	deep (Two a b) (node3 c d e <| m) sf
a <| Deep _ pr m sf	=  deep (consDigit a pr) m sf

consDigit :: a -> Digit a -> Digit a
consDigit a (One b) = Two a b
consDigit a (Two b c) = Three a b c
consDigit a (Three b c d) = Four a b c d

-- | /O(1)/. Add an element to the right end of a sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: (Measured v a) => FingerTree v a -> a -> FingerTree v a
Empty |> a		=  Single a
Single a |> b		=  deep (One a) Empty (One b)
Deep _ pr m (Four a b c d) |> e = m `seq`
	deep pr (m |> node3 a b c) (Two d e)
Deep _ pr m sf |> x	=  deep pr m (snocDigit sf x)

snocDigit :: Digit a -> a -> Digit a
snocDigit (One a) b = Two a b
snocDigit (Two a b) c = Three a b c
snocDigit (Three a b c) d = Four a b c d

-- | /O(1)/. Is this the empty sequence?
null :: (Measured v a) => FingerTree v a -> Bool
null Empty = True
null _ = False

-- | /O(1)/. Analyse the left end of a sequence.
viewl :: (Measured v a) => FingerTree v a -> ViewL (FingerTree v) a
viewl Empty			=  EmptyL
viewl (Single x)		=  x :< Empty
viewl (Deep _ (One x) m sf)	=  x :< case viewl m of
	EmptyL	->  digitToTree sf
	a :< m' ->  deep (nodeToDigit a) m' sf
viewl (Deep _ pr m sf)	=  lheadDigit pr :< deep (ltailDigit pr) m sf

lheadDigit :: Digit a -> a
lheadDigit (One a) = a
lheadDigit (Two a _) = a
lheadDigit (Three a _ _) = a
lheadDigit (Four a _ _ _) = a

ltailDigit :: Digit a -> Digit a
ltailDigit (Two _ b) = One b
ltailDigit (Three _ b c) = Two b c
ltailDigit (Four _ b c d) = Three b c d

-- | /O(1)/. Analyse the right end of a sequence.
viewr :: (Measured v a) => FingerTree v a -> ViewR (FingerTree v) a
viewr Empty			=  EmptyR
viewr (Single x)		=  Empty :> x
viewr (Deep _ pr m (One x))	=  (case viewr m of
	EmptyR	->  digitToTree pr
	m' :> a ->  deep pr m' (nodeToDigit a)) :> x
viewr (Deep _ pr m sf)	=  deep pr m (rtailDigit sf) :> rheadDigit sf

rheadDigit :: Digit a -> a
rheadDigit (One a) = a
rheadDigit (Two _ b) = b
rheadDigit (Three _ _ c) = c
rheadDigit (Four _ _ _ d) = d

rtailDigit :: Digit a -> Digit a
rtailDigit (Two a _) = One a
rtailDigit (Three a b _) = Two a b
rtailDigit (Four a b c _) = Three a b c

digitToTree :: (Measured v a) => Digit a -> FingerTree v a
digitToTree (One a) = Single a
digitToTree (Two a b) = deep (One a) Empty (One b)
digitToTree (Three a b c) = deep (Two a b) Empty (One c)
digitToTree (Four a b c d) = deep (Two a b) Empty (Two c d)

----------------
-- Concatenation
----------------

-- | /O(log(min(n1,n2)))/. Concatenate two sequences.
(><) :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
(><) =  appendTree0

appendTree0 :: (Measured v a) => FingerTree v a -> FingerTree v a -> FingerTree v a
appendTree0 Empty xs =
	xs
appendTree0 xs Empty =
	xs
appendTree0 (Single x) xs =
	x <| xs
appendTree0 xs (Single x) =
	xs |> x
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
	a <| xs
appendTree1 xs a Empty =
	xs |> a
appendTree1 (Single x) a xs =
	x <| a <| xs
appendTree1 xs a (Single x) =
	xs |> a |> x
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
	a <| b <| xs
appendTree2 xs a b Empty =
	xs |> a |> b
appendTree2 (Single x) a b xs =
	x <| a <| b <| xs
appendTree2 xs a b (Single x) =
	xs |> a |> b |> x
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
	a <| b <| c <| xs
appendTree3 xs a b c Empty =
	xs |> a |> b |> c
appendTree3 (Single x) a b c xs =
	x <| a <| b <| c <| xs
appendTree3 xs a b c (Single x) =
	xs |> a |> b |> c |> x
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
	a <| b <| c <| d <| xs
appendTree4 xs a b c d Empty =
	xs |> a |> b |> c |> d
appendTree4 (Single x) a b c d xs =
	x <| a <| b <| c <| d <| xs
appendTree4 xs a b c d (Single x) =
	xs |> a |> b |> c |> d |> x
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

----------------
-- 4.4 Splitting
----------------

-- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- on the accumulated measure changes from 'False' to 'True'.
split ::  (Measured v a) =>
          (v -> Bool) -> FingerTree v a -> (FingerTree v a, FingerTree v a)
split _p Empty  =  (Empty, Empty)
split p xs
  | p (measure xs) =  (l, x <| r)
  | otherwise	=  (xs, Empty)
  where Split l x r = splitTree p mempty xs

takeUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
takeUntil p  =  fst . split p

dropUntil :: (Measured v a) => (v -> Bool) -> FingerTree v a -> FingerTree v a
dropUntil p  =  snd . split p

data Split t a = Split t a t

splitTree ::	(Measured v a) =>
		(v -> Bool) -> v -> FingerTree v a -> Split (FingerTree v a) a
splitTree _p _i (Single x) = Split Empty x Empty
splitTree p i (Deep _ pr m sf)
  | p vpr	=  let	Split l x r	=  splitDigit p i pr
		   in	Split (maybe Empty digitToTree l) x (deepL r m sf)
  | p vm	=  let	Split ml xs mr	=  splitTree p vpr m
			Split l x r	=  splitNode p (vpr `mappendVal` ml) xs
		   in	Split (deepR pr  ml l) x (deepL r mr sf)
  | otherwise	=  let	Split l x r	=  splitDigit p vm sf
		   in	Split (deepR pr  m  l) x (maybe Empty digitToTree r)
  where	vpr	=  i    `mappend`  measure pr
	vm	=  vpr  `mappendVal` m

-- Avoid relying on right identity (cf Exercise 7)
mappendVal :: (Measured v a) => v -> FingerTree v a -> v
mappendVal v Empty = v
mappendVal v t = v `mappend` measure t

deepL          ::  (Measured v a) =>
	Maybe (Digit a) -> FingerTree v (Node v a) -> Digit a -> FingerTree v a
deepL Nothing m sf	=   case viewl m of
	EmptyL	->  digitToTree sf
	a :< m'	->  deep (nodeToDigit a) m' sf
deepL (Just pr) m sf	=   deep pr m sf

deepR          ::  (Measured v a) =>
	Digit a -> FingerTree v (Node v a) -> Maybe (Digit a) -> FingerTree v a
deepR pr m Nothing	=   case viewr m of
	EmptyR	->  digitToTree pr
	m' :> a	->  deep pr m' (nodeToDigit a)
deepR pr m (Just sf)	=   deep pr m sf

splitNode :: (Measured v a) => (v -> Bool) -> v -> Node v a ->
		Split (Maybe (Digit a)) a
splitNode p i (Node2 _ a b)
  | p va	= Split Nothing a (Just (One b))
  | otherwise	= Split (Just (One a)) b Nothing
  where	va	= i `mappend` measure a
splitNode p i (Node3 _ a b c)
  | p va	= Split Nothing a (Just (Two b c))
  | p vab	= Split (Just (One a)) b (Just (One c))
  | otherwise	= Split (Just (Two a b)) c Nothing
  where	va	= i `mappend` measure a
	vab	= va `mappend` measure b

splitDigit :: (Measured v a) => (v -> Bool) -> v -> Digit a ->
		Split (Maybe (Digit a)) a
splitDigit p i (One a) = i `seq` Split Nothing a Nothing
splitDigit p i (Two a b)
  | p va	= Split Nothing a (Just (One b))
  | otherwise	= Split (Just (One a)) b Nothing
  where	va	= i `mappend` measure a
splitDigit p i (Three a b c)
  | p va	= Split Nothing a (Just (Two b c))
  | p vab	= Split (Just (One a)) b (Just (One c))
  | otherwise	= Split (Just (Two a b)) c Nothing
  where	va	= i `mappend` measure a
	vab	= va `mappend` measure b
splitDigit p i (Four a b c d)
  | p va	= Split Nothing a (Just (Three b c d))
  | p vab	= Split (Just (One a)) b (Just (Two c d))
  | p vabc	= Split (Just (Two a b)) c (Just (One d))
  | otherwise	= Split (Just (Three a b c)) d Nothing
  where	va	= i `mappend` measure a
	vab	= va `mappend` measure b
        vabc	= vab `mappend` measure c

------------------
-- Transformations
------------------

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
