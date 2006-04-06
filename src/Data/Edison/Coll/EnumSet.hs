-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Edison.Coll.EnumSet
-- Copyright   :  (c) David F. Place 2006
-- Derived from Data.Set by Daan Leijen
-- License     :  BSD
-- Maintainer  :  David F. Place
-- Stability   :  Experimental
-- Portability :  portable
--
-- An efficient implementation of sets over small enumerations.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- >  import EnumSet as Set
--
-- The implementation of 'EnumSet' is based on bit-wise operations.
-----------------------------------------------------------------------------
{-
Copyright (c) 2006, David F. Place
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    
* Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in
    the documentation and/or other materials provided with the
    distribution.


* Neither the name of David F. Place nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-} 

-- | EnumSet: Efficient sets for small enumerations.
module Data.Edison.Coll.EnumSet (
            -- * Set type
            Set          
            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , isSubsetOf
            , isProperSubsetOf
            
            -- * Construction
            , empty
            , singleton
            , insert
            , delete
            
            -- * Combine
            , union, unions
            , difference
            , intersection
            , complement
            , complementWith

            -- * Filter
            , filter
            , partition
            , split
            , splitMember

            -- * Map
	    , map
	    , mapMonotonic

            -- * Fold
            , fold

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax

            -- * Conversion

            -- ** List
            , elems
            , toList
            , fromList
            
            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList
)  where
import Prelude hiding (filter,foldr,null,map)
import Data.Bits hiding (complement)
import Data.Word
import Data.List (foldl',intersperse,sort)
import Data.Monoid (Monoid(..))

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

(\\) :: Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Sets are bit strings of width wordLength.
--------------------------------------------------------------------}
-- | A set of values @a@ implemented as bitwise operations.  Useful
-- for members of class Enum with no more elements than there are bits 
-- in @Word@.
newtype Set a = Set Word deriving (Eq)

wordLength :: Int
wordLength = foldBits f 0 (maxBound::Word)
    where
      f z _ = z+1
    
check :: String -> Int -> Int 
check msg x  
    | x < wordLength = x
    | otherwise = error $ "EnumSet."++msg++"` beyond word size."


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is this the empty set?
null :: Set a -> Bool
null (Set 0) = True
null _       = False

-- | /O(1)/. The number of elements in the set.
size :: Set a -> Int
size (Set w) = foldBits f 0 w
    where
      f z _ = z+1

-- | /O(1)/. Is the element in the set?
member :: Enum a => a -> Set a -> Bool
member x (Set w) = testBit w $ fromEnum x

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: Set a
empty = Set 0

-- | /O(1)/. Create a singleton set.
singleton :: Enum a => a -> Set a
singleton x =
    Set $ setBit 0 $ check "singleton" $ fromEnum x

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(1)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: Enum a => a -> Set a -> Set a
insert x (Set w) =
    Set $ setBit w $ check "insert" $ fromEnum x

-- | /O(1)/. Delete an element from a set.
delete :: Enum a => a -> Set a -> Set a
delete x (Set w) = 
    Set $ clearBit w $ fromEnum x

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(1)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Set a -> Set a -> Bool
isProperSubsetOf x y = (x /= y) && (isSubsetOf x y)

-- | /O(1)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Set a -> Set a -> Bool
isSubsetOf x y = (x `union` y) == y

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(n)/. The minimal element of a set.
findMin :: Enum a => Set a -> a
findMin (Set w) = toEnum $ findMinIndex w

findMinIndex :: Word -> Int
findMinIndex 0 = 
    error "EnumSet.findMin: empty set has no minimal element"
findMinIndex w = f 0 w
    where 
      f i w
          | 1 == (w .&. 1) = i
          | otherwise = f (i+1) (w `shiftR` 1)

-- | /O(n)/. The maximal element of a set.
findMax :: Enum a => Set a -> a
findMax (Set w) = toEnum $ findMaxIndex w

findMaxIndex :: Word -> Int
findMaxIndex 0 = 
    error "EnumSet.findMax: empty set has no maximal element"
findMaxIndex w = foldBits (\_ i -> i) 0 w

-- | /O(n)/. Delete the minimal element.
deleteMin :: Set a -> Set a
deleteMin (Set 0) = empty
deleteMin (Set w) = Set $ clearBit w $ findMinIndex w

-- | /O(n)/. Delete the maximal element.
deleteMax :: Set a -> Set a
deleteMax (Set 0) = empty
deleteMax (Set w) = Set $ clearBit w $ findMaxIndex w

deleteFindMin :: Enum a => Set a -> (a,Set a)
deleteFindMin s@(Set 0) = 
    (error 
     "EnumSet.deleteFindMin: can not return the minimal element of an empty set", 
     s)
deleteFindMin s = (min,delete min s)
    where min = findMin s

deleteFindMax :: Enum a => Set a -> (a,Set a)
deleteFindMax s@(Set 0) = 
    (error 
     "EnumSet.deleteFindMax: can not return the maximal element of an empty set", 
     s)
deleteFindMax s = (max,delete max s)
    where max = findMax s

{--------------------------------------------------------------------
  Union. 
--------------------------------------------------------------------}
-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: [Set a] -> Set a
unions = foldl' union empty

-- | /O(1)/. The union of two sets.
union :: Set a -> Set a -> Set a
union (Set x) (Set y) = Set $ x .|. y


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(1)/. Difference of two sets. 
difference :: Set a -> Set a -> Set a
difference (Set x) (Set y) = Set $ (x .|. y) `xor` y

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(1)/. The intersection of two sets.
intersection :: Set a -> Set a -> Set a
intersection (Set x) (Set y) = Set $ x .&. y

{--------------------------------------------------------------------
  Complement
--------------------------------------------------------------------}
-- | /O(1). The complement of a set with its universe set. @complement@ can be used with bounded types for which the universe set
-- will be automatically created.
complement :: (Bounded a, Enum a) => Set a -> Set a
complement x = complementWith u x
    where u = (fromList [minBound .. maxBound]) `asTypeOf` x

complementWith :: Set a -> Set a -> Set a
complementWith (Set u) (Set x) = Set $ u `xor` x

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Enum a => (a -> Bool) -> Set a -> Set a
filter p (Set w) = Set $ foldBits f 0 w
    where 
      f z i 
        | p $ toEnum i = setBit z i
        | otherwise = z

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: Enum a => (a -> Bool) -> Set a -> (Set a,Set a)
partition p (Set w) = (Set yay,Set nay)
    where 
      (yay,nay) = foldBits f (0,0) w
      f (x,y) i 
          | p $ toEnum i = (setBit x i,y)
          | otherwise = (x,setBit y i)

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}
-- | /O(n)/. 
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
-- 
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (Enum a,Enum b) => (a -> b) -> Set a -> Set b
map f (Set w) = Set $ foldBits fold 0 w
    where 
      fold z i = setBit z $ check "map" $ fromEnum $ f (toEnum i)

-- | @'mapMonotonic'@ is provided for compatibility with the 
-- Data.Set interface.
mapMonotonic :: (Enum a,Enum b) => (a -> b) -> Set a -> Set b
mapMonotonic = map

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements of a set in an unspecified order.
fold :: Enum a => (b -> a -> b) -> b -> Set a -> b
fold f z (Set w) = foldBits folder z w
    where
      folder z i = f z $ toEnum i

foldr :: (Enum a) => (a -> c -> c) -> c -> Set a -> c
foldr f = fold (flip f)

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/. The elements of a set.
elems :: Enum a => Set a -> [a]
elems = toList

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements.
toList :: Enum a => Set a -> [a]
toList (Set w) = reverse $ foldBits f [] w
    where
      f z i = (toEnum i) : z

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: (Ord a,Enum a) => Set a -> [a]
toAscList = sort . toList

-- | /O(n)/. Create a set from a list of elements.
fromList :: Enum a => [a] -> Set a
fromList xs = Set $ foldl' f 0 xs
    where 
      f z x = setBit z $ check "fromList" $ fromEnum x
-- | @fromAscList@ and @fromDistinctAscList@ maintained for compatibility
-- with Data.Set, but here give no advantage.
fromAscList :: Enum a => [a] -> Set a
fromAscList = fromList

fromDistinctAscList :: Enum a => [a] -> Set a
fromDistinctAscList = fromList

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (Enum a, Show a) => Show (Set a) where
    show xs = 
        "{"++(concat $ intersperse "," [show x | x <- toList xs])++"}"

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
split :: (Ord a, Enum a) => a -> Set a -> (Set a,Set a)
split x s = (lesser,greater)
    where (lesser,_,greater) = splitMember x s

splitMember :: (Ord a, Enum a) => a -> Set a -> (Set a,Bool,Set a)
splitMember x (Set w) = (Set lesser,isMember,Set greater)
    where
      (lesser,isMember,greater) = foldBits f (0,False,0) w
      f (lesser,isMember,greater) i =
        case compare (toEnum i) x of
          GT -> (lesser,isMember,setBit greater i)
          LT -> (setBit lesser i,isMember,greater)
          EQ -> (lesser,True,greater)

{--------------------------------------------------------------------
  Utility functions. 
--------------------------------------------------------------------}

foldBits :: Bits c => (a -> Int -> a) -> a -> c -> a
foldbits _ z 0  = z
foldBits f z bs = foldBits' f 0 bs z

foldBits' :: Bits c => (a -> Int -> a) -> Int -> c -> a -> a
foldBits' f i bs z
    | bs == 0 = z
    | otherwise = z' `seq` foldBits' f i' bs' z'
    where z' | 1 == bs .&. 1 = f z i
             | otherwise =  z
          i' = i + 1
          bs' = bs `shiftR` 1

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}
instance (Enum a,Ord a) => Ord (Set a) where
    compare a b = compare (toAscList a) (toAscList b)

{--------------------------------------------------------------------
  Monoid
--------------------------------------------------------------------}
instance Enum a => Monoid (Set a) where
    mempty  = empty
    mappend = union
    mconcat = unions

