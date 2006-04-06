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
--
-- For this implementation to work as expected at type @A@, there are a number
-- of preconditions on the @Eq@, @Enum@ and @Ord@ instances.
--
-- The @Enum A@ instance must create a bijection between the elements of type @A@ and
-- a finite subset of the naturals [0,1,2,3....].  Also, the number of distinct elements
-- of @A@ must be less than or equal to the number of bits in @Word@.
--
-- The @Enum A@ instance must be consistent with the @Eq A@ instance. 
-- That is, we must have:
--
-- > forall x y::A, x == y <==> toEnum x == toEnum y 
--
-- Additionally, for operations that require an @Ord A@ context, we require that
-- toEnum be monotonic.  That is, we must have:
--
-- > forall x y::A, x < y <==> toEnum x < toEnum y
--
-- Derived @Eq@, @Ord@ and @Enum@ instances will fulfull these conditions, if
-- the enumerated type has sufficently few constructors.
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

	    -- * CollX operations
            , empty
            , singleton
            , fromSeq
            , insert
            , insertSeq
            , union
            , unionSeq
            , delete
            , deleteAll
            , deleteSeq
            , null
            , size
            , member
            , count

            -- * OrdCollX operations
            , deleteMin
            , deleteMax
            , unsafeInsertMin
            , unsafeInsertMax
            , unsafeFromOrdSeq
            , unsafeAppend
            , filterLT
            , filterLE
            , filterGT
            , filterGE
            , partitionLT_GE
            , partitionLE_GT
            , partitionLT_GT

            -- * SetX operations
            , intersection
            , difference
            , properSubset
            , subset

            -- * Coll operations
            , toSeq
            , lookup
            , lookupM
            , lookupAll
            , lookupWithDefault
            , fold, fold', fold1, fold1'
            , filter
            , partition     

            -- * OrdColl operations
            , minView
            , minElem
            , maxView
            , maxElem
            , foldr, foldr', foldl, foldl'
            , foldr1, foldr1', foldl1, foldl1'
            , toOrdSeq
            , unsafeMapMonotonic

            -- * Set operations
            , fromSeqWith
            , insertWith
            , insertSeqWith
            , unionl
            , unionr
            , unionWith
            , unionSeqWith
            , intersectionWith

            -- * Bonus operations
            , map
            , splitMember
            , complement
            , complementWith

            -- * Documenation
            , moduleName
)  where
import Prelude hiding (filter,foldl,foldr,null,map,lookup,foldl1,foldr1)
import qualified Data.Bits as Bits
import Data.Bits hiding (complement)
import Data.Word
import Data.Monoid (Monoid(..))

import Data.Edison.Prelude
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Seq.ListSeq as L
import Data.Edison.Coll.Defaults

moduleName = "Data.Edison.Coll.EnumSet"

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


-- no interesting structural invariants
structuralInvariant :: Set a -> Bool
structuralInvariant = const True


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
member :: (Eq a, Enum a) => a -> Set a -> Bool
member x (Set w) = testBit w $ fromEnum x

count :: (Eq a, Enum a) => a -> Set a -> Int
count = undefined {-countUsingMember-}

lookup :: (Eq a, Enum a) => a -> Set a -> a
lookup = lookupUsingLookupAll

lookupM :: (Eq a, Enum a, Monad m) => a -> Set a -> m a
lookupM x s
   | member x s = return x
   | otherwise  = fail (moduleName++".lookupM: lookup failed")

lookupAll  :: (Eq a, Enum a, S.Sequence s) => a -> Set a -> s a
lookupAll = undefined {-lookupAllUsingLookupM-}

lookupWithDefault :: (Eq a, Enum a) => a -> a -> Set a -> a
lookupWithDefault = lookupWithDefaultUsingLookupM

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: Set a
empty = Set 0

-- | /O(1)/. Create a singleton set.
singleton :: (Eq a, Enum a) => a -> Set a
singleton x =
    Set $ setBit 0 $ check "singleton" $ fromEnum x

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(1)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: (Eq a, Enum a) => a -> Set a -> Set a
insert x (Set w) =
    Set $ setBit w $ check "insert" $ fromEnum x

-- | /O(1)/. Delete an element from a set.
delete :: (Eq a, Enum a) => a -> Set a -> Set a
delete x (Set w) = 
    Set $ clearBit w $ fromEnum x

deleteAll :: (Eq a, Enum a) => a -> Set a -> Set a
deleteAll = delete

deleteSeq :: (Eq a, Enum a, S.Sequence s) => s a -> Set a -> Set a
deleteSeq = deleteSeqUsingDelete

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(1)/. Is this a proper subset? (ie. a subset but not equal).
properSubset :: Set a -> Set a -> Bool
properSubset x y = (x /= y) && (subset x y)

-- | /O(1)/. Is this a subset?
-- @(s1 `subset` s2)@ tells whether @s1@ is a subset of @s2@.
subset :: Set a -> Set a -> Bool
subset x y = (x `union` y) == y

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
findMinIndex :: Word -> Int
findMinIndex 0 = 
    error "EnumSet.findMin: empty set has no minimal element"
findMinIndex w = f 0 w
    where 
      f i w
          | 1 == (w .&. 1) = i
          | otherwise = f (i+1) (w `Bits.shiftR` 1)

findMaxIndex :: Word -> Int
findMaxIndex 0 = 
    error "EnumSet.findMax: empty set has no maximal element"
findMaxIndex w = foldBits (\_ i -> i) 0 w


-- | /O(n)/. The minimal element of a set.
minElem :: (Eq a, Enum a) => Set a -> a
minElem (Set w) = toEnum $ findMinIndex w


-- | /O(n)/. The maximal element of a set.
maxElem :: (Eq a, Enum a) => Set a -> a
maxElem (Set w) = toEnum $ findMaxIndex w


-- | /O(n)/. Delete the minimal element.
deleteMin :: (Ord a, Enum a) => Set a -> Set a
deleteMin (Set 0) = empty
deleteMin (Set w) = Set $ clearBit w $ findMinIndex w

-- | /O(n)/. Delete the maximal element.
deleteMax :: (Ord a, Enum a) => Set a -> Set a
deleteMax (Set 0) = empty
deleteMax (Set w) = Set $ clearBit w $ findMaxIndex w

minView :: (Eq a, Enum a, Monad m) => Set a -> m (a, Set a)
minView s@(Set 0) = fail (moduleName++".minView: empty set")
minView s = return (min,delete min s)
    where min = minElem s

maxView :: (Eq a, Enum a, Monad m) => Set a -> m (a, Set a)
maxView s@(Set 0) = fail (moduleName++".maxView: empty set")
maxView s = return (max,delete max s)
    where max = maxElem s

unsafeInsertMin :: (Ord a, Enum a) => a -> Set a -> Set a
unsafeInsertMin = insert

unsafeInsertMax :: (Ord a, Enum a) => a -> Set a -> Set a
unsafeInsertMax = insert

unsafeAppend :: (Ord a, Enum a) => Set a -> Set a -> Set a
unsafeAppend = union

unsafeFromOrdSeq :: (Ord a, Enum a, S.Sequence s) => s a -> Set a
unsafeFromOrdSeq = fromSeq

lowMask :: Int -> Word
lowMask x = (Bits.shiftL 1 x) - 1

highMask :: Int -> Word
highMask x = Bits.complement (lowMask x)

filterLT :: (Ord a, Enum a) => a -> Set a -> Set a
filterLT x (Set w) = Set (w .&. lowMask (fromEnum x))

filterLE :: (Ord a, Enum a) => a -> Set a -> Set a
filterLE x (Set w) = Set (w .&. lowMask (fromEnum x + 1))

filterGT :: (Ord a, Enum a) => a -> Set a -> Set a
filterGT x (Set w) = Set (w .&. highMask (fromEnum x + 1))

filterGE :: (Ord a, Enum a) => a -> Set a -> Set a
filterGE x (Set w) = Set (w .&. highMask (fromEnum x))

partitionLT_GE :: (Ord a, Enum a) => a -> Set a -> (Set a, Set a)
partitionLT_GE x s = (filterLT x s,filterGE x s)

partitionLE_GT :: (Ord a, Enum a) => a -> Set a -> (Set a, Set a)
partitionLE_GT x s = (filterLE x s,filterGT x s)

partitionLT_GT :: (Ord a, Enum a) => a -> Set a -> (Set a, Set a)
partitionLT_GT x s = (filterLT x s,filterGT x s)


{--------------------------------------------------------------------
  Union. 
--------------------------------------------------------------------}
-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unionSeq :: (Eq a, Enum a, S.Sequence s) => s (Set a) -> Set a
unionSeq = unionSeqUsingFoldl'

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
complement :: (Eq a, Bounded a, Enum a) => Set a -> Set a
complement x = complementWith u x
    where u = (fromSeq [minBound .. maxBound]) `asTypeOf` x

complementWith :: Set a -> Set a -> Set a
complementWith (Set u) (Set x) = Set $ u `xor` x

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: (Eq a, Enum a) => (a -> Bool) -> Set a -> Set a
filter p (Set w) = Set $ foldBits f 0 w
    where 
      f z i 
        | p $ toEnum i = setBit z i
        | otherwise = z

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: (Eq a, Enum a) => (a -> Bool) -> Set a -> (Set a,Set a)
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
unsafeMapMonotonic :: (Enum a) => (a -> a) -> Set a -> Set a
unsafeMapMonotonic = map

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements of a set in an unspecified order.
fold :: (Eq a, Enum a) => (a -> b -> b) -> b -> Set a -> b
fold f z (Set w) = foldBits folder z w
    where
      folder z i = f (toEnum i) z

fold' :: (Eq a, Enum a) => (a -> b -> b) -> b -> Set a -> b
fold' = fold

fold1 :: (Eq a, Enum a) => (a -> a -> a) -> Set a -> a
fold1 f (Set w) = foldBits folder (toEnum min) (clearBit w min)
    where
      min = findMinIndex w
      folder z i = f (toEnum i) z

fold1' :: (Eq a, Enum a) => (a -> a -> a) -> Set a -> a
fold1' = fold1

foldr :: (Ord a, Enum a) => (a -> c -> c) -> c -> Set a -> c
foldr = fold

foldr' :: (Ord a, Enum a) => (a -> c -> c) -> c -> Set a -> c
foldr' = foldr

foldr1 :: (Ord a, Enum a) => (a -> a -> a) -> Set a -> a
foldr1 = fold1

foldr1' :: (Ord a, Enum a) => (a -> a -> a) -> Set a -> a
foldr1' = foldr1

foldl :: (Ord a, Enum a) => (c -> a -> c) -> c -> Set a -> c
foldl = undefined

foldl' :: (Ord a, Enum a) => (c -> a -> c) -> c -> Set a -> c
foldl' = foldl

foldl1 :: (Ord a, Enum a) => (a -> a -> a) -> Set a -> a
foldl1 = undefined

foldl1' :: (Ord a, Enum a) => (a -> a -> a) -> Set a -> a
foldl1' = foldl1


{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
fromSeq :: (Eq a, Enum a, S.Sequence s) => s a -> Set a
fromSeq xs = Set $ S.fold' f 0 xs
  where
     f x z = setBit z $ check "fromSeq" $ fromEnum x

fromOrdSeq :: (Ord a, Enum a, S.Sequence s) => s a -> Set a
fromOrdSeq = fromSeq

insertSeq :: (Eq a, Enum a, S.Sequence s) => s a -> Set a -> Set a
insertSeq = insertSeqUsingUnion

toSeq :: (Eq a, Enum a, S.Sequence s) => Set a -> s a
toSeq (Set w) = S.reverse $ foldBits f S.empty w
  where
     f z i = S.lcons (toEnum i) z

toOrdSeq :: (Ord a, Enum a, S.Sequence s) => Set a -> s a
toOrdSeq = toSeq

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
{-
instance (Enum a, Show a) => Show (Set a) where
    show xs = 
        "{"++(concat $ intersperse "," [show x | x <- toList xs])++"}"
-}

{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
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
foldBits _ z 0  = z
foldBits f z bs = foldBits' f 0 bs z

foldBits' :: Bits c => (a -> Int -> a) -> Int -> c -> a -> a
foldBits' f i bs z
    | bs == 0 = z
    | otherwise = z' `seq` foldBits' f i' bs' z'
    where z' | 1 == bs .&. 1 = f z i
             | otherwise =  z
          i' = i + 1
          bs' = bs `Bits.shiftR` 1

{--------------------------------------------------------------------
  Ord 
--------------------------------------------------------------------}
{-
instance (Enum a,Ord a) => Ord (Set a) where
    compare a b = compare (toAscList a) (toAscList b)
-}

{--------------------------------------------------------------------
  Monoid
--------------------------------------------------------------------}
instance (Eq a, Enum a) => Monoid (Set a) where
    mempty  = empty
    mappend = union
    mconcat = unionSeq

instance (Eq a, Enum a) => C.CollX (Set a) a where
  {empty = empty; singleton = singleton; fromSeq = fromSeq; insert = insert;
   insertSeq = insertSeq; union = union; unionSeq = unionSeq; 
   delete = delete; deleteAll = deleteAll; deleteSeq = deleteSeq;
   null = null; size = size; member = member; count = count;
   structuralInvariant = structuralInvariant; instanceName c = moduleName}

instance (Ord a, Enum a) => C.OrdCollX (Set a) a where  
  {deleteMin = deleteMin; deleteMax = deleteMax; 
   unsafeInsertMin = unsafeInsertMin; unsafeInsertMax = unsafeInsertMax; 
   unsafeFromOrdSeq = unsafeFromOrdSeq; unsafeAppend = unsafeAppend; 
   filterLT = filterLT; filterLE = filterLE; filterGT = filterGT; 
   filterGE = filterGE; partitionLT_GE = partitionLT_GE; 
   partitionLE_GT = partitionLE_GT; partitionLT_GT = partitionLT_GT}

instance (Eq a, Enum a) => C.SetX (Set a) a where
  {intersection = intersection; difference = difference;
   properSubset = properSubset; subset = subset}

instance (Eq a, Enum a) => C.Coll (Set a) a where
  {toSeq = toSeq; lookup = lookup; lookupM = lookupM; 
   lookupAll = lookupAll; lookupWithDefault = lookupWithDefault; 
   fold = fold; fold' = fold'; fold1 = fold1; fold1' = fold1';
   filter = filter; partition = partition}

instance (Ord a, Enum a) => C.OrdColl (Set a) a where
  {minView = minView; minElem = minElem; maxView = maxView; 
   maxElem = maxElem; foldr = foldr; foldr' = foldr'; 
   foldl = foldl; foldl' = foldl'; foldr1 = foldr1; foldr1' = foldr1';
   foldl1 = foldl1; foldl1' = foldl1'; toOrdSeq = toOrdSeq;
   unsafeMapMonotonic = unsafeMapMonotonic}
