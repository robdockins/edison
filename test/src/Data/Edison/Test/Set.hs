-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- WARNING: The Set operations (insertWith...) are not adequately tested.
-- To be thorough, they should be tested on a type where distinguishable
-- values can still be "equal", and the results should be tested to make
-- sure that the "With" function was called on the right values.

module Data.Edison.Test.Set where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,foldl',
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude
import qualified Data.List as List -- not ListSeq!
import Data.Bits
import Data.Word

import Test.QuickCheck hiding( (===) )
import Test.HUnit (Test(..))

import Data.Edison.Prelude
import Data.Edison.Coll
import Data.Edison.Test.Utils
import qualified Data.Edison.Seq.ListSeq as L

import Data.Edison.Seq.JoinList (Seq)
import qualified Data.Edison.Seq.JoinList as S

----------------------------------------------------
-- Set implementations to test

import qualified Data.Edison.Coll.UnbalancedSet as US
import qualified Data.Edison.Coll.StandardSet as SS
import qualified Data.Edison.Coll.EnumSet as ES

-------------------------------------------------------
-- A utility class to propagate class contexts down
-- to the quick check properties

class (Eq (set a), Arbitrary (set a),
       Show (set a),
       Eq a, Ord a, Num a, Integral a, Real a,
       OrdSet (set a) a) => SetTest a set | set -> a

instance SetTest Int US.Set
instance SetTest Int SS.Set

newtype SmallInt = SI Int deriving (Show,Read,Eq,Ord,Enum,Num,Integral,Real)
instance Arbitrary SmallInt where
   arbitrary = arbitrary >>= \x -> return (SI $ abs x `mod` (finiteBitSize (0::Word) - 1))

instance CoArbitrary SmallInt where
   coarbitrary (SI x) = coarbitrary x

instance SetTest SmallInt ES.Set

--------------------------------------------------------
-- List all permutations of set types to test

allSetTests :: Test
allSetTests = TestList
   [ setTests (empty :: US.Set Int)
   , setTests (empty :: SS.Set Int)
   , setTests (empty :: ES.Set SmallInt)
   , qcTest $ prop_show_read (empty :: US.Set Int)
   , qcTest $ prop_show_read (empty :: ES.Set SmallInt)
   ]


---------------------------------------------------------
-- List all the tests to run for each type

setTests set = TestLabel ("Set Test "++(instanceName set)) . TestList $
   [ qcTest $ prop_single set
   , qcTest $ prop_single set
   , qcTest $ prop_fromSeq set
   , qcTest $ prop_insert set
   , qcTest $ prop_insertSeq set
   , qcTest $ prop_union set
   , qcTest $ prop_unionSeq set
   , qcTest $ prop_delete set
   , qcTest $ prop_deleteAll set
   , qcTest $ prop_deleteSeq set
   , qcTest $ prop_null_size set            -- 10
   , qcTest $ prop_member_count set
   , qcTest $ prop_toSeq set
   , qcTest $ prop_lookup set
   , qcTest $ prop_fold set
   , qcTest $ prop_strict_fold set
   , qcTest $ prop_filter_partition set
   , qcTest $ prop_deleteMin_Max set
   , qcTest $ prop_unsafeInsertMin_Max set
   , qcTest $ prop_unsafeFromOrdSeq set
   , qcTest $ prop_unsafeAppend set        -- 20
   , qcTest $ prop_filter set
   , qcTest $ prop_partition set
   , qcTest $ prop_minView_maxView set
   , qcTest $ prop_minElem_maxElem set
   , qcTest $ prop_foldr_foldl set
   , qcTest $ prop_strict_foldr_foldl set
   , qcTest $ prop_foldr1_foldl1 set
   , qcTest $ prop_strict_foldr1_foldl1 set
   , qcTest $ prop_toOrdSeq set
   , qcTest $ prop_intersect_difference set -- 30
   , qcTest $ prop_subset_subsetEq set
   , qcTest $ prop_fromSeqWith set
   , qcTest $ prop_insertWith set
   , qcTest $ prop_insertSeqWith set
   , qcTest $ prop_unionl_unionr_unionWith set
   , qcTest $ prop_unionSeqWith set
   , qcTest $ prop_intersectWith set
   , qcTest $ prop_unsafeMapMonotonic set
   , qcTest $ prop_symmetricDifference set
   , qcTest $ prop_strict set
   ]

-----------------------------------------------------
-- Utility operations


lmerge :: Ord a => [a] -> [a] -> [a]
lmerge xs [] = xs
lmerge [] ys = ys
lmerge xs@(x:xs') ys@(y:ys')
  | x < y     = x : lmerge xs' ys
  | y < x     = y : lmerge xs ys'
  | otherwise = x : lmerge xs' ys'


nub :: Eq a => [a] -> [a]
nub (x : xs@(x' : _)) = if x==x' then nub xs else x : nub xs
nub xs = xs

sort :: Ord a => [a] -> [a]
sort = nub . List.sort

(===) :: (Eq (set a),CollX (set a) a) => set a -> set a -> Bool
(===) s1 s2 = 
    structuralInvariant s1
    &&
    structuralInvariant s2
    &&
    s1 == s2

si :: CollX (set a) a => set a -> Bool
si = structuralInvariant

---------------------------------------------------------------
-- CollX operations

prop_single :: SetTest a set => set a -> a -> Bool
prop_single set x =
    let xs = singleton x `asTypeOf` set
     in si xs
        &&
        toOrdList xs == [x]

prop_fromSeq :: SetTest a set => set a -> Seq a -> Bool
prop_fromSeq set xs =
    let s = fromSeq xs `asTypeOf` set
     in si s
        &&
        toOrdList s == sort (S.toList xs)

prop_insert :: SetTest a set => set a -> a -> set a -> Bool
prop_insert set x xs =
    let insert_x_xs = insert x xs
     in si insert_x_xs
        &&
        if member x xs then
           toOrdList insert_x_xs == toOrdList xs
        else
           toOrdList insert_x_xs == List.insert x (toOrdList xs)

prop_insertSeq :: SetTest a set => set a -> Seq a -> set a -> Bool
prop_insertSeq set xs ys =
    insertSeq xs ys === union (fromSeq xs) ys

prop_union :: SetTest a set => set a -> set a -> set a -> Bool
prop_union set xs ys =
    let xys = union xs ys
     in si xys
        &&
        toOrdList xys == lmerge (toOrdList xs) (toOrdList ys)

prop_unionSeq :: SetTest a set => set a -> Seq (set a) -> Bool
prop_unionSeq set xss =
    unionSeq xss === S.foldr union empty xss

prop_delete :: SetTest a set => set a -> a -> set a -> Bool
prop_delete set x xs =
    let delete_x_xs = delete x xs
     in si delete_x_xs
        &&
        toOrdList delete_x_xs == List.delete x (toOrdList xs)

prop_deleteAll :: SetTest a set => set a -> a -> set a -> Bool
prop_deleteAll set x xs =
    deleteAll x xs === delete x xs

prop_deleteSeq :: SetTest a set => set a -> Seq a -> set a -> Bool
prop_deleteSeq set xs ys =
    deleteSeq xs ys === S.foldr delete ys xs

prop_null_size :: SetTest a set => set a -> set a -> Bool
prop_null_size set xs =
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (toOrdList xs)

prop_member_count :: SetTest a set => set a -> a -> set a -> Bool
prop_member_count set x xs =
    mem == not (Prelude.null (Prelude.filter (== x) (toOrdList xs)))
    &&
    count x xs == (if mem then 1 else 0)
  where mem = member x xs

---------------------------------------------------------------
-- Coll operations

prop_toSeq :: SetTest a set => set a -> set a -> Bool
prop_toSeq set xs =
    List.sort (S.toList (toSeq xs)) == toOrdList xs

prop_lookup :: SetTest a set => set a -> a -> set a -> Bool
prop_lookup set x xs =
    if member x xs then
      lookup x xs == x
      &&
      lookupM x xs == Just x
      &&
      lookupWithDefault 999 x xs == x
      &&
      lookupAll x xs == Prelude.take (count x xs) (repeat x)
    else
      lookupM x xs == Nothing
      &&
      lookupWithDefault 999 x xs == 999
      &&
      lookupAll x xs == []

prop_fold :: SetTest a set => set a -> set a -> Bool
prop_fold set xs =
    List.sort (fold (:) [] xs) == toOrdList xs
    &&
    (null xs || fold1 (+) xs == sum (toOrdList xs))

prop_strict_fold :: SetTest a set => set a -> set a -> Bool
prop_strict_fold set xs =
    fold' (+) 0 xs == fold (+) 0 xs
    &&
    (null xs || fold1' (+) xs == fold1 (+) xs)

prop_filter_partition :: SetTest a set => set a -> set a -> Bool
prop_filter_partition set xs =
    let filter_p_xs = filter p xs
        filter_not_p_xs = filter (not . p) xs
     in si filter_p_xs
        &&
        si filter_not_p_xs
        &&
        toOrdList filter_p_xs == Prelude.filter p (toOrdList xs)
        &&
        partition p xs == (filter_p_xs, filter_not_p_xs)
  where p x = x `mod` 3 == 2

------------------------------------------------------------------
-- OrdCollX operations

prop_deleteMin_Max :: SetTest a set => set a -> set a -> Bool
prop_deleteMin_Max set xs =
    let deleteMin_xs = deleteMin xs
        deleteMax_xs = deleteMax xs
     in si deleteMin_xs
        &&
        si deleteMax_xs
        &&
        toOrdList (deleteMin xs) == 
                 (let l = toOrdList xs 
                   in if L.null l then L.empty else L.ltail l)
        &&
        toOrdList (deleteMax xs) ==
                 (let l = toOrdList xs
                   in if L.null l then L.empty else L.rtail l)


prop_unsafeInsertMin_Max :: SetTest a set => 
        set a -> a -> set a -> Bool
prop_unsafeInsertMin_Max set i xs =
    if null xs then
      unsafeInsertMin 0 xs === singleton 0
      &&
      unsafeInsertMax 0 xs === singleton 0
    else
      unsafeInsertMin lo (delete lo xs) === xs
      &&
      unsafeInsertMax hi (delete hi xs) === xs
  where lo = minElem xs
        hi = maxElem xs

prop_unsafeFromOrdSeq :: SetTest a set => set a -> [a] -> Bool
prop_unsafeFromOrdSeq set xs =
    unsafeFromOrdSeq (sort xs) === (fromSeq xs `asTypeOf` set)

prop_unsafeAppend :: SetTest a set => 
        set a -> a -> set a -> Bool
prop_unsafeAppend set i xs =
       union ys zs === unsafeAppend ys zs
    where (ys,zs) = partitionLE_GT i xs


prop_filter :: SetTest a set => set a -> a -> set a -> Bool
prop_filter set x xs =
    si setLT && si setLE && si setGT && si setGE
    &&
    toOrdList setLT == Prelude.filter (< x) (toOrdList xs)
    &&
    toOrdList setLE == Prelude.filter (<= x) (toOrdList xs)
    &&
    toOrdList setGT == Prelude.filter (> x) (toOrdList xs)
    &&
    toOrdList setGE == Prelude.filter (>= x) (toOrdList xs)

 where setLT = filterLT x xs
       setLE = filterLE x xs
       setGT = filterGT x xs
       setGE = filterGE x xs

prop_partition :: SetTest a set => set a -> a -> set a -> Bool
prop_partition set x xs =
    partitionLT_GE x xs == (filterLT x xs, filterGE x xs)
    &&
    partitionLE_GT x xs == (filterLE x xs, filterGT x xs)
    &&
    partitionLT_GT x xs == (filterLT x xs, filterGT x xs)

-- OrdColl operations

prop_minView_maxView :: SetTest a set => set a -> set a -> Bool
prop_minView_maxView set xs =
    minView xs == (if null xs then Nothing
                              else Just (minElem xs, deleteMin xs))
    &&
    maxView xs == (if null xs then Nothing
                              else Just (maxElem xs, deleteMax xs))

prop_minElem_maxElem :: SetTest a set => set a -> set a -> Property
prop_minElem_maxElem set xs =
    not (null xs) ==>
      minElem xs == Prelude.head (toOrdList xs)
      &&
      maxElem xs == Prelude.last (toOrdList xs)

prop_foldr_foldl :: SetTest a set => set a -> set a -> Bool
prop_foldr_foldl set xs =
    foldr (:) [] xs == toOrdList xs
    &&
    foldl (flip (:)) [] xs == Prelude.reverse (toOrdList xs)

prop_strict_foldr_foldl :: SetTest a set => set a -> set a -> Bool
prop_strict_foldr_foldl set xs =
    foldr' (+) 0 xs == foldr (+) 0 xs
    &&
    foldl' (+) 0 xs == foldl (+) 0 xs

prop_foldr1_foldl1 :: SetTest a set => set a -> set a -> Property
prop_foldr1_foldl1 set xs =
    not (null xs) ==>
      foldr1 f xs == foldr f 1333 xs
      &&
      foldl1 (flip f) xs == foldl (flip f) 1333 xs
  where f x 1333 = x
        f x y = 3*x - 7*y

prop_strict_foldr1_foldl1 :: SetTest a set => set a -> set a -> Property
prop_strict_foldr1_foldl1 set xs =
    not (null xs) ==>
       foldr1' (+) xs == foldr1 (+) xs
       &&
       foldl1' (+) xs == foldl1 (+) xs

prop_toOrdSeq :: SetTest a set => set a -> set a -> Bool
prop_toOrdSeq set xs =
    S.toList (toOrdSeq xs) == toOrdList xs

-----------------------------------------------------------------------
-- SetX operations

prop_intersect_difference :: SetTest a set => 
        set a -> set a -> set a -> Bool
prop_intersect_difference set xs ys =
    intersection xs ys === filter (\x -> member x xs) ys
    &&
    difference xs ys === filter (\x -> not (member x ys)) xs

prop_subset_subsetEq :: SetTest a set => 
        set a -> set a -> set a -> Bool
prop_subset_subsetEq set xs ys =
    properSubset xs ys == (subset xs ys && xs /= ys)
    &&
    subset xs ys == (intersection xs ys == xs)


--------------------------------------------------------------------------
-- Set operations

prop_fromSeqWith :: SetTest a set => set a -> Seq a -> Bool
prop_fromSeqWith set xs =
    fromSeqWith const xs === (fromSeq xs `asTypeOf` set)

prop_insertWith :: SetTest a set => set a -> a -> set a -> Bool
prop_insertWith set x xs =
    insertWith const x xs === insert x xs

prop_insertSeqWith :: SetTest a set => set a -> Seq a -> set a -> Bool
prop_insertSeqWith set xs ys =
    insertSeqWith const xs ys === insertSeq xs ys

prop_unionl_unionr_unionWith :: SetTest a set => 
        set a -> set a -> set a -> Bool
prop_unionl_unionr_unionWith set xs ys =
    unionl xs ys === u
    &&
    unionr xs ys === u
    &&
    unionWith const xs ys === u
  where u = union xs ys

prop_unionSeqWith :: SetTest a set => set a -> Seq (set a) -> Bool
prop_unionSeqWith set xss =
    unionSeqWith const xss === unionSeq xss

prop_intersectWith :: SetTest a set => set a -> set a -> set a -> Bool
prop_intersectWith set xs ys =
    intersectionWith const xs ys === intersection xs ys

prop_unsafeMapMonotonic :: SetTest a set => set a -> set a -> Bool
prop_unsafeMapMonotonic set xs =
   if null xs
      then True
      else let xs' = deleteMax xs 
            in toOrdList (unsafeMapMonotonic (+1) xs') == Prelude.map (+1) (toOrdList xs')

prop_show_read :: (SetTest a set,Read (set a),Show (set a)) 
               => set a -> set a -> Bool
prop_show_read set xs = xs === read (show xs)


prop_symmetricDifference :: SetTest a set => set a -> set a -> set a -> Bool
prop_symmetricDifference set xs ys =
   union (difference xs ys) (difference ys xs) === symmetricDifference xs ys

prop_strict :: SetTest a set => set a -> set a -> Bool
prop_strict set xs =
   strict xs === xs
   &&
   strictWith (+1) xs === xs
