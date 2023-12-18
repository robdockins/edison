-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.Bag where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude
import qualified Data.List as List -- not ListSeq!

import Test.QuickCheck hiding( (===) )
import Test.HUnit (Test(..))

import Data.Edison.Prelude
import Data.Edison.Coll
import Data.Edison.Test.Utils
import qualified Data.Edison.Seq.ListSeq as L

import Data.Edison.Seq.JoinList (Seq)
import qualified Data.Edison.Seq.JoinList as S

--------------------------------------------------------------
-- Bag implementations to test

import qualified Data.Edison.Coll.LazyPairingHeap as LPH
import qualified Data.Edison.Coll.LeftistHeap as LH
import qualified Data.Edison.Coll.SkewHeap as SkH
import qualified Data.Edison.Coll.SplayHeap as SpH
import qualified Data.Edison.Coll.MinHeap as Min


---------------------------------------------------------------
-- A utility class to propagate class contexts down
-- to the quick check properties

class (Eq (bag a), Arbitrary (bag a),
       Show (bag a), Read (bag a),
       OrdColl (bag a) a) => BagTest a bag

instance (Ord a, Show a, Read a, Arbitrary a) => BagTest a LPH.Heap
instance (Ord a, Show a, Read a, Arbitrary a) => BagTest a LH.Heap
instance (Ord a, Show a, Read a, Arbitrary a) => BagTest a SkH.Heap
instance (Ord a, Show a, Read a, Arbitrary a) => BagTest a SpH.Heap
instance (Ord a, Show a, Read a, Arbitrary a, BagTest a bag)
   => BagTest a (Min.Min (bag a))

--------------------------------------------------------------
-- List all permutations of bag types to test

allBagTests :: Test
allBagTests = TestList
   [ bagTests (empty :: Ord a => LPH.Heap a)
   , bagTests (empty :: Ord a => Min.Min (LPH.Heap a) a)
   , bagTests (empty :: Ord a => LH.Heap a)
   , bagTests (empty :: Ord a => Min.Min (LH.Heap a) a)
   , bagTests (empty :: Ord a => SkH.Heap a)
   , bagTests (empty :: Ord a => Min.Min (SkH.Heap a) a)
   , bagTests (empty :: Ord a => SpH.Heap a)
   , bagTests (empty :: Ord a => Min.Min (SpH.Heap a) a)
   , bagTests (empty :: Ord a => Min.Min (Min.Min (LPH.Heap a) a) a)
   ]

---------------------------------------------------------------
-- List all the tests to run for each type

bagTests bag = TestLabel ("Bag test "++(instanceName bag)) . TestList $
   [ qcTest $ prop_single bag
   , qcTest $ prop_fromSeq bag
   , qcTest $ prop_insert bag
   , qcTest $ prop_insertSeq bag
   , qcTest $ prop_union bag
   , qcTest $ prop_unionSeq bag
   , qcTest $ prop_delete bag
   , qcTest $ prop_deleteAll bag
   , qcTest $ prop_deleteSeq bag
   , qcTest $ prop_null_size bag         -- 10
   , qcTest $ prop_member_count bag
   , qcTest $ prop_toSeq bag
   , qcTest $ prop_lookup bag
   , qcTest $ prop_fold bag
   , qcTest $ prop_strict_fold bag
   , qcTest $ prop_filter_partition bag
   , qcTest $ prop_deleteMin_Max bag
   , qcTest $ prop_unsafeInsertMin_Max bag
   , qcTest $ prop_unsafeFromOrdSeq bag
   , qcTest $ prop_filter bag            -- 20
   , qcTest $ prop_partition bag
   , qcTest $ prop_minView_maxView bag
   , qcTest $ prop_minElem_maxElem bag
   , qcTest $ prop_foldr_foldl bag
   , qcTest $ prop_strict_foldr_foldl bag
   , qcTest $ prop_foldr1_foldl1 bag
   , qcTest $ prop_strict_foldr1_foldl1 bag
   , qcTest $ prop_toOrdSeq bag
   , qcTest $ prop_unsafeAppend bag
   , qcTest $ prop_unsafeMapMonotonic bag -- 30
   , qcTest $ prop_read_show bag
   , qcTest $ prop_strict bag
   ]

----------------------------------------------------
-- utility operations

lmerge :: [Int] -> [Int] -> [Int]
lmerge xs [] = xs
lmerge [] ys = ys
lmerge xs@(x:xs') ys@(y:ys')
  | x <= y    = x : lmerge xs' ys
  | otherwise = y : lmerge xs ys'


(===) :: (Eq (bag a),CollX (bag a) a) => bag a -> bag a -> Bool
(===) b1 b2 = 
    structuralInvariant b1
    &&
    structuralInvariant b2
    &&
    b1 == b2

si :: CollX (bag a) a => bag a -> Bool
si = structuralInvariant

-----------------------------------------------------
-- CollX operations

prop_single :: BagTest Int bag => bag Int -> Int -> Bool
prop_single bag x =
    let xs = singleton x `asTypeOf` bag
     in si xs
        &&
        toOrdList xs == [x]

prop_fromSeq :: BagTest Int bag => bag Int -> Seq Int -> Bool
prop_fromSeq bag xs =
    fromSeq xs `asTypeOf` bag === S.foldr insert empty xs

prop_insert :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_insert bag x xs =
    let x_xs = insert x xs
     in si x_xs
        &&
        toOrdList x_xs == List.insert x (toOrdList xs)

prop_insertSeq :: BagTest Int bag => bag Int -> Seq Int -> bag Int -> Bool
prop_insertSeq bag xs ys =
    insertSeq xs ys === union (fromSeq xs) ys

prop_union :: BagTest Int bag => bag Int -> bag Int -> bag Int -> Bool
prop_union bag xs ys =
    let xys = union xs ys
     in si xys
        &&
        toOrdList xys == lmerge (toOrdList xs) (toOrdList ys)

prop_unionSeq :: BagTest Int bag => bag Int -> Seq (bag Int) -> Bool
prop_unionSeq bag xss =
    unionSeq xss === S.foldr union empty xss

prop_delete :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_delete bag x xs =
    let del_x_xs = delete x xs
     in si del_x_xs
        &&
        toOrdList del_x_xs == List.delete x (toOrdList xs)

prop_deleteAll :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_deleteAll bag x xs =
    let del_x_xs = deleteAll x xs
     in si del_x_xs
        &&
        toOrdList del_x_xs == Prelude.filter (/= x) (toOrdList xs)

prop_deleteSeq :: BagTest Int bag => bag Int -> Seq Int -> bag Int -> Bool
prop_deleteSeq bag xs ys =
    deleteSeq xs ys === S.foldr delete ys xs

prop_null_size :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_null_size bag xs =
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (toOrdList xs)

prop_member_count :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_member_count bag x xs =
    member x xs == (c > 0)
    &&
    c == Prelude.length (Prelude.filter (== x) (toOrdList xs))
  where c = count x xs

-------------------------------------------------------
-- Coll operations

prop_toSeq :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_toSeq bag xs =
    List.sort (S.toList (toSeq xs)) == toOrdList xs

prop_lookup :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_lookup bag x xs =
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

prop_fold :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_fold bag xs =
    List.sort (fold (:) [] xs) == toOrdList xs
    &&
    (null xs || fold1 (+) xs == sum (toOrdList xs))

prop_strict_fold :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_strict_fold bag xs =
    fold' (+) 0 xs == fold (+) 0 xs
    &&
    (null xs || fold1' (+) xs == fold1 (+) xs)

prop_filter_partition :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_filter_partition bag xs =
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

prop_deleteMin_Max :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_deleteMin_Max bag xs =
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

prop_unsafeInsertMin_Max :: BagTest Int bag => 
        bag Int -> Int -> bag Int -> Bool
prop_unsafeInsertMin_Max bag i xs =
    if null xs then
      unsafeInsertMin 0 xs === singleton 0
      &&
      unsafeInsertMax 0 xs === singleton 0
    else
      unsafeInsertMin lo xs === insert lo xs
      &&
      unsafeInsertMax hi xs === insert hi xs
  where lo = minElem xs - (if odd i then 1 else 0)
        hi = maxElem xs + (if odd i then 1 else 0)
    
prop_unsafeFromOrdSeq :: BagTest Int bag => bag Int -> [Int] -> Bool
prop_unsafeFromOrdSeq bag xs =
    si bag1
    &&
    toOrdList bag1 == xs'

  where xs' = List.sort xs
        bag1 = unsafeFromOrdSeq xs' `asTypeOf` bag

prop_filter :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_filter bag x xs =
    si bagLT && si bagLE && si bagGT && si bagGE
    &&
    toOrdList bagLT == Prelude.filter (< x) (toOrdList xs)
    &&
    toOrdList bagLE == Prelude.filter (<= x) (toOrdList xs)
    &&
    toOrdList bagGT == Prelude.filter (> x) (toOrdList xs)
    &&
    toOrdList bagGE == Prelude.filter (>= x) (toOrdList xs)

  where bagLT = filterLT x xs
        bagLE = filterLE x xs
        bagGT = filterGT x xs
        bagGE = filterGE x xs


prop_partition :: BagTest Int bag => bag Int -> Int -> bag Int -> Bool
prop_partition bag x xs =
    partitionLT_GE x xs == (filterLT x xs, filterGE x xs)
    &&
    partitionLE_GT x xs == (filterLE x xs, filterGT x xs)
    &&
    partitionLT_GT x xs == (filterLT x xs, filterGT x xs)


-----------------------------------------------------------------
-- OrdColl operations

prop_minView_maxView :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_minView_maxView bag xs =
    minView xs == (if null xs then Nothing
                              else Just (minElem xs, deleteMin xs))
    &&
    maxView xs == (if null xs then Nothing
                              else Just (maxElem xs, deleteMax xs))

prop_minElem_maxElem :: BagTest Int bag => bag Int -> bag Int -> Property
prop_minElem_maxElem bag xs =
    not (null xs) ==>
      minElem xs == Prelude.head (toOrdList xs)
      &&
      maxElem xs == Prelude.last (toOrdList xs)

prop_foldr_foldl :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_foldr_foldl bag xs =
    foldr (:) [] xs == toOrdList xs
    &&
    foldl (flip (:)) [] xs == Prelude.reverse (toOrdList xs)

prop_strict_foldr_foldl :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_strict_foldr_foldl bag xs =
    foldr' (+) 0 xs == foldr (+) 0 xs
    &&
    foldl' (+) 0 xs == foldl (+) 0 xs

prop_foldr1_foldl1 :: BagTest Int bag => bag Int -> bag Int -> Property
prop_foldr1_foldl1 bag xs =
    not (null xs) ==>
      foldr1 f xs == foldr f 1333 xs
      &&
      foldl1 (flip f) xs == foldl (flip f) 1333 xs
  where f x 1333 = x
        f x y = 3*x - 7*y

prop_strict_foldr1_foldl1 :: BagTest Int bag => bag Int -> bag Int -> Property
prop_strict_foldr1_foldl1 bag xs =
    not (null xs) ==>
       foldr1' (+) xs == foldr1 (+) xs
       &&
       foldl1' (+) xs == foldl1 (+) xs

prop_toOrdSeq :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_toOrdSeq bag xs =
    S.toList (toOrdSeq xs) == toOrdList xs

prop_unsafeAppend :: BagTest Int bag => 
        bag Int -> Int -> bag Int -> bag Int -> Bool
prop_unsafeAppend bag i xs ys =
    if null xs || null ys then
      unsafeAppend xs ys === union xs ys
    else
      unsafeAppend xs ys' === union xs ys'
  where delta = maxElem xs - minElem ys + (if odd i then 1 else 0)
        ys' = unsafeMapMonotonic (+delta) ys
  -- if unsafeMapMonotonic does any reorganizing in addition
  -- to simply replacing the elements, then this test will
  -- not provide even coverage

prop_unsafeMapMonotonic :: BagTest Int bag => bag Int -> bag Int -> Bool
prop_unsafeMapMonotonic bag xs =
    toOrdList (unsafeMapMonotonic (2*) xs) == Prelude.map (2*) (toOrdList xs)

prop_read_show :: BagTest Int bag 
               => bag Int -> bag Int -> Bool
prop_read_show bag xs = xs === read (show xs)

prop_strict :: BagTest a bag => bag a -> bag a -> Bool
prop_strict bag xs =
   strict xs === xs
   &&
   strictWith id xs === xs
