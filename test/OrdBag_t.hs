-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Coll.OrdBag_t where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude

import EdisonPrelude
import qualified Collection as C
import qualified List -- not ListSeq!
import qualified ListSeq as L
import Debug.QuickCheck

import LazyPairingHeap -- the bag module being tested
import qualified JoinList as S -- the sequence module being tested
  -- To different modules, simply replace the names above.
  -- To test a bag module that does not name its type constructor "Bag",
  -- you also need to define a type synonym
  --   type Bag a = ...
  -- You may also need to adjust the Seq type synonym.

type Bag a = Heap a
type Seq a = S.Seq a

tol :: Bag Int -> [Int]
tol = C.toOrdList

lmerge :: [Int] -> [Int] -> [Int]
lmerge xs [] = xs
lmerge [] ys = ys
lmerge xs@(x:xs') ys@(y:ys')
  | x <= y    = x : lmerge xs' ys
  | otherwise = y : lmerge xs ys'


-- CollX operations

prop_single :: Int -> Bool
prop_single x =
    tol (single x) == [x]

prop_fromSeq :: Seq Int -> Bool
prop_fromSeq xs =
    fromSeq xs == S.foldr insert empty xs

prop_insert :: Int -> Bag Int -> Bool
prop_insert x xs =
    tol (insert x xs) == List.insert x (tol xs)

prop_insertSeq :: Seq Int -> Bag Int -> Bool
prop_insertSeq xs ys =
    insertSeq xs ys == union (fromSeq xs) ys

prop_union :: Bag Int -> Bag Int -> Bool
prop_union xs ys =
    tol (union xs ys) == lmerge (tol xs) (tol ys)

prop_unionSeq :: Seq (Bag Int) -> Bool
prop_unionSeq xss =
    unionSeq xss == S.foldr union empty xss

prop_delete :: Int -> Bag Int -> Bool
prop_delete x xs =
    tol (delete x xs) == List.delete x (tol xs)

prop_deleteAll :: Int -> Bag Int -> Bool
prop_deleteAll x xs =
    tol (deleteAll x xs) == Prelude.filter (/= x) (tol xs)

prop_deleteSeq :: Seq Int -> Bag Int -> Bool
prop_deleteSeq xs ys =
    deleteSeq xs ys == S.foldr delete ys xs

prop_null_size :: Bag Int -> Bool
prop_null_size xs =
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (tol xs)

prop_member_count :: Bag Int -> Int -> Bool
prop_member_count xs x =
    member xs x == (c > 0)
    &&
    c == Prelude.length (Prelude.filter (== x) (tol xs))
  where c = count xs x


-- Coll operations

prop_toSeq :: Bag Int -> Bool
prop_toSeq xs =
    List.sort (S.toList (toSeq xs)) == tol xs

prop_lookup :: Bag Int -> Int -> Bool
prop_lookup xs x =
    if member xs x then
      lookup xs x == x
      &&
      lookupM xs x == Just x
      &&
      lookupWithDefault 999 xs x == x
      &&
      lookupAll xs x == Prelude.take (count xs x) (repeat x)
    else
      lookupM xs x == Nothing
      &&
      lookupWithDefault 999 xs x == 999
      &&
      lookupAll xs x == []

prop_fold :: Bag Int -> Bool
prop_fold xs =
    List.sort (fold (:) [] xs) == tol xs
    &&
    (null xs || fold1 (+) xs == sum (tol xs))

prop_filter_partition :: Bag Int -> Bool
prop_filter_partition xs =
    tol (filter p xs) == Prelude.filter p (tol xs)
    &&
    partition p xs == (filter p xs, filter (not . p) xs)
  where p x = x `mod` 3 == 2


-- OrdCollX operations

prop_deleteMin_Max :: Bag Int -> Bool
prop_deleteMin_Max xs =
    tol (deleteMin xs) == L.ltail (tol xs)
    &&
    tol (deleteMax xs) == L.rtail (tol xs)

prop_unsafeInsertMin_Max :: Int -> Bag Int -> Bool
prop_unsafeInsertMin_Max i xs =
    if null xs then
      unsafeInsertMin 0 xs == single 0
      &&
      unsafeInsertMax xs 0 == single 0
    else
      unsafeInsertMin lo xs == insert lo xs
      &&
      unsafeInsertMax xs hi == insert hi xs
  where lo = minElem xs - (if odd i then 1 else 0)
        hi = maxElem xs + (if odd i then 1 else 0)
    
prop_unsafeFromOrdSeq :: [Int] -> Bool
prop_unsafeFromOrdSeq xs =
    tol (unsafeFromOrdSeq xs') == xs'
  where xs' = List.sort xs

prop_unsafeAppend :: Int -> Bag Int -> Bag Int -> Bool
prop_unsafeAppend i xs ys =
    if null xs || null ys then
      unsafeAppend xs ys == union xs ys
    else
      unsafeAppend xs ys' == union xs ys'
  where delta = maxElem xs - minElem ys + (if odd i then 1 else 0)
        ys' = unsafeMapMonotonic (+delta) ys
  -- if unsafeMapMonotonic does any reorganizing in addition
  -- to simply replacing the elements, then this test will
  -- not provide even coverage

prop_filter :: Int -> Bag Int -> Bool
prop_filter x xs =
    tol (filterLT x xs) == Prelude.filter (< x) (tol xs)
    &&
    tol (filterLE x xs) == Prelude.filter (<= x) (tol xs)
    &&
    tol (filterGT x xs) == Prelude.filter (> x) (tol xs)
    &&
    tol (filterGE x xs) == Prelude.filter (>= x) (tol xs)

prop_partition :: Int -> Bag Int -> Bool
prop_partition x xs =
    partitionLT_GE x xs == (filterLT x xs, filterGE x xs)
    &&
    partitionLE_GT x xs == (filterLE x xs, filterGT x xs)
    &&
    partitionLT_GT x xs == (filterLT x xs, filterGT x xs)


-- OrdColl operations

prop_minView_maxView :: Bag Int -> Bool
prop_minView_maxView xs =
    minView xs == (if null xs then Nothing
                              else Just (minElem xs, deleteMin xs))
    &&
    maxView xs == (if null xs then Nothing
                              else Just (deleteMax xs, maxElem xs))

prop_minElem_maxElem :: Bag Int -> Property
prop_minElem_maxElem xs =
    not (null xs) ==>
      minElem xs == Prelude.head (tol xs)
      &&
      maxElem xs == Prelude.last (tol xs)

prop_foldr_foldl :: Bag Int -> Bool
prop_foldr_foldl xs =
    foldr (:) [] xs == tol xs
    &&
    foldl (flip (:)) [] xs == Prelude.reverse (tol xs)

prop_foldr1_foldl1 :: Bag Int -> Property
prop_foldr1_foldl1 xs =
    not (null xs) ==>
      foldr1 f xs == foldr f 1333 xs
      &&
      foldl1 (flip f) xs == foldl (flip f) 1333 xs
  where f x 1333 = x
        f x y = 3*x - 7*y

prop_toOrdSeq :: Bag Int -> Bool
prop_toOrdSeq xs =
    S.toList (toOrdSeq xs) == tol xs

-- bonus operation, not supported by all ordered collections

prop_unsafeMapMonotonic :: Bag Int -> Bool
prop_unsafeMapMonotonic xs =
    tol (unsafeMapMonotonic (2*) xs) == Prelude.map (2*) (tol xs)
