-- Copyright (c) 2002 Andrew Bromage.
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.FM where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude
import qualified List as L

import Test.QuickCheck hiding (elements)
import Test.HUnit (Test(..))

import Data.Edison.Test.Utils
import Data.Edison.Assoc
import qualified Data.Edison.Assoc.TernaryTrie as TT
import qualified Data.Edison.Assoc.StandardMap as SM
import qualified Data.Edison.Assoc.AssocList as AL
import qualified Data.Edison.Assoc.PatriciaLoMap as PLM

----------------------------------------------------------------
-- A utility class to propigate class contexts down
-- to the quick check properties

class (Ord k, Show k,Arbitrary k,
       Arbitrary (fm a), Show (fm a),FiniteMap fm k) 
        => FMTest k a fm | fm -> k 

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest [k] a (TT.FM k)

instance (Ord a, Show a, Arbitrary a) => FMTest Int a PLM.FM

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest k a (SM.FM k)

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest k a (AL.FM k)


---------------------------------------------------------------
-- List of all permutations of bag types to test

allFMTests :: Test
allFMTests = TestList
   [ fmTests (empty :: (Ord a) => SM.FM Int a)
   , fmTests (empty :: (Ord a) => AL.FM Int a)
   , fmTests (empty :: (Ord a) => PLM.FM a)
   , fmTests (empty :: (Ord a) => TT.FM [Int] a)
   ]

----------------------------------------------------------------
-- List all the tests to run for each type

fmTests fm = TestLabel ("FM test "++(instanceName fm)) . TestList $
   [ qcTest $ prop_Structure fm
   , qcTest $ prop_Single fm
   , qcTest $ prop_SortedStructure fm
   , qcTest $ prop_UnionStructure fm
   , qcTest $ prop_UnionSeqStructure fm
   , qcTest $ prop_DeleteStructure fm
   , qcTest $ prop_ToSeq fm
   , qcTest $ prop_ToFromSeq fm
   , qcTest $ prop_Member fm
   , qcTest $ prop_Elements fm          -- 10
   , qcTest $ prop_PartitionKey fm
   , qcTest $ prop_Partition fm
   , qcTest $ prop_Difference fm
   ]


-----------------------------------------------------------------
-- Utility Operations


removeDups :: (Eq k) => [(k, v)] -> [(k, v)]
removeDups [] = []
removeDups ((k,v):kvs) = (k,v) : removeDups (L.filter ((/= k) . fst) kvs)

si :: AssocX fm k => fm a -> Bool
si = structuralInvariant

-----------------------------------------------------------------
-- AssocX operations

prop_Structure :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_Structure fm xs
  = L.null xs `trivial`
        si (fromSeq xs `asTypeOf` fm)


prop_Single :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_Single fm xs
  = L.null xs `trivial`
        (si fm1 && (L.sort (keys fm1)) == (L.sort (keys fm2)))
    where
        fm1 = unionSeq [ singleton k v | (k,v) <- xs ] `asTypeOf` fm
        fm2 = fromSeq xs `asTypeOf` fm

prop_SortedStructure :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_SortedStructure fm xs
  = L.null xs `trivial`
        si (fromSeq (L.sort xs) `asTypeOf` fm)

prop_UnionStructure :: FMTest k Int fm => 
        fm Int -> [(k,Int)] -> [(k,Int)] -> Property
prop_UnionStructure fm xs ys
  = (L.null xs || L.null ys) `trivial`
        si (union (fromSeq xs) (fromSeq ys) `asTypeOf` fm)

prop_UnionSeqStructure  :: FMTest k Int fm => 
        fm Int -> [[(k,Int)]] -> Property
prop_UnionSeqStructure fm xs
  = (L.null xs) `trivial`
        si (unionSeq (L.map fromSeq xs) `asTypeOf` fm)

prop_DeleteStructure :: FMTest k Int fm =>
        fm Int -> [k] -> [(k,Int)] -> Property
prop_DeleteStructure fm xs ys
  = (L.null xs) `trivial`
        si (deleteSeq xs (fromSeq ys `asTypeOf` fm))

prop_ToSeq :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_ToSeq fm xs
  = (L.null cleaned) `trivial`
        (si fm1 
            && L.sort (toList fm1) == L.sort cleaned
            && L.sort (keys fm1)   == L.sort (L.map fst cleaned))
    where
        cleaned = removeDups xs
        fm1 = fromSeq cleaned `asTypeOf` fm

prop_ToFromSeq :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_ToFromSeq fm xs
  = (L.null xs) `trivial`
        (si fm1 && si fm2 && s1 == s2)
    where
        fm1 = fromSeq xs `asTypeOf` fm
        s1 = L.sort (toList fm1)
        fm2 = fromSeq s1 `asTypeOf` fm
        s2 = L.sort (toList fm2)

prop_Member :: FMTest k Int fm => fm Int -> [k] -> Property
prop_Member fm xs
  = (L.null xs) `trivial`
        (si fm1 
            && all (\x -> member x fm1) xs
            && all (\k -> lookupM k fm1 == Just 0) xs)
    where
	fm1 = fromSeq [ (x,0) | x <- xs ] `asTypeOf` fm


prop_Elements :: FMTest k Int fm => fm Int -> [(k,Int)] -> Property
prop_Elements fm xs
  = (L.null xs) `trivial`
  	(si fm1 &&
           L.sort (elements fm1) == L.sort (L.map snd cleaned))
    where
    	cleaned = removeDups xs
	fm1 = fromSeq cleaned `asTypeOf` fm


prop_PartitionKey :: FMTest k Int fm => fm Int -> k -> [(k,Int)] -> Property
prop_PartitionKey fm key xs
  = (L.null xs) `trivial`
  	(si fm1 && si fm2 &&
		L.sort (toSeq fm1) ==
			L.sort [ el | el@(k,v) <- cleaned, p k v ] &&
		L.sort (toSeq fm2) ==
			L.sort [ el | el@(k,v) <- cleaned, not (p k v) ])
    where
    	cleaned = removeDups xs
	fm0 = fromSeq cleaned `asTypeOf` fm
	(fm1, fm2) = partitionWithKey p fm0
	p k v = k < key


prop_Partition :: FMTest k Int fm => fm Int -> Int -> [(k,Int)] -> Property
prop_Partition fm val xs
  = (L.null xs) `trivial`
  	(si fm1 && si fm2 &&
		L.sort (toSeq fm1) ==
			L.sort [ el | el@(k,v) <- cleaned, p v ] &&
		L.sort (toSeq fm2) ==
			L.sort [ el | el@(k,v) <- cleaned, not (p v) ])
    where
    	cleaned = removeDups xs
	fm0 = fromSeq cleaned `asTypeOf` fm
	(fm1, fm2) = partition p fm0
	p v = v < val

prop_Difference :: FMTest k Int fm => 
	fm Int -> [(k,Int)] -> [(k,Int)] -> Property
prop_Difference fm xs ys
  = (L.null xs || L.null ys) `trivial`
  	(si fm1 && si fm2 && si fm3 &&
  	  check s1 s2 s3 && (not (L.null s3) || null fm3))
    where
	fm1 = fromSeq xs `asTypeOf` fm
	fm2 = fromSeq ys `asTypeOf` fm
	fm3 = difference fm1 fm2
	s1 = L.sort . L.map fst . toSeq $ fm1
	s2 = L.sort . L.map fst . toSeq $ fm2
	s3 = L.sort . L.map fst . toSeq $ fm3

	check []     _      zs
	  = L.null zs
	check xs     []     zs
	  = xs == zs
	check (x:xs) (y:ys) []
	  | x > y     = check (x:xs) ys []
	  | x == y    = check xs ys []
	  | otherwise = False
	check (x:xs) (y:ys) (z:zs)
	  | x > y     = check (x:xs) ys     (z:zs)
	  | x < y     = x == z &&
	                check xs     (y:ys) zs
	  | otherwise = check xs     ys     (z:zs)

{-
    -- Methods still to be tested:

    deleteAll
    size
    count
    lookupAll
    lookupWithDefault
    adjust
    adjustAll
    map
    fold
    fold1
    filter

    mapWithKey
    foldWithKey
    filterWithKey

    fromSeqWith
    fromSeqWithKey
    insertWith
    insertWithKey
    insertSeqWith
    
    insertSeqWithKey
    unionl
    unionr
    unionWith
    unionSeqWith
    intersectWith
    
    subset
    subsetEq

    unionWithKey
    unionSeqWithKey
    intersectWithKey

-}

