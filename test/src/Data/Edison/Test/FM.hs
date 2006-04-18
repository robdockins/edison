-- Copyright (c) 2002 Andrew Bromage.
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.FM where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude
import qualified Data.List as L
import Data.Maybe

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

class (Ord k, Show k, Arbitrary k,
       Arbitrary (fm a), Show (fm a),
       Eq (fm a), FiniteMap fm k)
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
   , fmTests (empty :: (Ord a) => TT.FM Int a)
   , qcTest $ prop_show_read (empty :: (Ord a) => AL.FM Int a)
   , qcTest $ prop_show_read (empty :: (Ord a) => PLM.FM a)
   , qcTest $ prop_show_read (empty :: (Ord a) => TT.FM Int a)
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
   , qcTest $ prop_Elements fm
   , qcTest $ prop_PartitionKey fm      -- 10
   , qcTest $ prop_Partition fm
   , qcTest $ prop_Difference fm
   , qcTest $ prop_insert fm
   , qcTest $ prop_insertSeq fm
   , qcTest $ prop_union fm
   , qcTest $ prop_unionSeq fm
   , qcTest $ prop_delete fm
   , qcTest $ prop_deleteSeq fm
   , qcTest $ prop_size fm
   , qcTest $ prop_member fm            -- 20
   , qcTest $ prop_lookup fm
   , qcTest $ prop_lookupAndDelete fm
   , qcTest $ prop_adjust fm
   , qcTest $ prop_adjustAll fm
   , qcTest $ prop_adjustOrInsert fm
   , qcTest $ prop_adjustAllOrInsert fm
   , qcTest $ prop_adjustOrDelete fm
   , qcTest $ prop_adjustOrDeleteAll fm
   , qcTest $ prop_fold fm
   , qcTest $ prop_fold1 fm             -- 30
   , qcTest $ prop_elements fm
   , qcTest $ prop_filter fm
   , qcTest $ prop_partition fm
   ]

-----------------------------------------------------------------
-- Utility Operations


removeDups :: (Eq k) => [(k, v)] -> [(k, v)]
removeDups [] = []
removeDups ((k,v):kvs) = (k,v) : removeDups (L.filter ((/= k) . fst) kvs)

si :: AssocX fm k => fm a -> Bool
si = structuralInvariant

(===) :: (Eq (fm a), AssocX fm k) => fm a -> fm a -> Bool
(===) m1 m2 = 
    structuralInvariant m1
    &&
    structuralInvariant m2
    &&
    m1 == m2


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


prop_insert :: FMTest k Int fm =>
	fm Int -> k -> Int -> [(k,Int)] -> Bool
prop_insert fm k x xs =
   let xs' = (k,x) : (L.filter ( (/=k) .  fst ) xs)
   in insert k x (fromSeq xs) === (fromSeq xs' `asTypeOf` fm)

prop_insertSeq :: FMTest k Int fm =>
	fm Int -> [(k,Int)] -> fm Int -> Bool
prop_insertSeq fm ins xs =
   insertSeq (removeDups ins) xs === L.foldr (uncurry insert) xs (removeDups ins)


prop_union :: FMTest k Int fm =>
        fm Int -> fm Int -> fm Int -> Bool
prop_union fm xs ys =
   let a = L.sort (keysList (union xs ys))
       b = L.sort (L.nub (keysList xs ++ keysList ys))
   in a == b

prop_unionSeq :: FMTest k Int fm =>
        fm Int -> [fm Int] -> Bool
prop_unionSeq fm xss =
   keysList (unionSeq xss) == keysList (L.foldr union empty xss)

prop_delete :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_delete fm k xs =
     L.sort (keysList (delete k xs)) == 
     L.sort (L.filter (/=k) (keysList xs)) 
  &&
     delete k xs === deleteAll k xs

prop_deleteSeq :: FMTest k Int fm =>
        fm Int -> [k] -> fm Int -> Bool
prop_deleteSeq fm ks xs =
   deleteSeq ks xs === L.foldr delete xs ks

prop_size :: FMTest k Int fm =>
        fm Int -> fm Int -> Bool
prop_size fm xs =
      L.length (keysList xs) == size xs
  &&
      (null xs == (size xs == 0))

prop_member :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_member fm k xs =
       member k xs == (not . L.null) (L.filter (==k) (keysList xs))
    &&
       if member k xs then count k xs == 1 else count k xs == 0


prop_lookup :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_lookup fm k xs =
   case lookupM k xs of
      Nothing ->
          not (member k xs)
       && lookupWithDefault 9999 k xs == 9999

      Just x  -> 
          lookup k xs == x 
       && lookupWithDefault 9999 k xs == x
       && ((snd . head) (L.filter ((==k) . fst) (toList xs))) == x
      

prop_lookupAndDelete :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_lookupAndDelete fm k xs =
   case lookupAndDeleteM k xs of
      Nothing ->
          not (member k xs)

      Just (z,zs)  ->
          (lookup k xs == z)
       && (lookupAndDelete k xs == (z,zs))
       && (lookupAndDeleteAll k xs == ([z],zs))

prop_adjust :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_adjust fm k xs =
   if member k xs
      then L.sort (toList (adjust (+ 1234) k xs)) ==
           L.sort (L.map (\(k',x) -> if k == k' then (k',x+1234) else (k',x)) (toList xs))
      else adjust (\x -> undefined) k xs === xs

prop_adjustAll :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_adjustAll fm k xs =
   adjust (+1234) k xs === adjustAll (+1234) k xs


prop_adjustOrInsert :: FMTest k Int fm =>
        fm Int -> k -> Int -> fm Int -> Bool
prop_adjustOrInsert fm k x xs =
   if member k xs
      then adjustOrInsert (+1234) x k xs === adjust (+1234) k xs
      else adjustOrInsert (+1234) x k xs === insert k x xs

prop_adjustAllOrInsert :: FMTest k Int fm =>
        fm Int -> k -> Int -> fm Int -> Bool
prop_adjustAllOrInsert fm k x xs =
   adjustAllOrInsert (+1234) x k xs === adjustOrInsert (+1234) x k xs

prop_adjustOrDelete :: FMTest k Int fm =>
        fm Int -> k -> fm Int -> Bool
prop_adjustOrDelete fm k xs =
     L.sort (toList (adjustOrDelete f k xs)) ==
     L.sort (catMaybes (L.map g (toList xs)))

   where f x = if x `mod` 7 == 0
                  then Nothing
                  else Just (2*x)
         g (k',x) = if k == k' then f x >>= return . (,) k' else return (k',x)


prop_adjustOrDeleteAll :: FMTest k Int fm =>
         fm Int -> k -> fm Int -> Bool
prop_adjustOrDeleteAll fm k xs =
      adjustOrDelete f k xs === adjustOrDeleteAll f k xs

   where f x = if x `mod` 7 == 0
                  then Nothing
                  else Just (2*x)


prop_fold :: FMTest k Int fm =>
        fm Int -> fm Int -> Bool
prop_fold fm xs =
   L.sort (fold (:) [] xs) == L.sort (elementsList xs)
   &&
   (null xs || fold (+) 0 xs == sum (elementsList xs))
   &&
   (null xs || fold' (+) 0 xs == sum (elementsList xs))

prop_fold1 :: FMTest k Int fm =>
        fm Int -> fm Int -> Bool
prop_fold1 fm xs =
   (null xs || fold (+) 0 xs == fold1 (+) xs)
   &&
   (null xs || fold (+) 0 xs == fold1' (+) xs)

prop_elements :: FMTest k Int fm =>
         fm Int -> [(k,Int)] -> Bool
prop_elements fm xs =
    L.sort (elementsList (fromList (removeDups xs) `asTypeOf` fm)) == 
    (L.sort . L.map snd . removeDups) xs

prop_filter :: FMTest k Int fm =>
         fm Int -> [(k,Int)] -> Bool
prop_filter fm xs = 
     (L.sort . L.filter (f . snd)) xs' ==
     L.sort (toList (filter f (fromList xs' `asTypeOf` fm)))     

  where xs' = removeDups xs
        f x = x `mod` 2 == 0

prop_partition :: FMTest k Int fm =>
         fm Int -> fm Int -> Bool
prop_partition fm xs =
     let (xs1,xs2) = partition f xs
     in xs1 === filter f xs
        &&
        xs2 === filter (not . f) xs
 
  where f x = x `mod` 2 == 0

prop_show_read :: (FMTest k Int fm, Read (fm Int)) => 
	fm Int -> fm Int -> Bool
prop_show_read fm xs = xs === read (show xs)


{-
    -- Methods still to be tested:


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

