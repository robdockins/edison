-- Copyright (c) 2002 Andrew Bromage.
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.FM where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude
import qualified Data.List as L
import Data.Maybe

import Test.QuickCheck hiding (elements, (===))
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

class (FMTest k a fm,OrdFiniteMap fm k) => OrdFMTest k a fm | fm -> k

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest [k] a (TT.FM k)

instance (Ord a, Show a, Arbitrary a) => FMTest Int a PLM.FM

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest k a (SM.FM k)
instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => OrdFMTest k a (SM.FM k)

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => FMTest k a (AL.FM k)

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => OrdFMTest k a (AL.FM k)

instance (Ord a, Show a, Arbitrary a) => OrdFMTest Int a PLM.FM

instance (Ord a, Show a, Arbitrary a,
          Ord k, Show k, Arbitrary k) => OrdFMTest [k] a (TT.FM k)

---------------------------------------------------------------
-- List of all permutations of finite map types to test

allFMTests :: Test
allFMTests = TestList
   [ fmTests (empty :: (Ord a) => SM.FM Int a)
   , fmTests (empty :: (Ord a) => AL.FM Int a)
   , fmTests (empty :: (Ord a) => PLM.FM a)
   , fmTests (empty :: (Ord a) => TT.FM Int a)
   , ordFMTests (empty :: (Ord a) => SM.FM Int a)
   , ordFMTests (empty :: (Ord a) => AL.FM Int a)
   , ordFMTests (empty :: (Ord a) => PLM.FM a)
   , ordFMTests (empty :: (Ord a) => TT.FM Int a)
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
   , qcTest $ prop_fromSeqWith fm
   , qcTest $ prop_fromSeqWithKey fm
   , qcTest $ prop_insertWith fm
   , qcTest $ prop_insertWithKey fm
   , qcTest $ prop_insertSeqWith fm
   , qcTest $ prop_insertSeqWithKey fm
   , qcTest $ prop_unionWith fm        -- 40
   , qcTest $ prop_unionlr fm
   , qcTest $ prop_unionSeqWith fm
   , qcTest $ prop_intersectionWith fm
   , qcTest $ prop_difference fm
   , qcTest $ prop_properSubset fm
   , qcTest $ prop_subset fm
   , qcTest $ prop_submapBy fm
   , qcTest $ prop_properSubmapBy fm
   , qcTest $ prop_sameMapBy fm
   , qcTest $ prop_toSeq fm            -- 50
   , qcTest $ prop_keys fm
   , qcTest $ prop_mapWithKey fm
   , qcTest $ prop_foldWithKey fm
   , qcTest $ prop_filterWithKey fm
   , qcTest $ prop_partitionWithKey fm
   , qcTest $ prop_unionWithKey fm
   , qcTest $ prop_unionSeqWithKey fm
   , qcTest $ prop_intersectionWithKey fm
   , qcTest $ prop_strict fm
   ]

ordFMTests fm = TestLabel ("Ord FM test "++(instanceName fm)) . TestList $
   [ qcTest $ prop_min fm
   , qcTest $ prop_max fm
   , qcTest $ prop_foldr fm
   , qcTest $ prop_foldl fm
   , qcTest $ prop_filterLT fm
   , qcTest $ prop_filterLE fm
   , qcTest $ prop_filterGT fm
   , qcTest $ prop_filterGE fm
   , qcTest $ prop_ord_partition fm
   , qcTest $ prop_fromOrdSeq fm
   , qcTest $ prop_unsafeAppend fm     -- 10
   , qcTest $ prop_minViewWithKey fm
   , qcTest $ prop_maxViewWithKey fm
   , qcTest $ prop_foldrWithKey fm
   , qcTest $ prop_foldlWithKey fm
   , qcTest $ prop_toOrdSeq fm
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

trivial x y = not x ==> y

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

prop_min :: OrdFMTest k Int fm =>
         fm Int -> [(k,Int)] -> Bool
prop_min fm xs =
      case minView xs' of
         Nothing     -> null xs'
         Just (z,zs) ->
            snd min == z
            &&
            minElem xs' == z
	    &&
            delete (fst min) xs' === zs
            &&
            deleteMin xs' === zs
            &&
            unsafeInsertMin (fst min) (snd min) (deleteMin xs') === xs'

  where xs' = (fromSeq (removeDups xs)) `asTypeOf` fm
        min = L.minimumBy (\x y -> compare (fst x) (fst y)) (removeDups xs)

prop_max :: OrdFMTest k Int fm =>
         fm Int -> [(k,Int)] -> Bool
prop_max fm xs =
      case maxView xs' of
         Nothing     -> null xs'
         Just (z,zs) ->
            snd max == z
            &&
            maxElem xs' == z
	    &&
            delete (fst max) xs' === zs
            &&
            deleteMax xs' === zs
            &&
            unsafeInsertMax (fst max) (snd max) (deleteMax xs') === xs'

  where xs' = (fromSeq (removeDups xs)) `asTypeOf` fm
        max = L.maximumBy (\x y -> compare (fst x) (fst y)) (removeDups xs)


prop_foldr :: OrdFMTest k Int fm =>
          fm Int -> [(k,Int)] -> Bool
prop_foldr fm xs =
	foldr f 17 map == L.foldr f 17 (L.map snd xs')
        &&
        foldr f 17 map == foldr' f 17 map
	&&
	(null map || foldr1 f map == L.foldr1 f (L.map snd xs'))
        &&
        (null map || foldr1 f map == foldr1' f map)

  where f x z = (z^(abs x) + 11) `mod` ((abs x)+23)
        map = fromSeq (removeDups xs) `asTypeOf` fm
        xs' = L.sortBy (\x y -> compare (fst x) (fst y)) (removeDups xs)

prop_foldl :: OrdFMTest k Int fm =>
          fm Int -> [(k,Int)] -> Bool
prop_foldl fm xs =
	foldl f 17 map == L.foldl f 17 (L.map snd xs')
        &&
        foldl f 17 map == foldl' f 17 map
	&&
	(null map || foldl1 f map == L.foldl1 f (L.map snd xs'))
        &&
        (null map || foldl1 f map == foldl1' f map)

  where f z x = (z^(abs x) + 11) `mod` ((abs x)+23)
        map = fromSeq (removeDups xs) `asTypeOf` fm
        xs' = L.sortBy (\x y -> compare (fst x) (fst y)) (removeDups xs)

prop_filterLT :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_filterLT fm k xs =
        filterLT k xs === filterWithKey (\k' _ -> k' < k) xs

prop_filterLE :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_filterLE fm k xs =
        filterLE k xs === filterWithKey (\k' _ -> k' <= k) xs

prop_filterGT :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_filterGT fm k xs =
        filterGT k xs === filterWithKey (\k' _ -> k' > k) xs

prop_filterGE :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_filterGE fm k xs =
        filterGE k xs === filterWithKey (\k' _ -> k' >= k) xs

prop_ord_partition :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_ord_partition fm k xs =
        partitionLT_GE k xs == (filterLT k xs, filterGE k xs)
        &&
        partitionLE_GT k xs == (filterLE k xs, filterGT k xs)
        &&
        partitionLT_GT k xs == (filterLT k xs, filterGT k xs)

prop_fromOrdSeq :: OrdFMTest k Int fm =>
          fm Int -> [(k,Int)] -> Bool
prop_fromOrdSeq fm xs = map1 === map2    
   where map1 = fromSeq (removeDups xs) `asTypeOf` fm
         map2 = unsafeFromOrdSeq (L.sortBy (\x y -> compare (fst x) (fst y)) (removeDups xs))

prop_unsafeAppend :: OrdFMTest k Int fm =>
          fm Int -> k -> fm Int -> Bool
prop_unsafeAppend fm k xs = unsafeAppend a b === xs
   where (a,b) = partitionLT_GE k xs


prop_fromSeqWith :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> Bool
prop_fromSeqWith fm xs = map1 === map2
   where map1 = fromSeqWith (+) xs `asTypeOf` fm
	 map2 = fromSeq .
                L.map (\l -> L.foldr (\ (_,x) (i,y) -> (i,x+y))
                                     (fst $ head l,snd $ head l)
                                     (tail l)) .
                L.groupBy (\x y -> (fst x) == (fst y)) .
                L.sortBy  (\x y -> compare (fst x) (fst y)) $ xs

prop_fromSeqWithKey :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> Bool
prop_fromSeqWithKey fm xs =
	fromSeqWithKey (const (+)) xs === (fromSeqWith (+) xs `asTypeOf` fm)

prop_insertWith :: FMTest k Int fm =>
          fm Int -> k -> Int -> [(k,Int)] -> Bool
prop_insertWith fm k x xs = map1 === map2
   where map1 = insertWith (+) k x (fromSeq (removeDups xs) `asTypeOf` fm)
         map2 = fromSeq . g . removeDups $ xs
         g [] = [(k,x)]
         g ((k',x'):rest)
             | k == k'   = (k,x+x') : rest
             | otherwise = (k',x') : g rest

prop_insertWithKey :: FMTest k Int fm =>
          fm Int -> k -> Int -> [(k,Int)] -> Bool
prop_insertWithKey fm k x xs =
        insertWithKey (const (+)) k x z === insertWith (+) k x z
   where z = (fromSeq . removeDups $ xs) `asTypeOf` fm

prop_insertSeqWith :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> fm Int -> Bool
prop_insertSeqWith fm xs map =
	insertSeqWith (+) xs map === L.foldr (uncurry (insertWith (+))) map xs

prop_insertSeqWithKey :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> fm Int -> Bool
prop_insertSeqWithKey fm xs map =
        insertSeqWithKey (const (+)) xs map === insertSeqWith (+) xs map

prop_unionWith :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> [(k,Int)] -> Bool
prop_unionWith fm as bs = map1 === map2
  where f = (+)
	map1 = unionWith f (fromSeq (removeDups as)) (fromSeq (removeDups bs))
	map2 = fromSeq zs `asTypeOf` fm
        zs = merge (sortFst (removeDups as)) (sortFst (removeDups bs))
        sortFst = L.sortBy (\x y -> compare (fst x) (fst y))
        merge [] ys = ys
        merge xs [] = xs
        merge ((k1,x):xs) ((k2,y):ys) =
	   case compare k1 k2 of
                EQ -> (k1,f x y) : merge xs ys
                LT -> (k1,x) : merge xs ((k2,y):ys)
                GT -> (k2,y) : merge ((k1,x):xs) ys

prop_unionlr :: FMTest k Int fm =>
          fm Int -> fm Int -> fm Int -> Bool
prop_unionlr fm xs ys =
     unionl xs ys === unionWith (\x y -> x) xs ys
     &&
     unionr xs ys === unionWith (\x y -> y) xs ys

prop_unionSeqWith :: FMTest k Int fm =>
          fm Int -> [fm Int] -> Bool
prop_unionSeqWith fm xss =
     unionSeqWith (+) xss === L.foldr (unionWith (+)) empty xss

prop_intersectionWith :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> [(k,Int)] -> Bool
prop_intersectionWith fm xs ys =
     intersectionWith (-) as bs === unionWith (-) (fromSeq xs') (fromSeq ys')

  where as = fromSeq (removeDups xs) `asTypeOf` fm
        bs = fromSeq (removeDups ys) `asTypeOf` fm
        xs' = L.filter (\ (k,_) -> k `L.elem` L.map fst ys) (removeDups xs)
        ys' = L.filter (\ (k,_) -> k `L.elem` L.map fst xs) (removeDups ys)

prop_difference :: FMTest k Int fm =>
          fm Int -> [(k,Int)] -> [(k,Int)] -> Bool
prop_difference fm xs ys =
	difference as bs === fromSeq xs'

  where as = fromSeq (removeDups xs) `asTypeOf` fm
        bs = fromSeq (removeDups ys) `asTypeOf` fm
        xs' = L.filter (\ (k,_) -> not (k `L.elem` L.map fst ys)) (removeDups xs)

prop_properSubset :: FMTest k Int fm =>
           fm Int -> [(k,Int)] -> [(k,Int)] -> Bool
prop_properSubset fm xs ys =
        properSubset as bs == (isSub && notEq)

  where as = fromSeq xs' `asTypeOf` fm
        bs = fromSeq ys' `asTypeOf` fm
        xs' = removeDups xs
        ys' = removeDups ys
        isSub = L.foldr (&&) True  . L.map (\ (k,_) -> k `L.elem` L.map fst ys') $ xs'
        notEq = length xs' < length ys'

prop_subset :: FMTest k Int fm =>
           fm Int -> [(k,Int)] -> [(k,Int)] -> Bool
prop_subset fm xs ys =
	subset as bs == isSub

  where as = fromSeq xs' `asTypeOf` fm
        bs = fromSeq ys' `asTypeOf` fm
        xs' = removeDups xs
        ys' = removeDups ys
        isSub = L.foldr (&&) True . L.map (\ (k,_) -> k `L.elem` L.map fst ys') $ xs'


prop_submapBy :: FMTest k Int fm =>
           fm Int -> fm Int -> fm Int -> Bool
prop_submapBy fm xs ys =
         submapBy f xs ys ==
         (subset xs ys && fold (&&) True (intersectionWith f xs ys))
  where f x y = x + y `mod` 3 == 0

prop_properSubmapBy :: FMTest k Int fm =>
           fm Int -> fm Int -> fm Int -> Bool
prop_properSubmapBy fm xs ys =
         properSubmapBy f xs ys ==
         (properSubset xs ys && fold (&&) True (intersectionWith f xs ys))
  where f x y = x + y `mod` 3 == 0

prop_sameMapBy :: FMTest k Int fm =>
           fm Int -> fm Int -> fm Int -> Bool
prop_sameMapBy fm xs ys =
	 sameMapBy f xs ys ==
         (subset xs ys && subset ys xs && fold (&&) True (intersectionWith f xs ys))
  where f x y = x + y `mod` 3 == 0


prop_toSeq :: FMTest k Int fm =>
           fm Int -> [(k,Int)] -> Bool
prop_toSeq fm xs =
     sortFst (toSeq map1) == sortFst (removeDups xs)

  where sortFst = L.sortBy (\x y -> compare (fst x) (fst y))
        map1 = fromSeq (removeDups xs) `asTypeOf` fm

prop_keys :: FMTest k Int fm =>
           fm Int -> [(k,Int)] -> Bool
prop_keys fm xs =
      L.sort ks == L.sort (keys map1)

  where ks = L.map fst . removeDups $ xs
        map1 = fromSeq (removeDups xs) `asTypeOf` fm

prop_mapWithKey :: FMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_mapWithKey fm xs =
      (mapWithKey (const (+19)) xs == map (+19) xs)
      &&
      (fold (&&) True . mapWithKey (\k x -> lookupM k xs == Just x) $ xs)

prop_foldWithKey :: FMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_foldWithKey fm xs =
      (foldWithKey (const (+)) 11 xs == fold (+) 11 xs)
      &&
      (foldWithKey (const (+)) 11 xs == foldWithKey' (const (+)) 11 xs)
      &&
      (foldWithKey (\k x z -> (lookupM k xs == Just x) && z) True xs)

prop_filterWithKey :: FMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_filterWithKey fm xs =
    filterWithKey (const f) xs === filter f xs
    &&
    filterWithKey (\k x -> lookupM k xs == Just x) xs === xs

  where f x = x `mod` 2 == 0

prop_partitionWithKey :: FMTest k Int fm =>
           fm Int -> k -> fm Int -> Bool
prop_partitionWithKey fm k xs =
    partitionWithKey f xs == (filterWithKey f xs, filterWithKey (\k x -> not (f k x)) xs)
  where f k' x = k' < k || (x `mod` 3 == 0)

prop_minViewWithKey :: OrdFMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_minViewWithKey fm xs =
    case minViewWithKey xs of
      Nothing -> minView xs == Nothing
      Just ((k,x), xs') ->
	(minView xs == Just (x,xs')) && 
        (minElemWithKey xs == (k,x)) &&
        (lookupM k xs == Just x)

prop_maxViewWithKey :: OrdFMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_maxViewWithKey fm xs =
    case maxViewWithKey xs of
      Nothing -> maxView xs == Nothing
      Just ((k,x), xs') ->
	(maxView xs == Just (x,xs')) && 
        (maxElemWithKey xs == (k,x)) &&
        (lookupM k xs == Just x)

prop_foldrWithKey :: OrdFMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_foldrWithKey fm xs =
    (foldrWithKey (const f) 19 xs == foldr f 19 xs) &&
    (foldrWithKey' (const f) 19 xs == foldr f 19 xs) &&
    (L.sort (foldrWithKey (\k x z -> k:z) [] xs) == L.sort (keys xs))
 where f x z = (z^(abs x) + 11) `mod` ((abs x)+23)

prop_foldlWithKey :: OrdFMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_foldlWithKey fm xs =
    (foldlWithKey g 19 xs == foldl f 19 xs) &&
    (foldlWithKey' g 19 xs == foldl f 19 xs) &&
    (L.sort (foldlWithKey (\z k x -> k:z) [] xs) == L.sort (keys xs))
 where f z x = (z^(abs x) + 11) `mod` ((abs x)+23)
       g z _ x = f z x

prop_toOrdSeq :: OrdFMTest k Int fm =>
           fm Int -> [(k,Int)] -> Bool
prop_toOrdSeq fm xs = xs' == toOrdSeq as
   where as  = fromSeq (removeDups xs) `asTypeOf` fm
         xs' = L.sortBy (\x y -> compare (fst x) (fst y)) (removeDups xs)

prop_unionWithKey :: FMTest k Int fm =>
           fm Int -> fm Int -> fm Int -> Bool
prop_unionWithKey fm xs ys =
   unionWithKey (const (+)) xs ys === unionWith (+) xs ys

prop_unionSeqWithKey :: FMTest k Int fm =>
           fm Int -> [fm Int] -> Bool
prop_unionSeqWithKey fm xss =
   unionSeqWithKey (const (+)) xss === unionSeqWith (+) xss

prop_intersectionWithKey :: FMTest k Int fm =>
           fm Int -> fm Int -> fm Int -> Bool
prop_intersectionWithKey fm xs ys =
     intersectionWithKey (const (-)) xs ys === intersectionWith (-) xs ys

prop_strict :: FMTest k Int fm =>
           fm Int -> fm Int -> Bool
prop_strict fm xs = 
     strict xs === xs
     &&
     strictWith id xs === xs

prop_show_read :: (FMTest k Int fm, Read (fm Int)) =>
	fm Int -> fm Int -> Bool
prop_show_read fm xs = xs === read (show xs)
