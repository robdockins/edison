-- Copyright (c) 2002 Andrew Bromage.  
-- See COPYRIGHT file for terms and conditions.

module TernaryTrie_t (
    testall,
    prop_Structure,
    prop_SortedStructure,
    removeDups,
    module Debug.QuickCheck
) where

import TernaryTrie
import qualified List as L
import IO

import Debug.QuickCheck ( quickCheck, verboseCheck, test, trivial )
import Debug.Trace

testall :: IO ()
testall
  = do	putStrLn "prop_Structure"
  	quickCheck prop_Structure

	putStrLn "prop_Single"
  	quickCheck prop_Single

	putStrLn "prop_SortedStructure"
  	quickCheck prop_SortedStructure

	putStrLn "prop_UnionStructure"
  	quickCheck prop_UnionStructure

	putStrLn "prop_UnionSeqStructure"
  	quickCheck prop_UnionSeqStructure

	putStrLn "prop_DeleteStructure"
  	quickCheck prop_DeleteStructure

	putStrLn "prop_Member"
  	quickCheck prop_Member

	putStrLn "prop_ToSeq"
  	quickCheck prop_ToSeq

	putStrLn "prop_ToFromSeq"
  	quickCheck prop_ToFromSeq

	putStrLn "prop_Elements"
  	quickCheck prop_Elements

	putStrLn "prop_Partition"
  	quickCheck prop_Partition

	putStrLn "prop_PartitionKey"
  	quickCheck prop_PartitionKey

	putStrLn "prop_Difference"
  	quickCheck prop_Difference

removeDups :: (Eq k) => [(k, v)] -> [(k, v)]
removeDups [] = []
removeDups ((k,v):kvs) = (k,v) : removeDups (L.filter ((/= k) . fst) kvs)

prop_Structure xs
  = L.null xs `trivial`
  	structuralInvariantFM (TernaryTrie.fromSeq xs)
    where types = xs :: [([Int], Int)]

prop_Single xs
  = L.null xs `trivial`
  	(structuralInvariantFM fm1 &&
		toSeq fm1 == (toSeq fm2 :: [([Int],Int)]))
    where
    	types = xs :: [([Int], Int)]
    	fm1 = TernaryTrie.unionSeq [ single k v | (k,v) <- xs ]
    	fm2 = TernaryTrie.fromSeq xs

prop_SortedStructure xs
  = L.null xs `trivial`
  	structuralInvariantFM (TernaryTrie.fromSeq (L.sort xs))
    where types = xs :: [([Int], Int)]

prop_UnionStructure xs ys
  = (L.null xs || L.null ys) `trivial`
  	structuralInvariantFM (union (fromSeq xs) (fromSeq ys))
    where
    	type1 = xs :: [([Int], Int)]
    	type2 = ys :: [([Int], Int)]

prop_UnionSeqStructure xs
  = (L.null xs) `trivial`
  	structuralInvariantFM (unionSeq (L.map fromSeq xs))
    where
    	type1 = xs :: [[([Int], Int)]]

prop_DeleteStructure xs ys
  = (L.null xs) `trivial`
  	structuralInvariantFM (deleteSeq xs (fromSeq ys))
    where
    	type1 = xs :: [[Int]]
	type2 = ys :: [([Int], Int)]

prop_ToSeq xs
  = (L.null cleaned) `trivial`
  	(structuralInvariantFM fm && toSeq fm == L.sort cleaned
		&& keys fm == L.sort (L.map fst cleaned))
    where
    	types = xs :: [([Int], Int)]
    	cleaned = removeDups xs
	fm = TernaryTrie.fromSeq cleaned

prop_ToFromSeq xs
  = (L.null xs) `trivial`
  	(structuralInvariantFM fm1 && structuralInvariantFM fm2
		&& s1 == s2) 
    where
    	types = xs :: [([Int], Int)]
	fm1 = fromSeq xs
	s1 = toSeq fm1 :: [([Int], Int)]
	fm2 = fromSeq s1
	s2 = toSeq fm2 :: [([Int], Int)]

prop_Member xs
  = (L.null xs) `trivial`
  	(structuralInvariantFM fm && all (TernaryTrie.member fm) xs
		&& all (\k -> TernaryTrie.lookupM fm k == Just k) xs)
    where
    	types = xs :: [[Int]]
	fm = TernaryTrie.fromSeq [ (x,x) | x <- xs ]

prop_Elements xs
  = (L.null xs) `trivial`
  	(structuralInvariantFM fm &&
		L.sort (TernaryTrie.elements fm) == L.sort (L.map snd cleaned))
    where
    	types = xs :: [([Int], Int)]
    	cleaned = removeDups xs
	fm = fromSeq cleaned

prop_PartitionKey key xs
  = (L.null xs) `trivial`
  	(structuralInvariantFM fm1 && structuralInvariantFM fm2 &&
		L.sort (TernaryTrie.toSeq fm1) ==
			L.sort [ el | el@(k,v) <- cleaned, p k v ] &&
		L.sort (TernaryTrie.toSeq fm2) ==
			L.sort [ el | el@(k,v) <- cleaned, not (p k v) ])
    where
    	type1 = key :: [Int]
    	type2 = xs :: [([Int], Int)]
    	cleaned = removeDups xs
	fm = fromSeq cleaned
	(fm1, fm2) = TernaryTrie.partitionWithKey p fm
	p k v = k < key

prop_Partition val xs
  = (L.null xs) `trivial`
  	(structuralInvariantFM fm1 && structuralInvariantFM fm2 &&
		L.sort (TernaryTrie.toSeq fm1) ==
			L.sort [ el | el@(k,v) <- cleaned, p v ] &&
		L.sort (TernaryTrie.toSeq fm2) ==
			L.sort [ el | el@(k,v) <- cleaned, not (p v) ])
    where
    	type1 = val :: Int
    	type2 = xs :: [([Int], Int)]
    	cleaned = removeDups xs
	fm = fromSeq cleaned
	(fm1, fm2) = TernaryTrie.partition p fm
	p v = v < val

prop_Difference xs ys
  = (L.null xs || L.null ys) `trivial`
  	(structuralInvariantFM fm1 && structuralInvariantFM fm2 &&
			structuralInvariantFM fm3 &&
			check s1 s2 s3 && (not (L.null s3)
				|| TernaryTrie.null fm3))
    where
    	type1 = xs :: [([Int], Int)]
    	type2 = ys :: [([Int], Int)]
	fm1 = fromSeq xs
	fm2 = fromSeq ys
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

