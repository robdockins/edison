-- Copyright (c) 1999 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Seq.Seq_t where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude

import EdisonPrelude
import Debug.QuickCheck

import SimpleQueue -- the module being tested
  -- To test a different module, simply replace the name above.
  -- To test a module that does not name its type constructor "Seq",
  -- you also need to define a type synonym
  --   type Seq a = ...

prop_equals :: Seq Int -> Seq Int -> Bool
prop_equals xs ys =
    (xs == ys) == (toList xs == toList ys)

prop_fromList :: [Int] -> Bool
prop_fromList xs = 
    fromList xs == (Prelude.foldr lcons empty xs :: Seq Int)
    &&
    toList (fromList xs :: Seq Int) == xs

prop_toList :: Seq Int -> Bool
prop_toList xs = 
    toList xs == foldr (:) [] xs
    &&
    fromList (toList xs) == xs

prop_single :: Int -> Bool
prop_single x =
    toList (single x :: Seq Int) == [x]

prop_lcons_rcons :: Int -> Seq Int -> Bool
prop_lcons_rcons x xs = 
    lcons x xs == append (single x) xs
    &&
    rcons xs x == append xs (single x)

prop_lview_rview :: Seq Int -> Bool
prop_lview_rview xs =
    lview xs == (if null xs then Nothing else Just (lhead xs, ltail xs))
    &&
    rview xs == (if null xs then Nothing else Just (rtail xs, rhead xs))

prop_lhead_rhead :: Seq Int -> Property
prop_lhead_rhead xs =
    not (null xs) ==>
      lhead xs == Prelude.head (toList xs)
      &&
      rhead xs == Prelude.last (toList xs)

prop_ltail_rtail :: Seq Int -> Bool
prop_ltail_rtail xs =
    toList (ltail xs) == (if null xs then [] else Prelude.tail (toList xs))
    &&
    toList (rtail xs) == (if null xs then [] else Prelude.init (toList xs))

prop_append :: Seq Int -> Seq Int -> Bool
prop_append xs ys = 
    toList (append xs ys) == toList xs ++ toList ys

prop_null_size :: Seq Int -> Bool
prop_null_size xs =
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (toList xs)

prop_reverse :: Seq Int -> Bool
prop_reverse xs = 
    toList (reverse xs) == Prelude.reverse (toList xs)

prop_reverseOnto :: Seq Int -> Seq Int -> Bool
prop_reverseOnto xs ys = 
    reverseOnto xs ys == append (reverse xs) ys

prop_map :: Seq Int -> Bool
prop_map xs = 
    toList (map (+1) xs) == Prelude.map (+1) (toList xs)

prop_fold :: Seq Int -> Bool
prop_fold xs =
    foldr (:) [99] xs == toList xs ++ [99]
    &&
    foldl (flip (:)) [99] xs == Prelude.reverse (toList xs) ++ [99]

prop_fold1 :: Seq Int -> Property
prop_fold1 xs =
    not (null xs) ==> 
       foldr1 f xs == Prelude.foldr1 f (toList xs)
       &&
       foldl1 f xs == Prelude.foldl1 f (toList xs)
  where f x y = 3*x - 2*y

prop_reduce :: Seq Int -> Bool
prop_reduce xs =
    reducel append (single 93) (map single xs) == append (single 93) xs
    &&
    reducer append (single 93) (map single xs) == append xs (single 93)

prop_reduce1 :: Seq Int -> Property
prop_reduce1 xs =
    not (null xs) ==>
      reduce1 append (map single xs) == xs

prop_inBounds_lookup :: Int -> Seq Int -> Bool
prop_inBounds_lookup i xs =
    inBounds xs i == (0 <= i && i < size xs)
    &&
    (if inBounds xs i then
       lookup xs i == lhead (drop i xs)
       &&
       lookupM xs i == Just (lookup xs i)
       &&
       lookupWithDefault 99 xs i == lookup xs i
     else
       lookupM xs i == Nothing
       &&
       lookupWithDefault 99 xs i == 99)


prop_update_adjust :: Int -> Seq Int -> Bool
prop_update_adjust i xs =
    if inBounds xs i then
      let ys = take i xs
          zs = drop (i+1) xs
          x = lookup xs i 
      in
        update i 99 xs == append ys (lcons 99 zs)
        &&
        adjust (+1) i xs == append ys (lcons (x+1) zs)
    else
      update i 99 xs == xs
      &&
      adjust (+1) i xs == xs

prop_withIndex :: Seq Int -> Bool
prop_withIndex xs =
    toList (mapWithIndex (+) xs) == Prelude.map (uncurry (+)) ixs
    &&
    foldrWithIndex f [] xs == ixs
    &&
    foldlWithIndex g [] xs == Prelude.reverse ixs
  where ixs = Prelude.zip [0..] (toList xs)
        f i x xs = (i,x):xs
        g xs i x = (i,x):xs

prop_take_drop_splitAt :: Int -> Seq Int -> Bool
prop_take_drop_splitAt n xs =
    size (take n xs) == max 0 (min n (size xs))
    &&
    append (take n xs) (drop n xs) == xs
    &&
    splitAt n xs == (take n xs, drop n xs)

prop_subseq :: Int -> Int -> Seq Int -> Bool
prop_subseq i len xs =
    subseq i len xs == take len (drop i xs)

prop_filter_takeWhile_dropWhile :: Int -> Seq Int -> Bool
prop_filter_takeWhile_dropWhile x xs =
    toList (filter p xs) == Prelude.filter p (toList xs)
    &&
    toList (takeWhile p xs) == Prelude.takeWhile p (toList xs)
    &&
    toList (dropWhile p xs) == Prelude.dropWhile p (toList xs)
  where p = (< x)

prop_partition_splitWhile :: Int -> Seq Int -> Bool
prop_partition_splitWhile x xs =
    partition p xs == (filter p xs, filter (not . p) xs)
    &&
    splitWhile p xs == (takeWhile p xs, dropWhile p xs)
  where p = (< x)

prop_zip_zipWith :: Seq Int -> Seq Int -> Bool
prop_zip_zipWith xs ys =
    toList (zip xs ys) == xys
    &&
    toList (zipWith (,) xs ys) == xys
  where xys = Prelude.zip (toList xs) (toList ys)

prop_zip3_zipWith3 :: Seq Int -> Seq Int -> Seq Int -> Bool
prop_zip3_zipWith3 xs ys zs =
    toList (zip3 xs ys zs) == xyzs
    &&
    toList (zipWith3 (,,) xs ys zs) == xyzs
  where xyzs = Prelude.zip3 (toList xs) (toList ys) (toList zs)

prop_unzip_unzipWith :: Seq (Int,Int) -> Bool
prop_unzip_unzipWith xys =
    unzip xys == (xs, ys)
    &&
    unzipWith fst snd xys == (xs, ys)
  where xs = map fst xys
        ys = map snd xys

prop_unzip3_unzipWith3 :: Seq (Int,Int,Int) -> Bool
prop_unzip3_unzipWith3 xyzs =
    unzip3 xyzs == (xs, ys, zs)
    &&
    unzipWith3 fst3 snd3 thd3 xyzs == (xs, ys, zs)
  where xs = map fst3 xyzs
        ys = map snd3 xyzs
        zs = map thd3 xyzs

        fst3 (x,y,z) = x        
        snd3 (x,y,z) = y
        thd3 (x,y,z) = z        

prop_concat :: Property
prop_concat = forAll genss $ \xss -> concat xss == foldr append empty xss

genss :: Gen (Seq (Seq Int))
genss = sized (\n -> resize (min 20 n) arbitrary)

prop_concatMap :: Seq Int -> Property
prop_concatMap xs = forAll genss check
  where check xss = concatMap f xs == concat (map f xs)
            where f = lookupWithDefault empty xss
