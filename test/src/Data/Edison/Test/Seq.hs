-- Copyright (c) 1999 Chris Okasaki.
-- See COPYRIGHT file for terms and conditions.

module Data.Edison.Test.Seq where


import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude

import Data.Edison.Prelude
import Data.Edison.Seq
import Data.Edison.Test.Utils

import Test.QuickCheck
import Test.HUnit (Test(..))

------------------------------------------------------
-- The sequnce implementations to check

import qualified Data.Edison.Seq.BankersQueue as BQ
import qualified Data.Edison.Seq.BinaryRandList as BRL
import qualified Data.Edison.Seq.BraunSeq as BS
import qualified Data.Edison.Seq.JoinList as JL
import qualified Data.Edison.Seq.ListSeq as LS
import qualified Data.Edison.Seq.MyersStack as MS
import qualified Data.Edison.Seq.RandList as RL
import qualified Data.Edison.Seq.SimpleQueue as SQ
import qualified Data.Edison.Seq.SizedSeq as Sized
import qualified Data.Edison.Seq.RevSeq as Rev


--------------------------------------------------------
-- Just a utility class to help propigate the class contexts
-- we need down into the quick check properties

class ( Eq (seq a), Arbitrary (seq a)
      , Show (seq a), Read (seq a)
      , Sequence seq ) => SeqTest a seq

instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a []
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a BQ.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a BS.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a JL.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a MS.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a RL.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a SQ.Seq
instance (Eq a,Arbitrary a,Show a,Read a) => SeqTest a BRL.Seq
instance (Eq a,Arbitrary a,Show a,Read a,SeqTest a seq) => SeqTest a (Sized.Sized seq)
instance (Eq a,Arbitrary a,Show a,Read a,SeqTest a seq) => SeqTest a (Rev.Rev seq)


---------------------------------------------------------
-- List all permutations of sequence types to test

allSequenceTests :: Test
allSequenceTests = TestList
   [ seqTests (empty :: [a])
   , seqTests (empty :: Sized.Sized [] a)
   , seqTests (empty :: Rev.Rev [] a)
   , seqTests (empty :: BQ.Seq a)
   , seqTests (empty :: Sized.Sized BQ.Seq a)
   , seqTests (empty :: Rev.Rev BQ.Seq a)
   , seqTests (empty :: BS.Seq a)
   , seqTests (empty :: Sized.Sized BS.Seq a)
   , seqTests (empty :: Rev.Rev BS.Seq a)
   , seqTests (empty :: JL.Seq a)
   , seqTests (empty :: Sized.Sized JL.Seq a)
   , seqTests (empty :: Rev.Rev JL.Seq a)
   , seqTests (empty :: MS.Seq a)
   , seqTests (empty :: Sized.Sized MS.Seq a)
   , seqTests (empty :: Rev.Rev MS.Seq a)
   , seqTests (empty :: RL.Seq a)
   , seqTests (empty :: Sized.Sized RL.Seq a)
   , seqTests (empty :: Rev.Rev RL.Seq a)
   , seqTests (empty :: SQ.Seq a)
   , seqTests (empty :: Sized.Sized SQ.Seq a)
   , seqTests (empty :: Rev.Rev SQ.Seq a)
   , seqTests (empty :: BRL.Seq a)
   , seqTests (empty :: Sized.Sized BRL.Seq a)
   , seqTests (empty :: Rev.Rev BRL.Seq a)
   , seqTests (empty :: Sized.Sized (Sized.Sized SQ.Seq) a)
   , seqTests (empty :: Rev.Rev (Rev.Rev SQ.Seq) a)
   , seqTests (empty :: Sized.Sized (Rev.Rev SQ.Seq) a)
   , seqTests (empty :: Rev.Rev (Sized.Sized SQ.Seq) a)
   ]


---------------------------------------------------------------
-- List all the tests to run for each type

-- infer the type so I don't have to write down the big nasty
-- class context...
seqTests seq = TestLabel ("Sequence test "++(instanceName seq)) . TestList $
  [ qcTest $ prop_equals seq
  , qcTest $ prop_fromList seq
  , qcTest $ prop_toList seq
  , qcTest $ prop_single seq
  , qcTest $ prop_lcons_rcons seq
  , qcTest $ prop_lview_rview seq
  , qcTest $ prop_lhead_rhead seq
  , qcTest $ prop_ltail_rtail seq
  , qcTest $ prop_append seq
  , qcTest $ prop_null_size seq
  , qcTest $ prop_reverse seq                      -- 10
  , qcTest $ prop_reverseOnto seq
  , qcTest $ prop_map seq
  , qcTest $ prop_fold seq
  , qcTest $ prop_strict_fold seq
  , qcTest $ prop_fold1 seq
  , qcTest $ prop_strict_fold1 seq
  , qcTest $ prop_reduce seq
  , qcTest $ prop_strict_reduce seq
  , qcTest $ prop_reduce1 seq
  , qcTest $ prop_strict_reduce1 seq               -- 20
  , qcTest $ prop_inBounds_lookup seq
  , qcTest $ prop_update_adjust seq
  , qcTest $ prop_withIndex seq
  , qcTest $ prop_take_drop_splitAt seq
  , qcTest $ prop_subseq seq
  , qcTest $ prop_filter_takeWhile_dropWhile seq
  , qcTest $ prop_partition_splitWhile seq
  , qcTest $ prop_zip_zipWith seq
  , qcTest $ prop_zip3_zipWith3 seq
  , qcTest $ prop_unzip_unzipWith seq              -- 30
  , qcTest $ prop_unzip3_unzipWith3 seq
  , qcTest $ prop_concat seq
  , qcTest $ prop_concatMap seq
  , qcTest $ prop_strict seq
  , qcTest $ prop_show_read seq
  ]

(===) :: (Eq (seq a),Sequence seq) => seq a -> seq a -> Bool
(===) s1 s2 = 
    structuralInvariant s1 
    &&
    structuralInvariant s2
    &&
    s1 == s2

si :: Sequence seq => seq a -> Bool
si = structuralInvariant


---------------------------------------------------
-- Properties to check

prop_equals :: SeqTest Int seq => seq Int -> seq Int -> seq Int -> Bool
prop_equals seq xs ys =
    si xs && si ys && (xs == ys) == (toList xs == toList ys)

prop_fromList :: SeqTest Int seq => seq Int -> [Int] -> Bool
prop_fromList seq xs =
    fromList xs === (Prelude.foldr lcons empty xs `asTypeOf` seq)
    &&
    toList (fromList xs `asTypeOf` seq) == xs

prop_toList :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_toList seq xs =
    toList xs == foldr (:) [] xs
    &&
    fromList (toList xs) === xs


prop_single :: SeqTest Int seq => seq Int -> Int -> Bool
prop_single seq x =
    let xs = singleton x `asTypeOf` seq
     in si xs && toList xs == [x]

prop_lcons_rcons :: SeqTest Int seq => seq Int -> Int -> seq Int -> Bool
prop_lcons_rcons seq x xs =
    lcons x xs === append (singleton x) xs
    &&
    rcons x xs === append xs (singleton x)


prop_lview_rview :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_lview_rview seq xs =
    lview xs == (if null xs then Nothing else Just (lhead xs, ltail xs))
    &&
    rview xs == (if null xs then Nothing else Just (rhead xs, rtail xs))

prop_lhead_rhead :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_lhead_rhead seq xs =
    not (null xs) ==>
      lhead xs == Prelude.head (toList xs)
      &&
      rhead xs == Prelude.last (toList xs)

prop_ltail_rtail :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_ltail_rtail seq xs =
    not (null xs) ==>
      let xs_ltail = ltail xs
          xs_rtail = rtail xs
       in si xs_ltail 
          &&
          si xs_rtail
          &&
          toList xs_ltail == Prelude.tail (toList xs)
          &&
          toList xs_rtail == Prelude.init (toList xs)

prop_append :: SeqTest Int seq => seq Int -> seq Int -> seq Int -> Bool
prop_append seq xs ys =
    let xys = append xs ys
     in si xys
        &&
        toList (append xs ys) == toList xs ++ toList ys

prop_null_size :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_null_size seq xs =
    si xs
    &&
    null xs == (size xs == 0)
    &&
    size xs == Prelude.length (toList xs)

prop_reverse :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_reverse seq xs =
    let rev_xs = reverse xs
     in si rev_xs
        &&
        toList (rev_xs) == Prelude.reverse (toList xs)

prop_reverseOnto :: SeqTest Int seq => seq Int -> seq Int -> seq Int -> Bool
prop_reverseOnto seq xs ys =
    reverseOnto xs ys === append (reverse xs) ys


prop_map :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_map seq xs =
    let succ_xs = map (+1) xs
     in si succ_xs
        &&
       toList succ_xs == Prelude.map (+1) (toList xs)

prop_fold :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_fold seq xs =
    foldr (:) [99] xs == toList xs ++ [99]
    &&
    foldl (flip (:)) [99] xs == Prelude.reverse (toList xs) ++ [99]
    &&
    fold (+) 0 xs == foldr (+) 0 xs
    &&
    fold' (+) 0 xs == foldr' (+) 0 xs
    &&
    if (not . null) xs then fold1 (+) xs == foldr1 (+) xs else True
    &&
    if (not . null) xs then fold1' (+) xs == foldr1 (+) xs else True

prop_strict_fold :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_strict_fold seq xs =
    foldr (+) 0 xs == foldr' (+) 0 xs
    &&
    foldl (+) 0 xs == foldl' (+) 0 xs

prop_fold1 :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_fold1 seq xs =
    not (null xs) ==>
       foldr1 f xs == Prelude.foldr1 f (toList xs)
       &&
       foldl1 f xs == Prelude.foldl1 f (toList xs)
  where f x y = 3*x - 2*y

prop_strict_fold1 :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_strict_fold1 seq xs =
    not (null xs) ==>
       foldr1' f xs == foldr1 f xs
       &&
       foldl1' f xs == foldl1 f xs
  where f x y = 3*x - 2*y

prop_reduce :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_reduce seq xs =
    reducel append (singleton 93) (map singleton xs) === append (singleton 93) xs
    &&
    reducer append (singleton 93) (map singleton xs) === append xs (singleton 93)

prop_strict_reduce  :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_strict_reduce seq xs =
    reducel' (+) 0 xs == reducel (+) 0 xs
    &&
    reducer' (+) 0 xs == reducer (+) 0 xs

prop_reduce1 :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_reduce1 seq xs =
    not (null xs) ==>
      reduce1 append (map singleton xs) === xs

prop_strict_reduce1 :: SeqTest Int seq => seq Int -> seq Int -> Property
prop_strict_reduce1 seq xs =
    not (null xs) ==>
      reduce1' (+) xs == reduce1 (+) xs


prop_inBounds_lookup :: SeqTest Int seq => seq Int -> Int -> seq Int -> Bool
prop_inBounds_lookup seq i xs =
    inBounds i xs == (0 <= i && i < size xs)
    &&
    (if inBounds i xs then
       lookup i xs == lhead (drop i xs)
       &&
       lookupM i xs == Just (lookup i xs)
       &&
       lookupWithDefault 99 i xs == lookup i xs
     else
       lookupM i xs == Nothing
       &&
       lookupWithDefault 99 i xs == 99)


prop_update_adjust :: SeqTest Int seq => seq Int -> Int -> seq Int -> Bool
prop_update_adjust seq i xs =
    if inBounds i xs then
      let ys = take i xs
          zs = drop (i+1) xs
          x = lookup i xs
      in
        si ys 
        &&
        si zs
        &&
        update i 99 xs == append ys (lcons 99 zs)
        &&
        adjust (+1) i xs == append ys (lcons (x+1) zs)
    else
      update i 99 xs === xs
      &&
      adjust (+1) i xs === xs

prop_withIndex :: SeqTest Int seq => seq Int -> seq Int -> Bool
prop_withIndex seq xs =
    toList (mapWithIndex (+) xs) == Prelude.map (uncurry (+)) ixs
    &&
    foldrWithIndex f [] xs == ixs
    &&
    foldlWithIndex g [] xs == Prelude.reverse ixs
  where ixs = Prelude.zip [0..] (toList xs)
        f i x xs = (i,x):xs
        g xs i x = (i,x):xs

prop_take_drop_splitAt :: SeqTest Int seq => seq Int -> Int -> seq Int -> Bool
prop_take_drop_splitAt seq n xs =
    size (take n xs) == max 0 (min n (size xs))
    &&
    append (take n xs) (drop n xs) === xs
    &&
    splitAt n xs == (take n xs, drop n xs)

prop_subseq :: SeqTest Int seq => seq Int -> Int -> Int -> seq Int -> Bool
prop_subseq seq i len xs =
    subseq i len xs === take len (drop i xs)

prop_filter_takeWhile_dropWhile :: SeqTest Int seq =>
	seq Int -> Int -> seq Int -> Bool

prop_filter_takeWhile_dropWhile seq x xs =
    toList (filter p xs) == Prelude.filter p (toList xs)
    &&
    toList (takeWhile p xs) == Prelude.takeWhile p (toList xs)
    &&
    toList (dropWhile p xs) == Prelude.dropWhile p (toList xs)
  where p = (< x)

prop_partition_splitWhile :: SeqTest Int seq =>
	seq Int -> Int -> seq Int -> Bool

prop_partition_splitWhile seq x xs =
    partition p xs == (filter p xs, filter (not . p) xs)
    &&
    splitWhile p xs == (takeWhile p xs, dropWhile p xs)
  where p = (< x)


prop_zip_zipWith :: SeqTest Int seq => seq Int -> seq Int -> seq Int -> Bool
prop_zip_zipWith seq xs ys =
    toList (zip xs ys) == xys
    &&
    toList (zipWith (,) xs ys) == xys
  where xys = Prelude.zip (toList xs) (toList ys)

prop_zip3_zipWith3 :: SeqTest Int seq =>
	seq Int -> seq Int -> seq Int -> seq Int -> Bool

prop_zip3_zipWith3 seq xs ys zs =
    toList (zip3 xs ys zs) == xyzs
    &&
    toList (zipWith3 (,,) xs ys zs) == xyzs
  where xyzs = Prelude.zip3 (toList xs) (toList ys) (toList zs)


prop_unzip_unzipWith :: (SeqTest Int seq,SeqTest (Int,Int) seq) =>
	seq Int -> seq (Int,Int) -> Bool

prop_unzip_unzipWith seq xys =
    si xs
    &&
    si ys
    &&
    unzip xys == (xs, ys)
    &&
    unzipWith fst snd xys == (xs, ys)
  where xs = map fst xys
        ys = map snd xys


prop_unzip3_unzipWith3 :: (SeqTest Int seq,SeqTest (Int,Int,Int) seq) =>
	seq Int -> seq (Int,Int,Int) -> Bool

prop_unzip3_unzipWith3 seq xyzs =
    si xs
    &&
    si ys
    &&
    si zs
    &&
    unzip3 xyzs == (xs, ys, zs)
    &&
    unzipWith3 fst3 snd3 thd3 xyzs == (xs, ys, zs)
  where xs = map fst3 xyzs
        ys = map snd3 xyzs
        zs = map thd3 xyzs

        fst3 (x,y,z) = x
        snd3 (x,y,z) = y
        thd3 (x,y,z) = z


prop_concat :: (SeqTest (seq Int) seq,SeqTest Int seq) => seq Int -> Property
prop_concat seq = forAll (genss seq) $
	\xss -> concat xss === foldr append empty xss


genss :: (SeqTest (seq Int) seq,SeqTest Int seq) =>
	seq Int -> Gen (seq (seq Int))

genss seq = sized (\n -> resize (min 20 n) arbitrary)



prop_concatMap :: (SeqTest (seq Int) seq, SeqTest Int seq) =>
	seq Int -> seq Int -> Property

prop_concatMap seq xs = forAll (genss seq) check
  where check xss = concatMap f xs === concat (map f xs)
            where f i = lookupWithDefault empty i xss

prop_strict :: SeqTest Int seq =>
        seq Int -> seq Int -> Bool
prop_strict seq xs = 
     strict xs === xs
     &&
     strictWith id xs === xs
     

prop_show_read :: (SeqTest Int seq) => seq Int -> seq Int -> Bool
prop_show_read seq xs = xs === read (show xs)