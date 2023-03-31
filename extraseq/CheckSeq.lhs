%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (C) 2003  Martin Holters <martin.holters@gmx.de>                %%
%%                                                                           %%
%% This library is free software; you can redistribute it and/or modify it   %%
%% under the terms of the GNU General Public License as published by the     %%
%% Free Software Foundation; either version 2 of the License, or (at your    %%
%% option) any later version.                                                %%
%%                                                                           %%
%% This library is distributed in the hope that it will be useful, but       %%
%% WITHOUT ANY WARRANTY; without even the implied warranty of                %%
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General %%
%% Public License for more details.                                          %%
%%                                                                           %%
%% You should have received a copy of the GNU General Public License along   %%
%% with this library; if not, write to the Free Software Foundation, Inc.,   %%
%% 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA                   %%
%%                                                                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\hide{
\begin{code}
module CheckSeq where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)
import qualified Prelude

import EdisonPrelude(Maybe2(Just2,Nothing2))
import QuickCheck

import Sequence

instance Show (a -> b) where
  show f = "<function>"
\end{code}
}
\label{sec:checkseq}
Edison already comes with a file \lstinline!TestSeq.hs! that defines QuickCheck
properties for all the sequence operations. Unfortunately, it has two flaws: It
is monomorphic and it cannot check preservation of the structural invariant.
That it is monomorphic means that all properties are defined on a type
\lstinline!Seq!, and what data structure this actually should be is defined by
importing the respective module into \lstinline!TestSeq.hs!, i.e. this file has
to be changed whenever another implementation is to be tested. While checking a
structural invariant could have been added to this by assuming the respective
module also exports a property like \lstinline!prop_inv :: Seq a -> Bool! and
then checking this property still holds after each modification, it seemed
beneficial to redesign the whole thing to end up with a polymorphic function
\lstinline!checkseq! that performs all necessary tests for a data structure
that can be defined via an argument.

To allow the invariant to be tested, we introduce a new type class that all
sequences that are to be checked have to be instances of. Furthermore, we
introduce a generator function that delivers an arbitrary sequence with given
contents which will be used when checking the equality operator, see below.
\begin{code}
class Sequence s => CheckableSequence s where
  invariant :: s a -> Bool
  arbitraryWith :: [a] -> Gen (s a)
\end{code}
It is very tempting now to write the ordinary \lstinline!arbitrary! function in
terms of \lstinline!arbitraryWith!, i.e. as
\begin{lstlisting}
arbitrary = arbitrary >>= arbitraryWith
\end{lstlisting}
first generating an arbitrary list and then constructing a sequence containing
its elements. There are two possible problems with this: First, when writing the
\lstinline!arbitrary! generators, care must be taken that they are complete,
i.e.  can construct every allowed instance of the data structure. Often, this is
easier verified for an explicit \lstinline!arbitrary! than for
\lstinline!arbitraryWith!, as the latter will typically be more complicated.
Second, it may sometimes be beneficial to generate more elements than the size
parameter suggests to avoid mostly trivial cases, especially in tree-like
structures after \lstinline!arbitrary! has already taken some recursive steps.
If this should be done with the help of \lstinline!arbitraryWith!, a lot of
ahead-planning before the recursion would be needed, resulting in even more
complication.

The properties themselves follow an approach that is somewhat different from
what they looked like in \lstinline!TestSeq.hs!. We first define a 
reference function in terms of  \lstinline!lview!, \lstinline!empty! and
\lstinline!cons!, and possibly other functions that have been checked before,
and then compare the results of the reference definition with that of the
implementation to be tested and verify the invariant, in case a new sequence
is among the results. It should be noted that \lstinline!empty! and
\lstinline!cons! correspond to the two list constructors \lstinline![]! and
\lstinline!:! and furthermore, \lstinline!lview! corresponds to the pattern
matching possible on lists. This makes it fairly easy to adapt list
representations of the functions as reference definitions, alas looking a 
little clumsier as the pattern matching has to be replaced with 
\lstinline!case! statements. 

Testing \lstinline!lview!, \lstinline!empty! and \lstinline!cons! themselves 
looks a little different. For \lstinline!lview!, we will assume it to be 
correct by definition, that is it gives the sequence interpretation to the 
underlying data structure by defining a head and a tail for
it. \lstinline!empty! and \lstinline!cons! then have to be compatible with
this interpretation which we will check without giving reference definitions
for them. 

However, we can check one thing about \lstinline!lview!, namely that
the tail it returns (if it does) fulfills the invariant:
\begin{code}
prop_lview :: (CheckableSequence s, Eq a) => s a -> Bool
prop_lview xs = case lview xs of 
                    Nothing2 -> True
                    Just2 _ xs' -> invariant xs'
\end{code}

We can now verify that \lstinline!empty! fulfills the invariant and that 
\lstinline!lview empty! yields \lstinline!Nothing2!. Unfortunately, we have to
construct some way of telling the property the data type it should operate on,
as we do not quantify over all sequences. We do this by explicitly passing in
the invariant as an argument and use it to resolve the overloading. We do the
same for the other constructor functions \lstinline!single!, 
\lstinline!formList!, \lstinline!copy! and \lstinline!tabulate!.
\begin{code}
prop_empty :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> Bool
prop_empty inv = inv xs && case lview xs of
                             Nothing2 -> True
                             Just2 _ _ -> False
  where xs = empty
\end{code}
There are two more things worthwhile noticing here. First, there is no variable
at all we quantify over, which is perfectly ok with QuickCheck; it will just
check the same thing 100 times. Second, the \lstinline!where! clause is very
important and will occur similarly throughout these properties. Replacing
\lstinline!xs!  with \lstinline!empty! in the \lstinline!case! clause of the
code fragment above would give a type error as the polymorphism could not be
resolved to a specific type. By using the \lstinline!where! clause we take
advantage of the monomorphism restriction that forces \lstinline!xs! to have
the same type at every occurrence and let the type of the given invariant fix
the type of the \lstinline!xs!. 

In order to check \lstinline!cons!, equality testing is useful, and we can
define it only in terms of \lstinline!lview!, so we test it first.  The
reference definition performs element-wise equality testing by simultaneous
recursion over both sequences. 
\begin{code}
equalsDef :: (Sequence s, Eq a) => s a -> s a -> Bool
equalsDef xs ys = case lview xs of
                    Nothing2 -> case lview ys of
                                  Nothing2 -> True
		                  Just2 _ _ -> False
                    Just2 x xs' -> case lview ys of
                                     Nothing2 -> False
		                     Just2 y ys' -> x==y && equalsDef xs' ys'
\end{code}
As no new sequence is generated, the resulting property is simply a
comparison of both return values of reference definition and given
implementation.  
\begin{code}
prop_equals :: (Sequence s, Eq a, Eq (s a)) => s a -> s a -> Bool
prop_equals xs ys = (xs==ys) == equalsDef xs ys
\end{code}
In this way testing the equality operator poses yet another problem: While it
would be sufficient to prove that 
\begin{displaymath}
\forall \lstinline!xs! . \forall \lstinline!ys! . 
(\lstinline!xs!==_{Impl}\lstinline!ys!) = 
(\lstinline!xs!==_{Def}\lstinline!ys!),
\end{displaymath}
we only perform automated testing, generating a limited number of instances for
\lstinline!xs! and \lstinline!ys!. This makes it highly unlikely that two
sequences are generated that are actually equal, except for very short cases.
To compensate this, we will check \lstinline!prop_equals! a second time with
sequences generated such that they are equal. In order to be able to do so, we
need a special generator that takes a list of elements and generates a sequence
containing the respective elements, but with an arbitrary internal structure.
This is why the \lstinline!CheckableSequence! class contains the second
function \lstinline!arbitraryWith!. 

Using equality, it is now easy to test that \lstinline!cons! is correct by
verifying that the resulting sequence satisfies the invariant and an 
\lstinline!lview! undoes its effect.
\begin{code}
prop_cons :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> a -> Bool
prop_cons xs x = lview xxs == Just2 x xs && invariant xxs
  where xxs = cons x xs
\end{code}

Most of the remaining reference definitions and properties are
straight-forward and do not require explanation. See appendix
\ref{chapter:checkseqsource} for the complete source code.
\hide{
\begin{code}
nullDef :: Sequence s => s a -> Bool
nullDef xs = case lview xs of
               Nothing2 -> True
	       Just2 _ _ -> False

prop_null :: (Sequence s) => s a -> Bool
prop_null xs = null xs == nullDef xs

singleDef :: Sequence s => a -> s a
singleDef x = cons x empty
  
prop_single :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> a -> Bool  
prop_single inv x = inv xs && xs == singleDef x
  where xs = single x

snocDef :: Sequence s => s a -> a -> s a
snocDef xs y = case lview xs of
                 Nothing2 -> single y
                 Just2 x xs' -> cons x (snocDef xs' y)

prop_snoc :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> a -> Bool
prop_snoc xs x = invariant xs' && xs' == snocDef xs x
  where xs' = snoc xs x

appendDef :: Sequence s => s a -> s a -> s a
appendDef xs ys = case lview xs of
                    Nothing2 -> ys
                    Just2 x xs' -> cons x (append xs' ys)
    
prop_append :: (CheckableSequence s, Eq a, Eq (s a)) 
               => s a -> s a -> Bool
prop_append xs ys = invariant xsys && xsys == appendDef xs ys
  where xsys = append xs ys

fromListDef :: Sequence s => [a] -> s a
fromListDef [] = empty
fromListDef (x:xs) = cons x (fromListDef xs)
    
prop_fromList :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> [a] -> Bool
prop_fromList inv l = inv s && s == fromListDef l
  where s = fromList l

copyDef :: Sequence s => Int -> a -> s a
copyDef n x | n<=0 = empty
            | otherwise = cons x (copyDef (n-1) x)
  
prop_copy :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> Int -> a -> Bool
prop_copy inv n x = inv xs && xs == copyDef n x
  where xs = copy n x

tabulateDef :: Sequence s => Int -> (Int -> a) -> s a
tabulateDef n f | n<=0 = empty
                | otherwise = snoc (tabulateDef (n-1) f) (f (n-1))
  
prop_tabulate :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> Int 
                                                 -> (Int -> a) -> Bool
prop_tabulate inv n f = inv s && s == tabulateDef n f
    where s = tabulate n f

lheadDef :: Sequence s => s a -> a
lheadDef xs = let Just2 x _ = lview xs in x
\end{code}
}

As \lstinline!lhead! may only be invoked for non-empty sequences, we have to
ensure this by using QuickChecks conditional property feature. It would be nice
if it was possible to check that \lstinline!lhead! actually produces an error
when invoked on an empty list, but this would unfortunately terminate the whole
test-bench. (The same holds of course for the other functions requiring a
non-empty sequence.)
\begin{code}
prop_lhead :: (Sequence s, Eq a, Eq (s a)) => s a -> Property
prop_lhead xs = not (null xs) ==> lhead xs == lheadDef xs
\end{code}
\hide{
\begin{code}  
ltailDef :: Sequence s => s a -> s a
ltailDef xs = case lview xs of
                Nothing2 -> empty
		Just2 _ xs' -> xs'

prop_ltail :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Bool
prop_ltail xs = invariant s && s == ltailDef xs
  where s = ltail xs

rviewDef :: Sequence s => s a -> Maybe2 (s a) a
rviewDef xs = case lview xs of
                Nothing2 -> Nothing2
                Just2 x' xs' -> case rviewDef xs' of
                                 Nothing2 -> Just2 empty x' 
                                 Just2 xs'' x'' -> Just2 (cons x' xs'') x''

prop_rview :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Bool
prop_rview xs =
    case xsx of
        Nothing2    -> rviewDef xs == xsx
	Just2 xs' x -> invariant xs' && rviewDef xs == xsx
    where xsx = rview xs

rheadDef :: Sequence s => s a -> a
rheadDef xs = let Just2 _ x = rview xs in x

prop_rhead :: (Sequence s, Eq a, Eq (s a)) => s a -> Property
prop_rhead xs = not (null xs) ==> rhead xs == rheadDef xs
  
rtailDef :: Sequence s => s a -> s a
rtailDef xs = case rview xs of
                Nothing2 -> empty
		Just2 xs' _ -> xs'

prop_rtail :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Bool
prop_rtail xs = invariant s && s == rtailDef xs
  where s = rtail xs

sizeDef :: Sequence s => s a -> Int
sizeDef xs = case lview xs of
               Nothing2 -> 0
	       Just2 _ xs' -> 1 + sizeDef xs'
\end{code}
}
To provide the user with some feedback whether his \lstinline!arbitrary!
function produces sequences of sufficient complexity, we \lstinline!collect!
the sizes of the test data when checking \lstinline!size!.
\begin{code}
prop_size  :: (Sequence s, Eq a, Eq (s a)) => s a -> Property
prop_size xs = collect (size xs) $ size xs == sizeDef xs
\end{code}
\hide{
\begin{code}
toListDef :: Sequence s => s a -> [a]
toListDef xs = case lview xs of
                 Nothing2 -> []
	         Just2 x xs' -> x : toListDef xs'
 
prop_toList :: (Sequence s, Eq a, Eq (s a)) => s a -> Bool
prop_toList xs = toList xs == toListDef xs

concatDef :: Sequence s => s (s a) -> s a
concatDef xss = case lview xss of
                  Nothing2 -> empty
		  Just2 xs xss' -> append xs (concatDef xss')

prop_concat :: (Sequence s, Eq a, Eq (s a)) => (s a -> Bool) -> s (s a) 
                                               -> Bool
prop_concat inv xss = inv res && res == concatDef xss
  where res = concat xss

reverseDef :: Sequence s => s a -> s a
reverseDef xs = case lview xs of
                  Nothing2 -> empty
		  Just2 x xs' -> snoc (reverseDef xs') x

prop_reverse :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Bool
prop_reverse xs = invariant rev && rev == reverseDef xs
  where rev = reverse xs

reverseOntoDef :: Sequence s => s a -> s a -> s a
reverseOntoDef xs ys = append (reverse xs) ys
  
prop_reverseOnto  :: (CheckableSequence s, Eq a, Eq (s a)) 
                     => s a -> s a -> Bool
prop_reverseOnto xs ys = invariant rev && rev == reverseOntoDef xs ys
  where rev = reverseOnto xs ys

mapDef :: Sequence s => (a -> b) -> s a -> s b
mapDef f xs = case lview xs of
                Nothing2 -> empty
		Just2 x xs' -> cons (f x) (mapDef f xs')

prop_map :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> (a -> a) -> Bool
prop_map xs f = invariant xs' && xs' == mapDef f xs
  where xs' = map f xs

concatMapDef :: Sequence s => (a -> s b) -> s a -> s b
concatMapDef f xs = concat (map f xs)

prop_concatMap :: (CheckableSequence s, Eq a, Eq (s a)) 
                  => s a -> (a -> s a) -> Bool
prop_concatMap xs f = invariant xs' && xs' == concatMapDef f xs
  where xs' = concatMap f xs

foldrDef :: Sequence s => (a -> b -> b) -> b -> s a -> b
foldrDef f e xs = case lview xs of
                    Nothing2 -> e
		    Just2 x xs' -> f x (foldrDef f e xs')

prop_foldr :: (Sequence s, Eq a, Eq (s a)) => s a -> (a -> a -> a) -> a
                                              -> Bool
prop_foldr xs f e = foldr f e xs == foldrDef f e xs

foldlDef :: Sequence s => (b -> a -> b) -> b -> s a -> b
foldlDef f e xs = case lview xs of
                    Nothing2 -> e
		    Just2 x xs' -> foldlDef f (f e x) xs'

prop_foldl :: (Sequence s, Eq a, Eq (s a)) => s a -> (a -> a -> a) -> a
                                              -> Bool
prop_foldl xs f e = foldl f e xs == foldlDef f e xs

foldr1Def :: Sequence s => (a -> a -> a) -> s a -> a
foldr1Def f xs = let Just2 xs' x = rview xs in foldr f x xs'

prop_foldr1 :: (Sequence s, Eq a, Eq (s a)) => s a -> (a -> a -> a) 
                                               -> Property
prop_foldr1 xs f = not (null xs) ==> foldr1 f xs == foldr1Def f xs

foldl1Def :: Sequence s => (a -> a -> a) -> s a -> a
foldl1Def f xs = let Just2 x xs' = lview xs in foldl f x xs'

prop_foldl1 :: (Sequence s, Eq a, Eq (s a)) => s a -> (a -> a -> a) 
                                               -> Property
prop_foldl1 xs f = not (null xs) ==> foldl1 f xs == foldl1Def f xs
\end{code}
}

The \lstinline!reduce! family of operations only return unambiguously defined
results if the function argument represents an associative function, i.e.
$x\oplus(y\oplus z) = (x\oplus y)\oplus z$. If this is the case, the result
has to be equal to what a \lstinline!fold! would yield, while the order in
which the function is applied will differ. We can therefore simply use an
appropriate \lstinline!fold! as reference definition.
\begin{code}
reducerDef :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducerDef = foldr
\end{code}
While QuickCheck provides a way to generate arbitrary functions which makes it 
possible to quantify over all functions for the \lstinline!fold! operations,
there is no (easy) way to quantify over all associative functions. To
circumvent this, we will use only one such function, namely \lstinline!(++)!
and show that this is sufficient in the sense that if instead of testing the
property, we would prove, we had then proven it for any associative function.

From what Wadler \cite{wadler_fpca89} calls parametricity we can derive that
for any two sets $A, A'$ representing types, any function 
$a : A \rightarrow A'$ and any operator $\oplus'$ such that 
$a(x\oplus y) = (a x) \oplus' (a y)$, it holds
that
\begin{displaymath}
a (\lstinline!reducer!\; (\oplus)\; u\; xs) 
= \lstinline!reducer!\; (\oplus')\; (a u)\; (a^* xs)
\end{displaymath}
where $a^*$ is element-wise application of $a$ (i.e. \lstinline!map a!) (see 
appendix \ref{sect:reducetff}).
We now choose $A$ to be $(A')^*$, i.e. lists of elements of $A'$ and 
$(\oplus)=(++)$, the append operation. Furthermore, we define $a$ as
\begin{displaymath}
a\; x=\left\{ 
\begin{array}{ll}
(a\;x_1)\oplus'(a\;x_2) & \textrm{for } x = x_1 +\hspace{-.5ex}+\; x_2 \\
x' & \textrm{for } x = [x']
\end{array}
\right.
\end{displaymath}
With this definition, obviously $a(x\oplus y) = (a x) \oplus' (a y)$ if 
$\oplus'$ is associative so that we can leave the decomposition of multiple
appends ambiguous without effecting the result. So we can use the above
equivalence to derive results for arbitrary types and associative functions by
just concentrating on appending and lists, starting with singletons, as 
obviously $a^{-1}\;x'=[x']$. 
\begin{code}
prop_reducer :: (Sequence s, Eq a, Eq (s a), Arbitrary a) 
                => s a -> a -> Bool
prop_reducer xs' e' = let xs = map (\x' -> [x']) xs'
                          e = [e']
                    in reducer (++) e xs == reducerDef (++) e xs
\end{code}

The same argument also holds for \lstinline!reducel! and can easily be adapted
for \lstinline!reduce1!.
\hide{
\begin{code}
reducelDef :: Sequence s => (a -> a -> a) -> a -> s a -> a
reducelDef = foldl

prop_reducel :: (Sequence s, Eq a, Eq (s a), Arbitrary a) 
                => s a -> a -> Bool
prop_reducel xs' e' = let xs = map (\x' -> [x']) xs'
                          e = [e']
                    in reducel (++) e xs == reducelDef (++) e xs

reduce1Def :: Sequence s => (a -> a -> a) -> s a -> a
reduce1Def = foldr1

prop_reduce1 :: (Sequence s, Eq a, Eq (s a), Arbitrary a) => s a -> Property
prop_reduce1 xs' = let xs = map (\x' -> [x']) xs'
                   in not (null xs') ==> reduce1 (++) xs == reduce1Def (++) xs

takeDef :: Sequence s => Int -> s a -> s a
takeDef i xs | i<=0 = empty
             | i>=size xs = xs
	     | otherwise = let Just2 x xs' = lview xs 
	                   in cons x (takeDef (i-1) xs')
			   
prop_take :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Int -> Bool
prop_take xs i = invariant xs' && xs' == takeDef i xs
  where xs' = take i xs

dropDef :: Sequence s => Int -> s a -> s a
dropDef i xs | i<=0 = xs
             | i>=size xs = empty
	     | otherwise = dropDef (i-1) (ltail xs)
			   
prop_drop :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Int -> Bool
prop_drop xs i = invariant xs' && xs' == dropDef i xs
  where xs' = drop i xs

splitAtDef :: Sequence s => Int -> s a -> (s a, s a)
splitAtDef i xs = (take i xs, drop i xs)

prop_splitAt :: (CheckableSequence s, Eq a, Eq (s a)) => s a -> Int -> Bool
prop_splitAt xs i = invariant xs' && invariant xs'' 
                    && (xs', xs'') == splitAtDef i xs
  where (xs', xs'') = splitAt i xs

subseqDef :: Sequence s => Int -> Int -> s a -> s a
subseqDef i j xs = take j (drop i xs)

prop_subseq :: (CheckableSequence s, Eq a, Eq (s a)) 
               => s a -> Int -> Int -> Bool
prop_subseq xs i j = invariant xs' && xs' == subseqDef i j xs
  where xs' = subseq i j xs

filterDef :: Sequence s => (a -> Bool) -> s a -> s a
filterDef p xs = case lview xs of
                   Nothing2 -> empty
                   Just2 x xs' -> if p x then cons x (filterDef p xs')
		                         else filterDef p xs'
		   
prop_filter :: (CheckableSequence s, Eq a, Eq (s a)) 
               => s a -> (a -> Bool) -> Bool
prop_filter xs p = invariant xs' && xs' == filterDef p xs
  where xs' = filter p xs
  
partitionDef :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
partitionDef p xs = (filter p xs, filter (not . p) xs)
						 
prop_partition :: (CheckableSequence s, Eq a, Eq (s a)) 
                  => s a -> (a -> Bool) -> Bool
prop_partition xs p = invariant xs' && invariant xs'' 
                      && (xs', xs'') == partitionDef p xs
  where (xs', xs'') = partition p xs

takeWhileDef :: Sequence s => (a -> Bool) -> s a -> s a
takeWhileDef p xs = case lview xs of
                      Nothing2 -> empty
                      Just2 x xs' -> if p x then cons x (takeWhileDef p xs')
		                            else empty
		   
prop_takeWhile :: (CheckableSequence s, Eq a, Eq (s a)) 
                  => s a -> (a -> Bool) -> Bool
prop_takeWhile xs p = invariant xs' && xs' == takeWhileDef p xs
  where xs' = takeWhile p xs

dropWhileDef :: Sequence s => (a -> Bool) -> s a -> s a
dropWhileDef p xs = case lview xs of
                      Nothing2 -> empty
                      Just2 x xs' -> if p x then (dropWhileDef p xs')
		                            else xs
		   
prop_dropWhile :: (CheckableSequence s, Eq a, Eq (s a)) 
                  => s a -> (a -> Bool) -> Bool
prop_dropWhile xs p = invariant xs' && xs' == dropWhileDef p xs
  where xs' = dropWhile p xs
  
splitWhileDef :: Sequence s => (a -> Bool) -> s a -> (s a, s a)
splitWhileDef p xs = (takeWhile p xs, dropWhile p xs)

prop_splitWhile :: (CheckableSequence s, Eq a, Eq (s a)) 
                   => s a -> (a -> Bool) -> Bool
prop_splitWhile xs p = invariant xs' && invariant xs'' 
                       && (xs', xs'') == splitWhileDef p xs
  where (xs', xs'') = splitWhile p xs

inBoundsDef :: Sequence s => s a -> Int -> Bool
inBoundsDef xs i = 0<=i && i<size xs

prop_inBounds :: (Sequence s, Eq a, Eq (s a)) => s a -> Int -> Bool
prop_inBounds xs i = inBounds xs i == inBoundsDef xs i

lookupDef :: Sequence s => s a -> Int -> a
lookupDef xs 0 = lhead xs
lookupDef xs i = lookupDef (ltail xs) (i-1)

prop_lookup :: (Sequence s, Eq a, Eq (s a)) => s a -> Int -> Property
prop_lookup xs i = inBounds xs i ==> lookup xs i == lookupDef xs i

lookupMDef :: Sequence s => s a -> Int -> Maybe a
lookupMDef xs i | not (inBounds xs i) = Nothing
lookupMDef xs 0 = Just (lhead xs)
lookupMDef xs i = lookupMDef (ltail xs) (i-1)

prop_lookupM :: (Sequence s, Eq a, Eq (s a)) => s a -> Int -> Bool
prop_lookupM xs i = lookupM xs i == lookupMDef xs i

lookupWithDefaultDef :: Sequence s => a -> s a -> Int -> a
lookupWithDefaultDef d xs i | not (inBounds xs i) = d
lookupWithDefaultDef d xs 0 = lhead xs
lookupWithDefaultDef d xs i = lookupWithDefaultDef d (ltail xs) (i-1)

prop_lookupWithDefault :: (Sequence s, Eq a, Eq (s a)) 
                          => s a -> a -> Int -> Bool
prop_lookupWithDefault xs d i 
  = lookupWithDefault d xs i == lookupWithDefaultDef d xs i

updateDef :: Sequence s => Int -> a -> s a -> s a
updateDef i e xs | not (inBounds xs i) = xs
updateDef 0 e xs = cons e (ltail xs)
updateDef i e xs = let Just2 x xs' = lview xs 
                   in cons x (updateDef (i-1) e xs')

prop_update :: (CheckableSequence s, Eq a, Eq (s a)) 
               => s a -> Int -> a -> Bool
prop_update xs i e = invariant xs' && xs' == updateDef i e xs
  where xs' = update i e xs

adjustDef :: Sequence s => (a -> a) -> Int -> s a -> s a
adjustDef f i xs | not (inBounds xs i) = xs
adjustDef f 0 xs = let Just2 x xs' = lview xs 
                   in cons (f x) xs'
adjustDef f i xs = let Just2 x xs' = lview xs 
                   in cons x (adjustDef f (i-1) xs')

prop_adjust :: (CheckableSequence s, Eq a, Eq (s a)) 
               => s a -> (a -> a) -> Int -> Bool
prop_adjust xs f i = invariant xs' && xs' == adjustDef f i xs
  where xs' = adjust f i xs

mapWithIndexDef :: Sequence s => (Int -> a -> b) -> s a -> s b
mapWithIndexDef f xs 
  = case rview xs of
      Nothing2 -> empty
      Just2 xs' x -> snoc (mapWithIndexDef f xs') (f (size xs') x)

prop_mapWithIndex :: (CheckableSequence s, Eq a, Eq (s a)) 
                     => s a -> (Int -> a -> a) -> Bool
prop_mapWithIndex xs f = invariant xs' && xs' == mapWithIndexDef f xs
  where xs' = mapWithIndex f xs
  
foldrWithIndexDef :: Sequence s => (Int -> a -> b -> b) -> b -> s a -> b
foldrWithIndexDef f e xs 
  = case rview xs of
      Nothing2 -> e
      Just2 xs' x -> foldrWithIndexDef f (f (size xs') x e) xs'

prop_foldrWithIndex :: (Sequence s, Eq a, Eq (s a)) 
              => s a -> (Int -> a -> a -> a) -> a -> Bool 
prop_foldrWithIndex xs f e = foldrWithIndex f e xs == foldrWithIndexDef f e xs
  
foldlWithIndexDef :: Sequence s => (b -> Int -> a -> b) -> b -> s a -> b
foldlWithIndexDef f e xs 
  = case rview xs of
      Nothing2 -> e
      Just2 xs' x -> f (foldlWithIndexDef f e xs') (size xs') x

prop_foldlWithIndex :: (Sequence s, Eq a, Eq (s a)) 
              => s a -> (a -> Int -> a -> a) -> a -> Bool 
prop_foldlWithIndex xs f e = foldlWithIndex f e xs == foldlWithIndexDef f e xs

zipWithDef :: Sequence s => (a -> b -> c) -> s a -> s b -> s c
zipWithDef f xs ys 
  | null xs || null ys = empty
  |otherwise = let Just2 x xs' = lview xs
                   Just2 y ys' = lview ys
	       in cons (f x y) (zipWithDef f xs' ys')
\end{code}
}

As we only use a generator for one type of element, we have to restrict the type
of the arguments to the \lstinline!zip! operations in the properties to be of
the same type.
\begin{code}  
prop_zipWith :: (CheckableSequence s, Eq a, Eq (s a))
                => s a -> s a -> (a -> a -> a) -> Bool
prop_zipWith xs ys f = invariant xys && xys == zipWithDef f xs ys
  where xys = zipWith f xs ys
\end{code}
\hide{
\begin{code}
zipWith3Def :: Sequence s => (a -> b -> c -> d) -> s a -> s b -> s c -> s d
zipWith3Def f xs ys zs
  | null xs || null ys || null zs = empty
  |otherwise = let Just2 x xs' = lview xs
                   Just2 y ys' = lview ys
                   Just2 z zs' = lview zs
	       in cons (f x y z) (zipWith3Def f xs' ys' zs')
  
prop_zipWith3 :: (CheckableSequence s, Eq a, Eq (s a))
                 => s a -> s a -> s a -> (a -> a -> a -> a) -> Bool
prop_zipWith3 xs ys zs f = invariant xyzs && xyzs == zipWith3Def f xs ys zs
  where xyzs = zipWith3 f xs ys zs
  
zipDef :: Sequence s => s a -> s b -> s (a, b)
zipDef xs ys = zipWith (\x y -> (x, y)) xs ys

prop_zip :: (CheckableSequence s, Eq a, Eq (s (a, a))) => s a -> s a -> Bool
prop_zip xs ys = invariant xys && xys == zipDef xs ys
  where xys = zip xs ys

zip3Def :: Sequence s => s a -> s b -> s c -> s (a, b, c)
zip3Def xs ys zs = zipWith3 (\x y z -> (x, y, z)) xs ys zs

prop_zip3 :: (CheckableSequence s, Eq a, Eq (s (a, a, a))) 
             => s a -> s a -> s a -> Bool
prop_zip3 xs ys zs = invariant xyzs && xyzs == zip3Def xs ys zs
  where xyzs = zip3 xs ys zs

unzipDef :: Sequence s => s (a, b) -> (s a, s b)
unzipDef xs = (map fst xs, map snd xs)
\end{code}
}
As we will only be able to quantify explicitly over \lstinline!Seq a!, we cannot
generate the arguments to the \lstinline!unzip! operations this way, but have
to use implicit quantification. This however enforces us to resolve the type in
another way, so we again take the invariant as an extra argument.
\begin{code}
prop_unzip :: (Sequence s, Eq a, Eq (s a)) 
              => (s a -> Bool) -> s (a, a) -> Bool
prop_unzip inv xys = inv xs && inv ys && (xs, ys) == unzipDef xys
  where (xs, ys) = unzip xys
\end{code}
\hide{
\begin{code}
unzip3Def :: Sequence s => s (a, b, c) -> (s a, s b, s c)
unzip3Def xs = (map fst3 xs, map snd3 xs, map thd3 xs)
  where fst3 (x, _, _) = x
        snd3 (_, y, _) = y
        thd3 (_, _, z) = z

prop_unzip3 :: (Sequence s, Eq a, Eq (s a)) 
              => (s a -> Bool) -> s (a, a, a) -> Bool
prop_unzip3 inv xyzs = inv xs && inv ys && inv zs 
                       && (xs, ys, zs) == unzip3Def xyzs  
  where (xs, ys, zs) = unzip3 xyzs	      

unzipWithDef :: Sequence s => (a -> b) -> (a -> c) -> s a -> (s b, s c)
unzipWithDef f g xs = (map f xs, map g xs)

prop_unzipWith :: (CheckableSequence s, Eq a, Eq (s a))
                  => s a -> (a -> a) -> (a -> a) -> Bool
prop_unzipWith xs f g = invariant xs' && invariant xs'' 
                        && (xs', xs'') == unzipWithDef f g xs  
  where (xs', xs'') = unzipWith f g xs

unzipWith3Def :: Sequence s 
                 => (a -> b) -> (a -> c) -> (a -> d) -> s a -> (s b, s c, s d)
unzipWith3Def f g h xs = (map f xs, map g xs, map h xs)

prop_unzipWith3 :: (CheckableSequence s, Eq a, Eq (s a))
                   => s a -> (a -> a) -> (a -> a) -> (a -> a) -> Bool
prop_unzipWith3 xs f g h = invariant xs' && invariant xs'' && invariant xs'''
                           && (xs', xs'', xs''') == unzipWith3Def f g h xs  
  where (xs', xs'', xs''') = unzipWith3 f g h xs
\end{code}
}
To finally test all properties for a given data-type we have to collect them
in a function that can be parameterized in the type it checks. Again, we just
add the invariant as an additional argument just to resolve the overloading; to
get meaningful results, the invariant given as an argument should always be
the same as the one in the \lstinline!CheckableSequence! class. The typical
invocation will hence look like 
\begin{lstlisting}
checkseq (invariant :: Seq Int -> Bool)
\end{lstlisting}
\hide{
\begin{code}
checkseq :: (Sequence s, Eq a, Arbitrary (s (s a)), Show (s (s a)), 
             Arbitrary a, Show a, Eq (s a), Arbitrary (s a), Show (s a), 
	     Arbitrary (s (a, a)), Show (s (a, a)), Eq (s (a, a)), 
	     Arbitrary (s (a, a, a)), Eq (s (a, a, a)), CheckableSequence s, 
	     Show (s (a, a, a)))            
	     => (s a -> Bool) -> IO ()
\end{code}
}

We start by checking that both \lstinline!arbitrary! and
\lstinline!arbitraryWith! generate sequences that actually fulfill the
invariant. This also defines the type of the generator \lstinline!seqs!, that
is enforced to be monomorphic throughout the function.
\begin{code}
checkseq inv = do putStr "Checking arbitrary...         "                  
                  quickCheck (forAll seqs inv)                        
                  putStr "Checking arbitraryWith...     "                  
                  quickCheck (\xs -> forAllWith xs 
		              (\s -> property $ inv s && xs == toListDef s))
\end{code}
The remaining properties all follow one of the two following pattern,
i.e. either explicit quantification or implicit quantification but with the
invariant \lstinline!inv! as an extra argument.
\begin{code}
		  putStr "Checking lview ...            "
                  quickCheck (forAll seqs prop_lview) 
		  putStr "Checking empty...             "
                  quickCheck (prop_empty inv)
                  putStr "Checking == ...               "
                  quickCheck (forAll seqs prop_equals)    
\end{code}
With the exception of the equality test, where the special generator is used
to generate sequences containing the same elements.
\begin{code}
		  putStr "Checking == (equal cases)...  "
		  quickCheck (\xs -> forAllWith xs
		              (\s1 -> forAllWith xs
			       (\s2 -> property $ prop_equals s1 s2)))
\end{code}
\hide{
\begin{code}
		  putStr "Checking cons...              "
		  quickCheck (forAll seqs prop_cons)      
		  putStr "Checking null...              "
                  quickCheck (forAll seqs prop_null)
		  putStr "Checking single...            "
                  quickCheck (prop_single inv)
		  putStr "Checking snoc...              "
		  quickCheck (forAll seqs prop_snoc)		  
		  putStr "Checking append...            "
                  quickCheck (forAll seqs prop_append)
		  putStr "Checking fromList...          "
                  quickCheck (prop_fromList inv)
		  putStr "Checking copy...              "
                  quickCheck (prop_copy inv)                  
		  putStr "Checking tabulate...          "
                  quickCheck (prop_tabulate inv)
		  putStr "Checking lhead...             "
		  quickCheck (forAll seqs prop_lhead)
		  putStr "Checking ltail...             "
                  quickCheck (forAll seqs prop_ltail)
		  putStr "Checking rview...             "
		  quickCheck (forAll seqs prop_rview)
		  putStr "Checking rhead...             "
		  quickCheck (forAll seqs prop_rhead)
		  putStr "Checking rtail...             "
		  quickCheck (forAll seqs prop_rtail)
		  putStr "Checking size...              "
		  quickCheck (forAll seqs prop_size)
		  putStr "Checking toList...            "
       		  quickCheck (forAll seqs prop_toList)
		  putStr "Checking concat...            "
		  quickCheck (forAll gen20 (prop_concat inv))
		  putStr "Checking reverse...           "
		  quickCheck (forAll seqs prop_reverse)
		  putStr "Checking reverseOnto...       "
		  quickCheck (forAll seqs prop_reverseOnto)
		  putStr "Checking map...               "
		  quickCheck (forAll seqs prop_map)
\end{code}
}
When testing \lstinline!concatMap!, we use a down-sized generator for the
function argument to avoid producing too large sequences.
\begin{code}
		  putStr "Checking concatMap...         "
		  quickCheck (forAll seqs 
                              (\xs -> forAll gen20  
                               (\f -> (prop_concatMap xs f))))
\end{code}
\hide{
\begin{code}
		  putStr "Checking foldr...             "
		  quickCheck (forAll seqs prop_foldr)
		  putStr "Checking foldl...             "
		  quickCheck (forAll seqs prop_foldl)
		  putStr "Checking foldr1...            "
		  quickCheck (forAll seqs prop_foldr1)
		  putStr "Checking foldl1...            "
		  quickCheck (forAll seqs prop_foldl1)
		  putStr "Checking reducer...           "
		  quickCheck (forAll seqs prop_reducer)
		  putStr "Checking reducel...           "
		  quickCheck (forAll seqs prop_reducel)
		  putStr "Checking reduce1...           "
		  quickCheck (forAll seqs prop_reduce1)
		  putStr "Checking take...              "
		  quickCheck (forAll seqs prop_take)
		  putStr "Checking drop...              "
		  quickCheck (forAll seqs prop_drop)
		  putStr "Checking splitAt...           "
		  quickCheck (forAll seqs prop_splitAt)
		  putStr "Checking subseq...            "
		  quickCheck (forAll seqs prop_subseq)
		  putStr "Checking filter...            "
		  quickCheck (forAll seqs prop_filter)
		  putStr "Checking partition...         "
		  quickCheck (forAll seqs prop_partition)
		  putStr "Checking takeWhile...         "
		  quickCheck (forAll seqs prop_takeWhile)
		  putStr "Checking dropWhile...         "
		  quickCheck (forAll seqs prop_dropWhile)
		  putStr "Checking splitWhile...        "
		  quickCheck (forAll seqs prop_splitWhile)
		  putStr "Checking inBounds...          "
		  quickCheck (forAll seqs prop_inBounds)
		  putStr "Checking lookup...            "
		  quickCheck (forAll seqs prop_lookup)
		  putStr "Checking lookupM...           "
		  quickCheck (forAll seqs prop_lookupM)
		  putStr "Checking lookupWithDefault... "
		  quickCheck (forAll seqs prop_lookupWithDefault)
		  putStr "Checking update...            "
		  quickCheck (forAll seqs prop_update)
		  putStr "Checking adjust...            "
		  quickCheck (forAll seqs prop_adjust)
		  putStr "Checking mapWithIndex...      "
		  quickCheck (forAll seqs prop_mapWithIndex)
		  putStr "Checking foldrWithIndex...    "
		  quickCheck (forAll seqs prop_foldrWithIndex)
		  putStr "Checking foldlWithIndex...    "
		  quickCheck (forAll seqs prop_foldlWithIndex)
		  putStr "Checking zipWith...           "
		  quickCheck (forAll seqs prop_zipWith)
		  putStr "Checking zipWith3...          "
		  quickCheck (forAll seqs prop_zipWith3)
		  putStr "Checking zip...               "
		  quickCheck (forAll seqs prop_zip)
		  putStr "Checking zip3...              "
		  quickCheck (forAll seqs prop_zip3)
		  putStr "Checking unzip...             "
		  quickCheck (prop_unzip inv)
		  putStr "Checking unzip3...            "
		  quickCheck (prop_unzip3 inv)
		  putStr "Checking unzipWith...         "
		  quickCheck (forAll seqs prop_unzipWith)
		  putStr "Checking unzipWith3...        "
		  quickCheck (forAll seqs prop_unzipWith3)
\end{code}
}

For the remaining checks we refer to appendix \ref{chapter:checkseqsource}, but
skip them here as they do not provide any new insights.

\begin{code}
               where seqs = arbitrary
	             genWith = \xs -> arbitraryWith xs
		     forAllWith = \xs prop -> forAll (genWith xs) prop
		     gen20 :: Arbitrary a => Gen a
	             gen20 = sized (\n -> resize (min 20 n) arbitrary)

\end{code}

All implementations described in the following contain a function
\begin{lstlisting}
checkme = CheckSeq.checkseq (invariant :: Seq Int -> Bool)
\end{lstlisting}
to simplify testing them, i.e. calling \lstinline!checkme! is sufficient. As all
sequence operations are polymorphic in the type of the element, it is easy to
see by Wadler's parametricity theorem that if a property for them holds for some
element type \lstinline!a!, then it holds for any type of same or lower
cardinality. As we furthermore only perform a limited number of tests, the
difference between types with infinite domains and those with sufficiently
large, but finite domains (like \lstinline!Int!) becomes irrelevant. This
justifies why we only test for an element type of \lstinline!Int!, but are still
confident that the properties hold for other types as well.

