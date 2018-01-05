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
module SimpleDeque (
    -- type of simple deque
    Seq, -- instance of Sequence, Functor, Monad, MonadPlus

    -- sequence operations
    empty,single,cons,snoc,append,lview,lhead,ltail,rview,rhead,rtail,
    null,size,concat,reverse,reverseOnto,fromList,toList,
    map,concatMap,foldr,foldl,foldr1,foldl1,reducer,reducel,reduce1,
    copy,tabulate,inBounds,lookup,lookupM,lookupWithDefault,update,adjust,
    mapWithIndex,foldrWithIndex,foldlWithIndex,
    take,drop,splitAt,subseq,filter,partition,takeWhile,dropWhile,splitWhile,
    zip,zip3,zipWith,zipWith3,unzip,unzip3,unzipWith,unzipWith3,

    -- documentation
    moduleName,

    -- re-export view type from EdisonPrelude for convenience
    Maybe2(Just2,Nothing2)
) where

import Prelude hiding (concat,reverse,map,concatMap,foldr,foldl,foldr1,foldl1,
                       filter,takeWhile,dropWhile,lookup,take,drop,splitAt,
                       zip,zip3,zipWith,zipWith3,unzip,unzip3,null)

import EdisonPrelude(Maybe2(Just2,Nothing2))
import qualified Sequence as S ( Sequence(..) )
import SequenceDefaults
import qualified ListSeq as L
import Monad
import QuickCheck
import qualified CheckSeq

-- signatures for exported functions
moduleName     :: String
empty          :: Seq a
single         :: a -> Seq a
cons           :: a -> Seq a -> Seq a
snoc           :: Seq a -> a -> Seq a
append         :: Seq a -> Seq a -> Seq a
lview          :: Seq a -> Maybe2 a (Seq a)
lhead          :: Seq a -> a
ltail          :: Seq a -> Seq a
rview          :: Seq a -> Maybe2 (Seq a) a
rhead          :: Seq a -> a
rtail          :: Seq a -> Seq a
null           :: Seq a -> Bool
size           :: Seq a -> Int
concat         :: Seq (Seq a) -> Seq a
reverse        :: Seq a -> Seq a
reverseOnto    :: Seq a -> Seq a -> Seq a
fromList       :: [a] -> Seq a
toList         :: Seq a -> [a]
map            :: (a -> b) -> Seq a -> Seq b
concatMap      :: (a -> Seq b) -> Seq a -> Seq b
foldr          :: (a -> b -> b) -> b -> Seq a -> b
foldl          :: (b -> a -> b) -> b -> Seq a -> b
foldr1         :: (a -> a -> a) -> Seq a -> a
foldl1         :: (a -> a -> a) -> Seq a -> a
reducer        :: (a -> a -> a) -> a -> Seq a -> a
reducel        :: (a -> a -> a) -> a -> Seq a -> a
reduce1        :: (a -> a -> a) -> Seq a -> a
copy           :: Int -> a -> Seq a
tabulate       :: Int -> (Int -> a) -> Seq a
inBounds       :: Seq a -> Int -> Bool
lookup         :: Seq a -> Int -> a
lookupM        :: Seq a -> Int -> Maybe a
lookupWithDefault :: a -> Seq a -> Int -> a
update         :: Int -> a -> Seq a -> Seq a
adjust         :: (a -> a) -> Int -> Seq a -> Seq a
mapWithIndex   :: (Int -> a -> b) -> Seq a -> Seq b
foldrWithIndex :: (Int -> a -> b -> b) -> b -> Seq a -> b
foldlWithIndex :: (b -> Int -> a -> b) -> b -> Seq a -> b
take           :: Int -> Seq a -> Seq a
drop           :: Int -> Seq a -> Seq a
splitAt        :: Int -> Seq a -> (Seq a, Seq a)
subseq         :: Int -> Int -> Seq a -> Seq a
filter         :: (a -> Bool) -> Seq a -> Seq a
partition      :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
takeWhile      :: (a -> Bool) -> Seq a -> Seq a
dropWhile      :: (a -> Bool) -> Seq a -> Seq a
splitWhile     :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
zip            :: Seq a -> Seq b -> Seq (a,b)
zip3           :: Seq a -> Seq b -> Seq c -> Seq (a,b,c)
zipWith        :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith3       :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
unzip          :: Seq (a,b) -> (Seq a, Seq b)
unzip3         :: Seq (a,b,c) -> (Seq a, Seq b, Seq c)
unzipWith      :: (a -> b) -> (a -> c) -> Seq a -> (Seq b, Seq c)
unzipWith3     :: (a -> b) -> (a -> c) 
                  -> (a -> d) -> Seq a -> (Seq b, Seq c, Seq d)
moduleName = "SimpleDeque"
\end{code}
}
\label{sec:SimpleDeque}
The simple deque implementation suggested by Hoogerwoord 
\cite{hoogerwoord_jfp92} is very similar to the \lstinline$SimpleQueue$, 
and therefore has very similar properties, i.e. constant amortised time 
bounds for the typical deque operations if used non-persistently. 
The data type also consists of two lists, one front and one rear list.
\begin{code}
data Seq a = D [a] [a]
\end{code}
Contrary to the \lstinline$SimpleQueue$, the two lists are handled 
symmetrically, and the maintained invariant is that whenever the deque contains
at least two elements, both lists are non-empty. 
However, to reduce the number of cases that must be distinguished, we 
arbitrarily demand that the element of a singleton deque is stored in the 
front list. 
\begin{code}
invariant :: Seq a -> Bool
invariant (D [] []) = True         -- empty
invariant (D [_] []) = True        -- one element
invariant (D (_:_) (_:_)) = True   -- two or more elements
invariant (D _ _) = False
\end{code}

The runtime analysis is carried out assuming strictness and applying the 
physicist's method for amortisation, where the potential is defined to be
the difference of the length of the two lists. Hoogerwoord showed that with this
potential, \lstinline!empty!, \lstinline!ltail!, \lstinline!rtail!,
\lstinline!cons!, \lstinline!snoc! and \lstinline!reverse! all run in \Oof{1}
amortised.

Except for \lstinline!ltail! and \lstinline!rtail!, those operations are almost
literally translated into Haskell from Hoogerwoords article. For the two
\lstinline!tail!s, we first define a function that, given a front and a rear
list, creates a deque that fulfils the invariant.
\begin{code}
makeD xs [] = D xs' (L.reverse ys')
  where (xs', ys') = L.splitAt ((L.size xs + 1) `div` 2) xs
makeD [] ys = D (L.reverse xs') ys'
  where (ys', xs') = L.splitAt (L.size ys `div` 2) ys
makeD xs ys = D xs ys
\end{code}
The first two cases handle the situation of (at least) one list being empty,
where the other list is split into half and one of the halves is reversed.
If the length of the list is odd, the split is performed such that the front
list gets one element longer than the rear to handle the singleton case as
demanded.
The runtime complexity of this obviously is linear in the length of the 
non-empty list, the resulting potential is at most one. In the third case
both lists are non-empty and thus can be used unaltered, what requires constant
time. At first, the calculation of the size of the non-empty list seems to be 
avoidable by maintaining separate size fields in the data-type. This approach 
would, however, introduce an additional overhead to all operations, thereby 
effectively decreasing the overall performance if mainly operations are used 
that add or remove single elements.

The two basic constructors are straight-forward and obviously run 
in \Oof{1} and result in a potential of zero or one, respectively.
\begin{code}
empty = D [] []
single x = D [x] []
\end{code}
The \lstinline$cons$ and \lstinline$snoc$ operations are similarly easy to 
implement.
\begin{code}
cons x (D xs []) = D [x] xs        -- xs contains zero or one element
cons x (D xs ys) = D (x:xs) ys

snoc (D [] _) y = D [y] []         -- ys is empty
snoc (D xs ys) y = D xs (y:ys)
\end{code}
If both lists are non-empty, the new element is added to the respective list,
taking constant time and increasing the potential by at most 1. If the rear
list is empty when \lstinline$cons$ is invoked, the old front list becomes
the new rear list (reversing is unnecessary as the front list has at most 
one element) and the new front list is the singleton list containing the
new element. For an empty front list when \lstinline$snoc$ is invoked, things
are even easier as the rear list is guaranteed to be empty. Hence, these cases
also require constant time and increase the potential by at most one.

The \lstinline$append$ operation distinguishes three cases, depending on the
second deque.
\begin{code}
append d1 (D [] _) = d1
append d1 (D [x2] []) = snoc d1 x2
append (D xs1 ys1) (D xs2 ys2) =
    D (xs1 ++ L.reverseOnto ys1 xs2) ys2
\end{code}
If the second deque contains no or only one element, the \lstinline$append$
degenerates to identity or \lstinline$snoc$, respectively, yielding constant
runtime. The non-trivial case obviously requires \Oof{n_1} steps in total,
$|\lstinline!xs1!|$ for the append and $|\lstinline!ys1!|$ for the 
\lstinline$reverseOnto$, with $n_1=|\lstinline!xs1!|+|\lstinline!ys1!|$ being 
the size of the first deque. For the initial potential we have 
$\Phi(n)=\left||\lstinline!xs1!|-|\lstinline!ys1!|\right|+
         \left||\lstinline!xs2!|-|\lstinline!ys2!|\right|$, 
while the resulting potential is 
\begin{displaymath}
\Phi(n+1)=\left|(|\lstinline!xs1!|+|\lstinline!ys1!|+
          |\lstinline!xs2!|)-|\lstinline!ys2!|\right|
          \leq
          \left||\lstinline!xs1!|+|\lstinline!ys1!|\right|+
          \left||\lstinline!xs2!|-|\lstinline!ys2!|\right|
          = n_1 + \left||\lstinline!xs2!|-|\lstinline!ys2!|\right|,
\end{displaymath}
so the change in potential is bounded by
\begin{displaymath}
\Phi(n+1)-\Phi(n)
\leq n_1 - \left||\lstinline!xs1!|-|\lstinline!ys1!|\right|
\leq n_1,
\end{displaymath}
giving an amortised time complexity of \Oof{n_1}.

Accessing the left-most element requires distinction between three cases.
\begin{code}
lview (D [] _) = Nothing2
lview (D [x] ys) = Just2 x (makeD [] ys)
lview (D (x:xs) ys) = Just2 x (D xs ys)

lhead (D [] _) = error "SimpleDeque.lhead: empty sequence"
lhead (D (x:_) _) = x

ltail d@(D [] _) = d
ltail (D [x] ys) = makeD [] ys
ltail (D (x:xs) ys) = D xs ys
\end{code}
If the front list is empty, the whole deque is empty. If the front list 
contains exactly one element, this is the demanded one, but calculating
the tail requires a call to \lstinline$makeD$ to reestablish the invariant.
(As \lstinline$lhead$ does not produce a tail, it does not distinguish this 
case.) Otherwise, the front list contains at least two elements, hence taking
the tail of the list as new front will not contradict the invariant. Except for
the second case, the operations have amortised runtime of \Oof{1}, as they
obviously require constant runtime and change the potential by at most one.
The second case needs special attention. The call to \lstinline$makeD$ has
runtime complexity \Oof{n}, but the potential is reduced from 
$|1-(n-1)|=n-2$ to 0 or 1, i.e. at least by $n-3$, compensating the linear
runtime, thereby giving an amortised runtime \Oof{1}.

Accessing the right-most element is very similar.
\begin{code}
rview (D [] _) = Nothing2
rview (D [x] []) = Just2 (D [] []) x
rview (D xs (y:ys)) = Just2 (makeD xs ys) y

rhead (D [] _) = error "SimpleDeque.rhead: empty sequence"
rhead (D [x] []) = x
rhead (D _ (y:_)) = y

rtail (D _ []) = D [] []   -- if rear is empty, at most one element in deque
rtail (D xs (_:ys)) = makeD xs ys
\end{code}
Although the distinguished cases are slightly different, due to the 
asymmetrical handling of singleton deques, the above argument can easily be
adapted to establish constant amortised runtime for these operations, too.

Checking whether a deque contains any elements at all obviously run in
constant time.
\begin{code}
null (D [] _) = True
null _ = False
\end{code}
Calculation of the size of the deque requires determining the length of both 
lists, resulting in a runtime complexity of \Oof{n}.
\begin{code}
size (D xs ys) = length xs + length ys
\end{code}
Thanks to the symmetry, reversing a deque is fairly simple.
\begin{code}
reverse d@(D xs []) = d
reverse (D xs ys) = D ys xs
\end{code}
If the deque contains at least two elements, both list are non-empty and can
thus simply be exchanged. Otherwise, the deque contains zero or one element
and is therefore equal to its reverse. Both cases obviously run in constant
time and leave the potential unaltered, yielding constant amortised runtime.

The \lstinline$reverseOnto$ operation is very similar to \lstinline$append$.
\begin{code}
reverseOnto d1@(D xs1 ys1) (D [] _) = reverse d1
reverseOnto d1@(D xs1 ys1) (D [x2] []) = snoc (reverse d1) x2
reverseOnto (D xs1 ys1) (D xs2 ys2) =
    D (ys1 ++ L.reverseOnto xs1 xs2) ys2
\end{code}
Considering that \lstinline$reverse$ runs in \Oof{1} and that the non-trivial 
case is equal to \lstinline$append$ except for the exchange of \lstinline$xs1$ 
and \lstinline$ys1$, it is easily seen that with an argument very similar to 
that for \lstinline$append$, the amortised runtime turns out to be \Oof{n_1}.

Using the helper function, conversion of a list to a deque is straight forward,
as is the conversion back to a list.
\begin{code}
fromList xs = makeD xs []

toList (D xs []) = xs
toList (D xs ys) = xs ++ L.reverse ys
\end{code}
Both operations clearly take \Oof{n} time, but is should be noted that 
\lstinline$toList$ returns a result that can be calculated incrementally up to
the end of the front list in a lazy manner.

The \lstinline$map$ and \lstinline$fold$/\lstinline$reduce$-family of operations 
are simply performed by calling the appropriate list functions on the front and 
rear list. The required runtime is mainly determined by applying the function 
argument \lstinline$f$ $n$ times.

\begin{code}
map f (D xs ys) = D (L.map f xs) (L.map f ys)

foldr f e (D xs ys) = L.foldr f (L.foldl (flip f) e ys) xs

foldl f e (D xs ys) = L.foldr (flip f) (L.foldl f e xs) ys

foldr1 f (D [] _) = error "SimpleDeque.foldr1: empty sequence"
foldr1 f (D [x] []) = x
foldr1 f (D xs ys) = L.foldr f (L.foldl1 (flip f) ys) xs

foldl1 f (D [] _) = error "SimpleDeque.foldl1: empty sequence"
foldl1 f (D xs ys) = L.foldr (flip f) (L.foldl1 f xs) ys

reducer f e (D xs ys) = L.reducer f (L.reducel (flip f) e ys) xs

reducel f e (D xs ys) = L.reducer (flip f) (L.reducel f e xs) ys

reduce1 f (D [] _) = error "SimpleDeque.reduce1: empty sequence"
reduce1 f (D xs ys) = L.reducer (flip f) (L.reduce1 f xs) ys
\end{code}

Splitting a deque at a specified position can be performed by repeated 
application of \lstinline$lview$ and \lstinline$cons$. As both \lstinline$lview$ 
and \lstinline$cons$ have constant amortised runtime and are called $i$ times, 
the total amortised runtime is \Oof{i}.
\begin{code}
splitAt _ d@(D [] []) = (d, d)
splitAt i d | i<=0      = (empty, d)
            | otherwise = let Just2 x d' = lview d
                              (d1, d2) = splitAt (i-1) d'
                          in (x `cons` d1, d2)
\end{code}

The \lstinline$copy$ constructor is easily implemented using the 
\lstinline$copy$ for lists to generate front and end lists with suitable
sizes.
\begin{code}
copy n x = D (L.copy ((n+1) `div` 2) x) (L.copy (n `div` 2) x)
\end{code}
The required  runtime is \Oof{n} and the potential of the resulting deque
is at most 1, do the amortised runtime is \Oof{n}.

Similarly, \lstinline$tabulate$ can be implemented by constructing a deque with
appropriately enumerated lists and then applying \lstinline$map$ to it.
\begin{code}
tabulate n f | n <= 0    = empty
             | otherwise = let k=(n+1) `div` 2 - 1
                           in  map f (D [0..k] [n-1, n-2..k+1])
\end{code}
Application of \lstinline$f$ $n$ times is again the predominant part of the 
required runtime.

Like \lstinline$map$, the \lstinline$filter$ and \lstinline$partition$ 
operations can be carried out directly by using the respective list operations.
But as this my cause one of the involved lists to become empty, a call to
\lstinline$makeD$ is required. Both the runtime of the \lstinline$makeD$ and 
the possible increase of potential are\Oof{n}, as is the number of applications 
of~\lstinline$p$.
\begin{code}
filter p (D xs ys) = makeD (L.filter p xs) (L.filter p ys)

partition p (D xs ys)
  = (makeD xsT ysT, makeD xsF ysF)
 where
   (xsT,xsF) = L.partition p xs
   (ysT,ysF) = L.partition p ys
\end{code}

The check whether a certain index is within the deque can be done by checking 
whether it is within the catenation of the front and rear list; reversing the 
rear is not necessary as it does not change its length. Observing the 
incrementality of the \lstinline$++$, this implementation has time complexity 
\Oof{i}.
\begin{code}
inBounds (D xs ys) = L.inBounds (xs ++ ys)
\end{code}

The remaining operations are all implemented using defaults.

Concatenating multiple deques is performed by right-associatively 
\lstinline$append$ing them using \lstinline$foldr$. 
\begin{code}
concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
\end{code}
The \lstinline$foldr$ runs in \Oof{n} plus the time needed for the 
\lstinline$append$s, which is \Oof{m} with $m$ being the length of the 
resulting deque, plus the time needed for the \lstinline$map$ for 
\lstinline$concatMap$, giving a total runtime of \Oof{n+m}.

Like \lstinline$splitAt$, also \lstinline$take$ and \lstinline$drop$ can be 
implemented by repeated calls of \lstinline$lview$ or \lstinline$ltail$, 
respectively. As \lstinline$lview$ and \lstinline$ltail$ run in constant 
amortised time, this obviously results in an amortised time complexity of 
\Oof{i}.
\begin{code}
take = takeUsingLview
drop = dropUsingLtail
\end{code}

Using \lstinline$drop$, it is easy to implement \lstinline$lookup$ and 
derivatives.
\begin{code}
lookupM = lookupMUsingDrop
lookup = lookupUsingLookupM
lookupWithDefault = lookupWithDefaultUsingLookupM
\end{code}
Unfortunately, as the original deque may be still be used afterwards, the 
amortised runtime analysis that has been done for \lstinline$ltail$ and hence 
for \lstinline$drop$ and relied on non-persistent usage cannot be 
transferred to these operations. Instead, if the index happens to lie in the 
rear list, the operations always take \Oof{n}.

This is not the case for \lstinline$adjust$ and \lstinline$update$, which are 
implemented by splitting the deque, updating the element and appending
the two parts again.
\begin{code}
adjust = adjustUsingSplitAt
update = updateUsingAdjust
\end{code}
As both the splitting and the appending take time \Oof{i} amortised, this also is 
the total amortised time complexity.

The *\lstinline$WithIndex$-operations utilise the \lstinline$toList$ function. 
As the conversion to a list (and back in case of \lstinline$map$) is \Oof{n}, 
applying \lstinline$f$ $n$ times determines the runtime of these operations.
\begin{code}
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
\end{code}

Using \lstinline$drop$ and \lstinline$take$, it is easy to implement 
\lstinline$subseq$. The total amortised time complexity is that of 
\lstinline$drop$ plus that of \lstinline$take$, i.e. \Oof{i+j} (for 
\lstinline$subseq i j$).
\begin{code}
subseq = subseqDefault
\end{code}

The *\lstinline$While$ operations traverse the deque by repeated application of
\lstinline$lview$. Per accepted element, this takes constant amortised time for 
\lstinline$lview$, and \lstinline$cons$ if applicable, plus the time required 
for evaluation of the predicate, where at most $n$ elements can be accepted.
\begin{code}
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
\end{code}

The \lstinline$zip$/\lstinline$unzip$-operations all operate by conversion to 
lists internally.
\begin{code}
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
unzip = unzipUsingLists
unzip3 = unzip3UsingLists
unzipWith = unzipWithUsingLists
unzipWith3 = unzipWith3UsingLists
\end{code}
For the \lstinline$zip$s, this means that the longest deque determines the 
runtime if its front list is shorter than the shortest deque so that the 
\lstinline$reverse$ is forced. Assume that the length of the longer deque 
$n_2$ 
is more than three times that of the shorter deque $n_1$, as otherwise, 
$n_2\leq3n_1=\Oof{n_1}$. But then, for the front list to be shorter than the 
shorter deque, the potential of the longer deque has to be at least 
$(n_2-n_1)-n_1=n_2-2n_1$, where $n_2-n_1$ is the minimum length of the rear 
list 
and $n_1$ is the maximum length of the front list. As the potential of the 
resulting deque is at most 1, it is decreased by at least 
$n_2-2n_1=\Oof{n_2}$, 
so the amortised runtime is \Oof{\min(n_1, n_2)}. The same argument holds for 
the case of three deques. The amortised runtime complexity of the 
\lstinline$unzip$s is trivially \Oof{n}.
\hide{
\begin{code}
instance S.Sequence Seq where
  {empty = empty; single = single; cons = cons; snoc = snoc;
   append = append; lview = lview; lhead = lhead; ltail = ltail;
   rview = rview; rhead = rhead; rtail = rtail; null = null;
   size = size; concat = concat; reverse = reverse; 
   reverseOnto = reverseOnto; fromList = fromList; toList = toList;
   map = map; concatMap = concatMap; foldr = foldr; foldl = foldl;
   foldr1 = foldr1; foldl1 = foldl1; reducer = reducer; 
   reducel = reducel; reduce1 = reduce1; copy = copy; 
   tabulate = tabulate; inBounds = inBounds; lookup = lookup;
   lookupM = lookupM; lookupWithDefault = lookupWithDefault;
   update = update; adjust = adjust;
   mapWithIndex = mapWithIndex;
   foldrWithIndex = foldrWithIndex; foldlWithIndex = foldlWithIndex;
   take = take; drop = drop; splitAt = splitAt; subseq = subseq;
   filter = filter; partition = partition; takeWhile = takeWhile;
   dropWhile = dropWhile; splitWhile = splitWhile; zip = zip;
   zip3 = zip3; zipWith = zipWith; zipWith3 = zipWith3; unzip = unzip;
   unzip3 = unzip3; unzipWith = unzipWith; unzipWith3 = unzipWith3;
   instanceName s = moduleName}

instance Functor Seq where
  fmap = map

instance Monad Seq where
  return = single
  xs >>= k = concatMap k xs

instance MonadPlus Seq where
  mplus = append
  mzero = empty

instance Eq a => Eq (Seq a) where
  q1 == q2 = toList q1 == toList q2

instance Show a => Show (Seq a) where
  show q = show (toList q)
\end{code}
}

Generating an arbitrary \lstinline!SimpleDeque! can be performed by generating
arbitrary front and rear lists. Only if one of them is empty, special attention
is required not to violate the invariant. Specifically, the empty list is made
the rear list and the other list is shortened to at most one element. I.e. if
both lists are empty, the resulting deque is empty; if only one list is empty,
the resulting deque contains exactly one element; if both lists are non-empty
(but otherwise of arbitrary length), all their elements are preserved.  This
way, every possible \lstinline!SimpleDeque! can be generated: the empty case if
both generated lists are empty, the one-element case if exactly one list is
empty, or an arbitrary distribution of elements among front and rear list if
both lists are non-empty and the resulting queue therefore contains at least two
elements.
%The \lstinline!coarbitrary! operation simply calls \lstinline!coarbitrary! for
%both lists.
\begin{code}
instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do xs <- arbitrary
                 ys <- arbitrary
                 return (if L.null xs then D (L.take 1 ys) [] 
                         else if L.null ys then D (L.take 1 xs) []
                         else D xs ys)
\end{code}
\hide{
\begin{code}
  coarbitrary (D xs ys) = coarbitrary xs . coarbitrary ys
\end{code}
}

Generating a \lstinline!SimpleDeque! for a given list of elements distinguishes
two cases: If less than two elements (i.e. zero or one) are passed in, they are
all stored in the front list, which is directly enforced by the invariant.
Otherwise, the input list at split at a random position such that both parts 
are non-empty to acquire the front and rear list, respectively. Again it is easy
to see that any possible \lstinline!SimpleDeque! can be generated, as for the
non-trivial case with more than one element, the splitting is done at an
arbitrary position in the allowed range.
\begin{code}
instance CheckSeq.CheckableSequence Seq where
  arbitraryWith xs = let len = S.size xs
                     in if len < 2 then return (D xs [])
		        else do lenf <- choose (1, len-1)
			        let (f, r') = L.splitAt lenf xs
			        return (D f (L.reverse r')) 
\end{code}
\hide{
\begin{code}
  invariant = invariant

checkme = CheckSeq.checkseq (invariant :: Seq Int -> Bool)
\end{code}
}
