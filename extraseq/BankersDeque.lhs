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
module BankersDeque (
    -- type of banker's deque
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
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> Seq a -> (Seq b, Seq c, Seq d)

moduleName = "BankersDeque"
\end{code}
}
\label{sec:BankersDeque}
Similarly to how \lstinline$SimpleDeque$ could be based on 
\lstinline$SimpleQueue$ by adding symmetry, we can get a deque implementation 
suitable for persistent usage by adapting the \lstinline$BankersQueue$ to 
maintain a symmetric invariant \cite{okasaki_jfp95, okasaki_pfds98}.
The data type of the deque is equal to that of the queue, with a front and a 
rear list and their respective lengths.
\begin{code}
data Seq a = D !Int [a] [a] !Int
\end{code}
While for the \lstinline$BankersQueue$ the invariant was that the front list 
had to be at least as long as the rear, for the \lstinline$BankersDeque$, the 
length  have to be balanced in some sense; in particular, the maintained 
invariant is
\begin{displaymath}
|\lstinline!f!|\leq c|\lstinline!r!|+1
\wedge|\lstinline!r!|\leq 
c|\lstinline!f!|+1
\end{displaymath}
for some integer constant $c>1$. Note that this implies that for any deque 
containing at least two elements, both lists are non-empty. Specified in 
Haskell itself, the invariant looks as follows:
\begin{code}
invariant :: Seq a -> Bool
invariant (D lenf f r lenr) = lenf == L.size f
                              && lenr == L.size r
	                      && lenf <= c*lenr+1
		              && lenr <= c*lenf+1
\end{code}

Many operations will have amortised runtime that increases with $c$, so we 
choose
\begin{code}
c = 2::Int
\end{code}
Higher values of $c$ have the advantage that for sequences of accesses to same 
end of the deque, fewer rotations will be executed. However, in these 
situations, it may be assumed that one of the queue implementations may be 
more suitable anyway, so we use the lowest possible value for $c$.

The amortised runtime analysis, with which Okasaki shows contant runtime for
\lstinline!cons!, \lstinline!snoc!, \lstinline!ltail!, \lstinline!rtail! and
\lstinline!reverse!, uses the bankers method where for both lists, $d(i)$
denotes the number of debits on the $i$th element and 
$D(i)=\sum_{j=0}^i$ is the total debit for the first $i+1$ elements. The 
maintained debit invariant is
\begin{displaymath}
D(i)\leq\min\left(i(c+1), cs+1-t\right),\quad s=\min(|f|,|r|),
\quad t=\max(|f|,|r|).
\end{displaymath}
Note that this enforces $D(i)=0$, thus also $d(i)=0$, i.e. the list heads are 
free of debits and can be accessed at will.

We start by defining a helper function (slightly adapted from the 
\lstinline!check! function in \cite{okasaki_pfds98}) that will (re)establish 
the structural invariant by truncating the longer list to half the combined 
length of both list and reversing the rest onto the back of the shorter list.
\begin{code}
makeD lenf f r lenr
  | lenf>c*lenr+1 = let i = (lenf+lenr) `div` 2
                        j = lenf+lenr-i
                    in D i (L.take i f) (r ++ L.reverse (L.drop i f)) j
  | lenr>c*lenf+1 = let j = (lenf+lenr) `div` 2
                        i = lenf+lenr-j
                    in D i (f ++ L.reverse (L.drop j r)) (L.take j r) j
  | otherwise     = D lenf f r lenr
\end{code}
  
The two basic constructors both trivially maintain both the structural and the 
debit invariant and run in constant time.
\begin{code}
empty = D 0 [] [] 0
single x = D 1 [x] [] 0
\end{code}

Using the helper function, \lstinline$cons$ and \lstinline$snoc$ are equally 
easy to implement, as found in \cite{okasaki_pfds98}.
\begin{code}
cons x (D lenf f r lenr) = makeD (lenf+1) (x:f) r lenr
snoc (D lenf f r lenr) y = makeD lenf f (y:r) (lenr+1)
\end{code}
For the runtime analysis of \lstinline$cons$, we have to distinguish whether 
the call of \lstinline$makeD$ causes a rotation or not. If not, 
$|\lstinline!f!|$ is increased by one, thereby violating the invariant for all 
nodes where previously 
$D(i)=c\min(|\lstinline!f!|,|\lstinline!r!|)+1-
      \max(|\lstinline!f!|,|\lstinline!r!|)$,
if $|\lstinline!f!|>|\lstinline!r!|$. To reestablish the invariant, it is 
obviously sufficient to discharge one debit per list, say the first one. In case 
a rotation is forced, we know that before the \lstinline$cons$, 
$|\lstinline!f!|=c|\lstinline!r!|+1$ and so 
$c\min(|\lstinline!f!|,|\lstinline!r!|)+1-\max(|\lstinline!f!|,|\lstinline!r!|) 
= c|\lstinline!r!|+1-|\lstinline!f!| = 0$, i.e. all debits had been discharged.
After the rotation, there is one debit on every element in the front list and 
one debit on the first $|\lstinline!r!|$\footnote{\lstinline$f$ and 
\lstinline$r$ still refer to the lists before the \lstinline$cons$ while 
\lstinline$f'$ and \lstinline$r'$ denote the lists after the rotation.} elements 
on the rear list (as \lstinline$take$ and \lstinline$append$ for lists are 
incremental), plus 
$|\lstinline!f!|+1=c|\lstinline!r!|+2$ on the 
$|\lstinline!r!|$th node of the rear list. So the resulting 
debits are
\begin{displaymath}
D_{\lstinline!f!'}(i)=i+1\qquad 
D_{\lstinline!r!'}(i)=\left\{\
\begin{array}{ll}
i+1 & \textrm{for } i<|\lstinline!r!| \\
(c+1)|\lstinline!r!|+2& \textrm{for } i\geq |\lstinline!r!|.
\end{array}
\right.
\end{displaymath}
The debit invariant can therefore be reestablished by discharging the one debit 
on the head elements of both lists and discharging one extra debit on the rear 
list, giving constant amortised runtime. 

By symmetry, the same holds for \lstinline$snoc$.

Thanks to the explicitly stored sizes, we can perform \lstinline$append$ in 
time linear to the shorter of the two deques.
\begin{code}
append (D lenf1 f1 r1 lenr1) (D lenf2 f2 r2 lenr2)
    | lenf1+lenr1 <= lenf2+lenr2 = 
           makeD (lenf1 + lenr1 + lenf2) (f1 ++ L.reverseOnto r1 f2) r2 lenr2
    | otherwise = 
           makeD lenf1 f1 (r2 ++ L.reverseOnto f2 r1) (lenr2 + lenf2 + lenr1)
\end{code}
For the analysis, we will assume that 
$|\lstinline!f1!|+|\lstinline!r1!|=n_1\leq 
n_2=|\lstinline!f2!|+|\lstinline!r2!|$ and establish that it has amortised 
runtime complexity \Oof{n_1}, then, by symmetry, \lstinline$append$ will 
obviously run in \Oof{\min(n_1,n_2)} amortised.

First, we discharge all debits on \lstinline$f1$ and \lstinline$r1$, which are 
bounded by \Oof{n_1}, and furthermore pay the costs for the \lstinline$++$ and 
\lstinline$reverseOnto$, which are together also \Oof{n_1}. What remains to do 
is similar to the analysis of \lstinline$cons$, again we distinguish whether a 
rotation is performed or not. If not, like with \lstinline$cons$, it is 
sufficient to discharge $n_1$ debits on both resulting lists. If a rotation is 
caused, we know that $|\lstinline!f2!|+x=c|\lstinline!r2!|+1$ for some 
$x<n_1$, so the debits on \lstinline$f2$ and \lstinline$r2$ are bounded by
\begin{displaymath}
\begin{array}{rcl}
c\min(|\lstinline!f2!|,|\lstinline!r2!|)+1-
\max(|\lstinline!f2!|,|\lstinline!r2!|) 
&\leq &c|\lstinline!r2!|+1-|\lstinline!f2!|\\
&=&c|\lstinline!r2!|+1-(c|\lstinline!r2!|+1-x)\\
&=&x<n_1,
\end{array}
\end{displaymath}
and so can be discharged in \Oof{n_1}. With an argument similar to the one for 
\lstinline$cons$, we can establish that the debits after the rotation are
\begin{displaymath}
D_{\lstinline!f!'}(i)=i+1\qquad 
D_{\lstinline!r!'}(i)=\left\{\
\begin{array}{ll}
i+1 & \textrm{for } i<|\lstinline!r!| \\
(c+1)|\lstinline!r!|-x+1+n_1& \textrm{for } i\geq |\lstinline!r!|.
\end{array}
\right.
\end{displaymath}
So by discharging one debit on the front list and $n_1+1-x=\Oof{n_1}$ debits 
on the rear list the debit invariant can be reestablished, giving a total 
amortised runtime complexity of \Oof{n_1}.

Accessing the first (last) element is straight-forward and requires 
distinction of three cases: the deque may be empty, may contain one element in 
the rear list (front list) or has a non-empty front list (rear list). Note:
The Haskell implementations given in \cite{okasaki_pfds98} are incorrect as 
they are missing the second case; the Standard ML ones are correct, though.
\begin{code}
lview (D _ [] [] _) = Nothing2
lview (D _ [] [y] _) = Just2 y empty
lview (D lenf (x:f) r lenr) = Just2 x (makeD (lenf-1) f r lenr)

lhead (D _ [] [] _) = error "BankersDeque.lhead: empty sequence"
lhead (D _ [] [y] _) = y
lhead (D _ (x:_) _ _) = x

ltail (D lenf (_:f) r lenr) = makeD (lenf-1) f r lenr
ltail _ = empty                   -- zero or one element

rview (D _ [] [] _) = Nothing2
rview (D _ [x] [] _) = Just2 empty x
rview (D lenf f (y:r) lenr) = Just2 (makeD lenf f r (lenr-1)) y

rhead (D _ [] [] _) = error "BankersDeque.rhead: empty sequence"
rhead (D _ [x] [] _) = x
rhead (D _ _ (y:_) _) = y

rtail (D lenf f (_:r) lenr) = makeD lenf f r (lenr-1)
rtail _ = empty                   -- zero or one element
\end{code}
Thanks to the symmetry, we can restrict our analysis again to the 
\lstinline$lview$-family and the same results will also hold for the 
\lstinline$rview$-family. As the head element of the lists are guaranteed to 
be free of debits, \lstinline$lhead$ clearly runs in constant time, as do 
\lstinline$lview$/\lstinline$ltail$ in case the returned deque is empty. If 
the returned deque is non-empty, we have to distinguish again whether a 
rotation is required or not. If not, the debit invariant in the front list may 
be violated due to the resulting index shift. This can be countered by 
discharging the first $c+1$ debits in the list. Furthermore, 
$\min(|\lstinline!f!|,|\lstinline!r!|)$ may have been reduced by one, 
making it necessary to discharge $c$ debits on the 
rear list. In case a rotation is required, before the 
\lstinline$lview$/\lstinline$ltail$ it must have been the case that 
$c|\lstinline!f!|+1=|\lstinline!r!|$ and all debits had been discharged.
After the rotation, there is one debit on every element of the rear list,
one debit on the first $|\lstinline!f!|-1$ elements of the front list and 
$|\lstinline!r!|=c|\lstinline!f!|+1$ debits on the $(|\lstinline!f!|-1)$th 
element, giving the debits
\begin{displaymath}
D_{\lstinline!r!'}(i)=i+1\qquad 
D_{\lstinline!f!'}(i)=\left\{\
\begin{array}{ll}
i+1 & \textrm{for } i<|\lstinline!f!|-1 \\
(c+1)|\lstinline!f!|+1& \textrm{for } i\geq |\lstinline!f!|-1.
\end{array}
\right.
\end{displaymath}
The debit invariant can be reestablished by discharging the debit on the head 
of the rear list and discharging the first $c+2$ debits on the front list. 
Thus, the amortised runtime complexity is \Oof{1}. 

Determining the size of a deque is simple due to the explicit size fields and 
obviously runs in \Oof{1}.
\begin{code}
null (D lenf _ _ lenr) = (lenf == 0) && (lenr == 0)
size (D lenf _ _ lenr) = lenf + lenr
\end{code}

Reversing a deque is equally simple, as both the structural and the debit 
invariant are symmetric and are thus preserved when front and rear list are 
exchanged, making this operation run in constant time.
\begin{code}
reverse (D lenf f r lenr) = D lenr r f lenf
\end{code}

Conversion from a list can be performed by a simple call to the helper 
function after first determining the length of the list.
\begin{code}
fromList xs = makeD (length xs) xs [] 0
\end{code}
Determining the length takes $|\lstinline!xs!|$ steps, and after the rotation 
that will be necessary for $|\lstinline!xs!|>1$, there are one debit on the 
first element in the front list and $|\lstinline!xs!|$ debits on the head of 
the rear list, which have to be discharged, resulting in an amortised runtime 
of \Oof{|\lstinline!xs!|}.

Conversion to a list has to discharge all \Oof{n} debits on the deque and then 
append the reversed rear to the front list, requiring additional $n$ steps, 
giving a total amortised runtime complexity of \Oof{n}.
\begin{code}
toList (D _ f [] _) = f
toList (D _ f r  _) = f ++ L.reverse r
\end{code}

Like for the \lstinline$SimpleDeque$, the \lstinline$map$ and 
\lstinline$fold$/\lstinline$reduce$-family of operations are simply performed 
by calling the appropriate list functions on the front and rear list. The 
required runtime is mainly determined by applying the function argument 
\lstinline$f$ $n$ times.
\begin{code}
map f (D i xs ys j) = D i (L.map f xs) (L.map f ys) j

foldr f e (D _ xs ys _) = L.foldr f (L.foldl (flip f) e ys) xs

foldl f e (D _ xs ys _) = L.foldr (flip f) (L.foldl f e xs) ys

foldr1 _ (D _ []  [] _) = error "BankersDeque.foldr1: empty sequence"
foldr1 _ (D _ [x] [] _) = x
foldr1 f (D _ xs  ys _) = L.foldr f (L.foldl1 (flip f) ys) xs

foldl1 _ (D _ [] []  _) = error "BankersDeque.foldl1: empty sequence"
foldl1 _ (D _ [] [y] _) = y
foldl1 f (D _ xs ys  _) = L.foldr (flip f) (L.foldl1 f xs) ys

reducer f e (D _ xs ys _) = L.reducer f (L.reducel (flip f) e ys) xs

reducel f e (D _ xs ys _) = L.reducer (flip f) (L.reducel f e xs) ys

reduce1 _ (D _ [] []  _) = error "BankersDeque.reduce1: empty sequence"
reduce1 _ (D _ [] [y] _) = y
reduce1 f (D _ xs ys  _) = L.reducer (flip f) (L.reduce1 f xs) ys

mapWithIndex f (D i xs ys j) = 
    D i (L.mapWithIndex f xs) (L.mapWithIndex f' ys) j  
    where f' idx x = f (i+j-idx-1) x

foldrWithIndex f e (D i xs ys j) = 
    L.foldrWithIndex f (L.foldlWithIndex f' e ys) xs
    where f' y idx x = f (i+j-idx-1) x y

foldlWithIndex f e (D i xs ys j) =
    L.foldrWithIndex f' (L.foldlWithIndex f e xs) ys
    where f' idx x y = f y (i+j-idx-1) x

\end{code}

Similarly, the \lstinline$copy$ and \lstinline$tabulate$ constructors can be
implemented by constructing the two lists appropriately.
\begin{code}
copy n x 
  | n < 0     = empty
  | otherwise = let lenf = n `div` 2
                    lenr = n - lenf
                in D lenf (L.copy lenf x) (L.copy lenr x) lenr
tabulate n f 
  | n < 0     = empty
  | otherwise = let lenf = n `div` 2
                    lenr = n - lenf
                in D lenf (L.map f [0..lenf-1]) (L.map f [n-1,n-2..lenf]) lenr
\end{code}
As for both operations the lists can be build incrementally, one debit will be 
placed on every element (assuming for \lstinline$tabulate$ that \lstinline$f$ 
runs in constant time), so that the debit invariant can be established by 
discharging only one debit per list (on the head element), curiously yielding 
constant amortised runtime.

Like \lstinline$map$, the \lstinline$filter$ and \lstinline$partition$ 
operations can be carried out directly by using the respective list operations.
But as this my cause the structural invariant to be violated, a call to
\lstinline$makeD$ is required. The amortised runtime is obviously \Oof{n}, 
assuming that \lstinline!p! runs in constant time.
\begin{code}
filter p (D lenf f r lenr) = 
    let f' = L.filter p f
        r' = L.filter p r
    in makeD (L.size f') f' r' (L.size r')

partition p (D lenf f r lenr)
  = (makeD (L.size fT) fT rT (L.size rT), makeD (L.size fF) fF rF (L.size rF))
 where
   (fT,fF) = L.partition p f
   (rT,rF) = L.partition p r
\end{code}

Looking up an element at a certain index is carried out by directly looking up 
the element in the respective list, which can be determined using the explicit 
size fields.
\begin{code}
lookup (D lenf f r lenr) i
  | i < lenf  = L.lookup f i
  | otherwise = L.lookup r (lenr - (i - lenf) - 1)

lookupM (D lenf f r lenr) i
  | i < lenf  = L.lookupM f i
  | otherwise = L.lookupM r (lenr - (i - lenf) - 1)

lookupWithDefault d (D lenf f r lenr) i
  | i < lenf  = L.lookupWithDefault d f i
  | otherwise = L.lookupWithDefault d r (lenr - (i - lenf) - 1)
\end{code}
If the index is in the front list, the debits on the first $i$ elements 
have to be discharged, which are bounded by \Oof{i}, and the 
\lstinline$lookup$ in the list also takes \Oof{i} time. If the index lies in 
the rear list, the worst case occurs for $i=|\lstinline!f!|$, which results in 
looking up the last element in the rear list, which makes it necessary to 
discharge all debits plus requires $|\lstinline!r!|$ time. But as 
$|\lstinline!r!|\leq c|\lstinline!f!|+1=ci+1=\Oof{i}$, we can establish 
amortised time complexity \Oof{i} for both cases.

The same holds for \lstinline$update$ and \lstinline$adjust$ (plus the time to 
execute \lstinline$g$ for \lstinline$adjust$). Actually, the cost for the 
list-\lstinline$update$/\lstinline$adjust$ could be assigned to new debits, 
but this would not change the asymptotic time complexity, as the old \Oof{i} 
debits would have to be discharged before, anyway.
\begin{code}
update i e d@(D lenf f r lenr)
  | i < lenf  = if i < 0 then d
                else D lenf (L.update i e f) r lenr
  | otherwise = let k' = lenr - (i - lenf) - 1
                in if k' < 0 then d
                   else D lenf f (L.update k' e r) lenr
adjust g i d@(D lenf f r lenr)
  | i < lenf  = if i < 0 then d
                else D lenf (L.adjust g i f) r lenr
  | otherwise = let k' = lenr - (i - lenf) - 1
                in if k' < 0 then d
                   else D lenf f (L.adjust g k' r) lenr
\end{code}

The \lstinline$take$ operation also distinguishes whether the cut-off index is 
in the front or the rear list and then calls the appropriate list function.
\begin{code}
take len d@(D lenf f r lenr)
  | len <= 0 = empty
  | len <= lenf = makeD len (L.take len f) [] 0
  | len <= lenf+lenr = let len' = len - lenf
                       in makeD lenf f (L.drop (lenr - len') r) len'
  | otherwise = d
\end{code}
We analyse the two cases separately. If $\lstinline!len!\leq|\lstinline!f!|$, 
both the list-\lstinline$take$ and the caused rotation obviously run in 
\Oof{\lstinline!len!}. Otherwise, as argued for the 
\lstinline$lookup$-functions, the list-\lstinline$drop$ also runs in 
\Oof{\lstinline!len!}, leaving the cost for \lstinline$makeD$ to be
analysed. If no rotation is forced, all debits on the rear list have already 
been discharged for the \lstinline$drop$ and its cost has been paid, leaving 
the front list, from which 
$c(\lstinline!len!-|\lstinline!f!|)=\Oof{\lstinline!len!}$ must be discharged 
to maintain the debit invariant, as $\min(\lstinline!f!,\lstinline!r!)$ may be 
reduced by up to $(\lstinline!len!-|\lstinline!f!|)$. If a rotation is 
performed, we know that for the original deque it is the case that 
$c(|\lstinline!r!|-x)+1=|\lstinline!f!|$ for some 
$x<\lstinline!len!-|\lstinline!f!|$ (not necessarily integer). Thus, the 
debits present on the front list before the rotation are bounded by 
$c|\lstinline!r!|+1-|\lstinline!f!|
=c|\lstinline!r!|+1-(c(|\lstinline!r!|-x)+1)
=cx<c(\lstinline!len!-|\lstinline!f!|)=\Oof{\lstinline!len!}$ 
and can be discharged. After the rotation, the debits on the two lists are
\begin{displaymath}
D_{\lstinline!f!'}(i)=i+1\qquad 
D_{\lstinline!r!'}(i)=\left\{\
\begin{array}{ll}
i+1 & \textrm{for } i<\lstinline!len!-|\lstinline!f!| \\
\lstinline!len! & \textrm{for } i\geq \lstinline!len!-|\lstinline!f!|.
\end{array}
\right.
\end{displaymath}
So, furthermore discharging the two debits on the list heads and additional  
$\lstinline!len!$ debits on the rear list reestablish the debit invariant, 
yielding \Oof{\lstinline!len!} amortised runtime complexity.

By symmetry, we can then easily establish \Oof{n-\lstinline!len!} runtime for 
\lstinline$drop$.
\begin{code}
drop len d@(D lenf f r lenr)
  | len <= 0 = d
  | len <= lenf = makeD (lenf - len) (L.drop len f) r lenr
  | len <= lenf+lenr = let len' = len - lenf 
                       in  makeD 0 [] (L.take (lenr - len') r) (lenr - len') 
  | otherwise = empty
\end{code}
We want to establish runtime \Oof{\lstinline!len!}, however. For the case of 
the index \lstinline!len! being in the rear list, this is easy, as again 
$\Oof{n}=\Oof{\lstinline!len!}$ is easily shown to be an upper bound. In the 
other case, if no rotation is forced, the debits of the first \lstinline!len! 
elements in \lstinline!f! have to be discharged and the list-\lstinline!drop! 
also runs in \Oof{\lstinline!len!}, and finally, to reestablish the debit 
invariant, it is necessary to discharge $(c+1)\cdot\lstinline!len!$ debits on 
the front list (due to the index shift) and $c\cdot\lstinline!len!$ debits on 
the rear list, all of which establishes the amortised runtime of 
\Oof{\lstinline!len!}. The argument for the case of a rotation being performed 
is similar to the ones given above, observing that before the 
\lstinline!drop!, $c(|\lstinline!f!|-x)+1-|\lstinline!r!|$ for some 
$x<\lstinline!len!$ and will not be repeated here.

Note that this establishes an amortised runtime complexity of 
\Oof{\min(\lstinline!len!,n-\lstinline!len!)} not only for \lstinline!drop!, 
but by symmetry also for \lstinline!take!. Then, of course, 
\lstinline!splitAt! can also be implemented in 
\Oof{\min(\lstinline!len!,n-\lstinline!len!)}, omitting the formal argument.
\begin{code}
splitAt i d@(D lenf f r lenr)
  | i <= 0 = (empty, d)
  | i <= lenf = let (f',f'') = L.splitAt i f
                in (makeD i f' [] 0, makeD (lenf - i) f'' r lenr)
  | i <= lenf+lenr = let i' = i - lenf
                         (r', r'') = L.splitAt (lenr - i') r
                     in (makeD lenf f r'' i', makeD 0 [] r' (lenr - i'))
  | otherwise = (d, empty)
\end{code}

The remaining functions are all implemented using defaults.

concatenating multiple deques is performed by right-associatively 
\lstinline$append$ing them using \lstinline$foldr$. 
\begin{code}
concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
\end{code}
The \lstinline$foldr$ runs in \Oof{n} plus the time needed for the 
\lstinline$append$s, which is \Oof{m} with $m$ being the length of the 
resulting deque, plus the time needed for the \lstinline$map$ for 
\lstinline$concatMap$, giving a total runtime of \Oof{n+m}.

As \lstinline!reverse! runs in \Oof{1}, \lstinline!reverseOnto! can be reduced 
to a \lstinline!reverse! and an \lstinline!append!, running in 
\Oof{\min(n_1,n_2)} like \lstinline!append!.
\begin{code}
reverseOnto = reverseOntoUsingReverse
\end{code}

As the size is explicitly stored, we can perform \lstinline!inBounds! in 
constant time.
\begin{code}
inBounds = inBoundsUsingSize
\end{code}

Using \lstinline$drop$ and \lstinline$take$, it is easy to implement 
\lstinline$subseq$. The total amortised time complexity is that of 
\lstinline$drop$ plus that of \lstinline$take$, i.e. \Oof{i+j} (for 
\lstinline$subseq i j$).
\begin{code}
subseq = subseqDefault
\end{code}

The *\lstinline$While$ operations traverse the deque by repeated application of
\lstinline$lview$. Per accepted element, this takes constant amortised time 
for \lstinline$lview$, and \lstinline$cons$ if applicable, plus the time 
required for evaluation of the predicate, where at most $n$ elements can be 
accepted.
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
Like for the \lstinline$SimpleDeque$, the \lstinline$reverse$ of the longer 
deque for a \lstinline$zip$ is only forced if its front list is shorter the 
shorter deque, which bounds the length of the longer deque to 
$(c+1)n_1=\Oof{n_1}$. But up to the end of the front list, \lstinline$toList$ 
is incremental, so that again the amortised runtime complexity of 
the \lstinline$zip$s is \Oof{\min(n_1, n_2)}.
\hide{
\begin{code}
-- instances

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
   update = update; adjust = adjust; mapWithIndex = mapWithIndex;
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
\end{code}
}

Equality testing can be done a little more clever than usual by first 
comparing the sizes of the two involved deques, which takes constant time. The 
worst-case time is still linear in the length, of course.
\begin{code}
instance Eq a => Eq (Seq a) where
  q1 == q2 =
    (size q1 == size q2) && (toList q1 == toList q2)
\end{code}

\hide{
\begin{code}
instance Show a => Show (Seq a) where
  show q = show (toList q)
\end{code}
}

To generate an arbitrary \lstinline!BankersDeque!, it is sufficient to 
generate arbitrary front and rear lists and then reduce the length of the 
longer to establish the invariant, if necessary. As the lists are of arbitrary
length and accepted as they are if they fulfill the invariant, obviously any
allowed \lstinline!BankersDeque! can be generated this way.
%The \lstinline!coarbitrary!  function can be implemented conveniently using
%the list-\lstinline!coarbitrary! for front and rear list.
\begin{code}
instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary =
    do f <- arbitrary
       r <- arbitrary
       return (let lenf  = L.size f
                   lenr  = L.size r
		   lenf' = min lenf (c*lenr+1)
		   lenr' = min lenr (c*lenf+1)
               in D lenf' (L.take lenf' f) (L.take lenr' r) lenr')
\end{code}
\hide{
\begin{code}
  coarbitrary (D _ f r _) = coarbitrary f . coarbitrary r
\end{code}
}

Generating an arbitrary \lstinline!BankersDeque! with given elements makes it 
necessary to split the input list at a position such that the two parts fulfil 
the invariant. With $|\lstinline!xs!|=|\lstinline!f!|+|\lstinline!r!|$ we get 
from the invariant that
\begin{displaymath}
\begin{array}{rcl}
|\lstinline!r!| & \leq & c|\lstinline!f!|+1 \\ 
|\lstinline!xs!|-|\lstinline!f!| & \leq & c|\lstinline!f!|+1 \\
|\lstinline!xs!|-1 & \leq & (c+1)|\lstinline!f!| \\
\left\lceil\frac{|\lstinline!xs!|-1}{c+1}\right\rceil
=\left\lfloor\frac{|\lstinline!xs!|+c-1}{c+1}\right\rfloor
& \leq & |\lstinline!f!|
\end{array}
\end{displaymath}
and
\begin{displaymath}
\begin{array}{rcl}
|\lstinline!f!| & \leq & c|\lstinline!r!|+1 \\ 
|\lstinline!f!| & \leq & c(|\lstinline!xs!|-|\lstinline!f!|)+1 \\ 
(c+1)|\lstinline!f!| & \leq & c|\lstinline!xs!|+1 \\ 
|\lstinline!f!| & \leq & 
\left\lfloor\frac{c|\lstinline!xs!|+1}{c+1}\right\rfloor.
\end{array}
\end{displaymath}
So we pick a random value for $|\lstinline!f!|$ in this range and select front 
and rear list accordingly. As $|\lstinline!f!|$ may be anywhere in the allowed
range, any allowed \lstinline!BankersDeque! of the desired length with the given
elements can be generated.
\begin{code}
instance CheckSeq.CheckableSequence Seq where
  arbitraryWith xs = do let len = S.size xs
                        lenf <- choose ((len+c-1) `div` (c+1), 
			                (c*len + 1) `div` (c+1))
			let lenr = len-lenf
			let (f, r') = S.splitAt lenf xs
			return (D lenf f (S.reverse r') lenr)
\end{code}
\hide{
\begin{code}
  invariant = invariant

checkme = CheckSeq.checkseq (invariant :: Seq Int -> Bool)
\end{code}
}
