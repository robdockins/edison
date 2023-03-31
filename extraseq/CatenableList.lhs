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
module CatenableList (
    -- type of catenable list
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
import qualified BankersQueue as Q
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
unzipWith3     :: (a -> b) -> (a -> c) -> (a -> d) -> Seq a 
                  -> (Seq b, Seq c, Seq d)
moduleName = "CatenableList"
\end{code}
}
This design for a sequence that supports catenation in constant amortised time 
while also preserving constant time-bounds for the other basic list operations 
was proposed by Okasaki \cite{okasaki_focs95, okasaki_pfds98} as a simpler 
alternative to data structures as proposed by Kaplan and Tarjan 
\cite{kaplantarjan_stof95}, that would, however, achieve worst-case time-bounds.
The elements are stored in a multi-way tree, in pre-order left-to-right 
ordering. To store the children of a node, a queue, the \lstinline!BankersQueue! 
in this case, is used.
\begin{code}
data Seq a = E | C a (Q.Seq (Seq a))
\end{code}
The only structural invariant is that child-nodes can never be \lstinline!E!. 
(If such a case would occur, the respective node is removed from the queue of 
children.)
\begin{code}
invariant :: Seq a -> Bool
invariant E = True
invariant xs = noE xs
  where noE E = False
        noE (C _ q) = Q.foldr (&&) True (Q.map noE q)
\end{code}

Amortised runtime analysis is carried out using the banker's method where 
$d_t(i)$ denotes the number of debits on node $i$ in tree $t$ and 
$D_t(i)=\sum_{j=0}^id_t(i)$ the accumulated debits on nodes 0 through $i$.
$D_t$ is used as a short-hand for $D_t(|t|-1)$, i.e. all debits on tree $t$.

Two debit invariants are maintained. First, the number of debits on each
individual node must not exceed the node's degree. Note that the sum of all
degrees in a tree is one less than the number of nodes and therefore $D_t<|t|$.
Second, we require $D_t(i)\leq i+depth_t(i)$ and refer to this as left-linear
debit invariant. As $d_t(0)=D_t(0)\leq0+0=0$, this ensures that the root is free
of debits.

Using these invariants, Okasaki has shown constant amortised time for
\lstinline!append! and \lstinline!ltail!. We will repeat his arguments and
analyse the remaining operations.

As usual, the two basic constructors are trivial constant time implementations.
\begin{code}
empty = E
single x = C x Q.empty
\end{code}

Concatenating two \lstinline!CatenableList!s is easily performed by adding 
the second one as last child to the root of the first.
\begin{code}
append xs E = xs
append E ys = ys
append (C x q) ys = C x (Q.snoc q ys)
\end{code}
As \lstinline!snoc! runs in constant amortised time, we can discharge the debit 
it would create immediately, and by observing that no node's degree decreases, 
we only need to verify that the left-linear debit invariant is preserved.
For all nodes $i<|\lstinline!xs!|$, this is trivial because their index and 
depth remain the same and no new debits are created on them. For the nodes of 
the second list, their indexes are increased by $|\lstinline!xs!|$, their depth 
is increased by one and the accumulated debits are increased by the debits on 
\lstinline!xs!, so we get
\begin{displaymath}
\begin{array}{rcl}
D_t(|\lstinline!xs!|+i) &=& D_{\lstinline!xs!}+D_{\lstinline!ys!}(i)\\
&<&|\lstinline!xs!| + i + depth_{\lstinline!ys!}(i)\\
&=&|\lstinline!xs!| + i + depth_t(|\lstinline!xs!|+i)-1\\
&<&(|\lstinline!xs!| + i) + depth_t(|\lstinline!xs!|+i),
\end{array}
\end{displaymath}
where $t$ denotes the resulting tree. Thus the left-linear debit invariant is 
indeed preserved and we have established constant amortised runtime for 
\lstinline!append!.

Accessing the head element is easy and can obviously be performed in constant 
time as the root node is guaranteed to be free of debits. Removing it is a 
little more difficult, however. The solution is to \lstinline!append! all 
children of the root node together, as depicted in figure \ref{fig_catlisttail}.
\begin{figure}[htbp]
\begin{center}
\raisebox{1cm}{\includegraphics{CatenableList.1}}
\raisebox{2cm}{$\xrightarrow{\texttt{tail}}$}
\includegraphics{CatenableList.2}
\caption{\lstinline!tail! operation for \lstinline!CatenableList!}
\label{fig_catlisttail}
\end{center}
\end{figure}
As \lstinline!append! is commutative, the order in which this is performed does 
not influence the result. But to exploit lazy evaluation, it is most beneficial 
to do it right-associatively as the result will then be incremental when 
traversed from left to right.
\begin{code}
lview E = Nothing2
lview (C x q) = Just2 x (Q.foldr append empty q)

lhead E = error "CatenableList.lhead: empty sequence"
lhead (C x _) = x

ltail E = E
ltail (C _ q) = Q.foldr append empty q
\end{code}
This way, one additional debit is placed on all former children of the root node 
and their degrees are increased by one, with the exception of the right-most 
child, so we immediately discharge the debit placed on it. Again, we only need
to verify that the left-linear debit invariant is preserved. Now let 
\lstinline!xs' = tail xs! and the $i$th node of \lstinline!xs! reside in 
the $j$th subtree $t_j$. Now consider what happens to node $i$ when 
\lstinline!tail! is performed: its index is decreased to $i-1$, its depth in the
tree is increased by $j-1$ and the accumulated debits up to it increase by $j$.
Hence we get
\begin{displaymath}
\begin{array}{rcl}
D_{\lstinline!xs'!}(i-1) &=& D_{\lstinline!xs!}(i)+j\\
&\leq& depth_{\lstinline!xs!}(i)+i+j\\
&=& depth_{\lstinline!xs'!}(i-1)-(j-1)+i+j\\
&=& depth_{\lstinline!xs'!}(i-1)+(i-1)+2
\end{array}
\end{displaymath}
and it is sufficient to discharge $2=\Oof{1}$ more debits to restore the 
left-linear debit invariant.

To access or delete the right-most element, it is necessary to traverse the 
right spine. For the deletion, the construction of the resulting nodes becomes a 
little clumsy to avoid having empty leaves in the tree. If this would occur, 
instead the respective entry has to be removed from the queue of children.
\begin{code}
rview E = Nothing2
rview (C x q) 
  | Q.null q = Just2 E x
  | otherwise = let Just2 q' xs  = Q.rview q
		    Just2 xs' x' = rview xs
		in if null xs' then Just2 (C x q') x' 
		   else Just2 (C x (Q.snoc q' xs')) x'

rhead E = error "CatenableList.rhead: empty sequence"
rhead (C x q) = if Q.null q then x else rhead (Q.rhead q)

rtail E = E
rtail (C x q) 
  | Q.null q = E
  | otherwise = let Just2 q' xs = Q.rview q
		    xs'         = rtail xs
		in if null xs' then C x q' else C x (Q.snoc q' xs')
\end{code}
Accessing the right-most child of a node $i$ runs in \Oof{degree(i)}, so that 
traversing the right-spine takes up to $\sum_jdegree(j)=\Oof{|\lstinline!xs!|}$ 
(for nodes $j$ on the right spine) steps, in addition to the up to 
$|\lstinline!xs!|$ debits that have to be discharged, yielding linear amortised 
runtime.

Determining whether a list is empty is easily done in constant time by pattern 
matching on the root node.
\begin{code}
null E = True
null _ = False
\end{code}

Determining the size requires a complete traversal, resulting in linear runtime 
complexity.
\begin{code}
size E       = 0
size (C x q) = Q.foldr (+) 1 (Q.map size q)
\end{code}

The \lstinline!map! and \lstinline!fold! family of operations is implemented by 
using the respective queue operations to recurse down the tree. Only 
\lstinline!foldr1! is excepted from this as the right-most element cannot 
easily be determined.
\begin{code}
map _ E = E
map f (C x q) = C (f x) (Q.map (map f) q)

foldr _ e E       = e
foldr f e (C x q) = f x (Q.foldr (flip (foldr f)) e q)

foldl _ e E       = e
foldl f e (C x q) = (Q.foldl (foldl f) (f e x) q)

foldl1 _ E       = error "CatenableList.foldl1: empty sequence"
foldl1 f (C x q) = (Q.foldl (foldl f) x q)
\end{code}
As usual, the runtime is mainly determined by applying \lstinline!f! $n$ times.

Both \lstinline!cons! and \lstinline!snoc! simply \lstinline!append! a singleton 
list, therefore requiring constant amortised time.
\begin{code}
cons x s = append (single x) s
snoc = snocUsingAppend
\end{code}

Conversion from a list can be done by consecutive \lstinline!cons!, which will 
result in a unary tree, i.e. a list, where all but the root node can be 
suspended without violating the debit invariants, thereby yielding constant 
amortised time. The same holds for \lstinline!copy! and \lstinline!tabulate! 
(assuming constant runtime for the function passed to the latter).
\begin{code}
fromList = fromListUsingCons
copy = copyUsingLists
tabulate = tabulateUsingCons
\end{code}

Conversion to a list requires all debits to be discharged and the complete tree 
to be traversed, taking linear runtime. But it should be noted that the 
operation is incremental.
\begin{code}
toList = toListUsingFoldr
\end{code}

Concatenating several lists is very similar to what happens in 
\lstinline!ltail!.
\begin{code}
concat = concatUsingFoldr
\end{code}
There is, however, one major difference: The concatenated lists are not 
guaranteed to be non-empty, but the empty ones will not remain in the resulting 
list, which may cause the depth increase to be less than what was stated in the 
analysis of \lstinline!ltail!. In fact, if $e$ is the number of empty lists, 
then the depth is reduced by $e$, so that $e$ additional debits have to be 
discharged to reestablish the left-linear debit invariant, resulting in an 
amortised runtime complexity of \Oof{1+e}. 

For \lstinline!concatMap!, this argument could be adapted, but still, the 
cost for the application of the function argument has to be paid, resulting in 
an amortised runtime complexity that is linear in the length of the input list 
(for a constant time function argument), but independent of the length of the 
result.
\begin{code}
concatMap = concatMapUsingFoldr
\end{code}

Reversing a list obviously takes \Oof{n}, as yet alone accessing the last 
element does, and this cost cannot be left as debits, as it occurs on the first 
node of the result.
\begin{code}
reverse = reverseUsingReverseOnto
reverseOnto = reverseOntoUsingFoldl
\end{code}

As mentioned above, \lstinline!foldr1! cannot be easily implemented directly. 
Therefore, it uses the list representation internally, as do the 
\lstinline!reduce! operations.
\begin{code}
foldr1 = foldr1UsingLists
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
\end{code}

The different forms of sublist extraction all use the constant amortised time 
\lstinline!ltail!/\lstinline!lview! operations internally and therefore run in 
time linear to their arguments.
\begin{code}
take = takeUsingLview
drop = dropUsingLtail
splitAt = splitAtUsingLview
subseq = subseqDefault
\end{code}

The \lstinline!filter! and \lstinline!partition! operations use 
\lstinline!foldr! to traverse the list and \lstinline!cons! to build the 
appropriate result(s).
\begin{code}
filter = filterUsingFoldr
partition = partitionUsingFoldr
\end{code}

Likewise, the \lstinline!*While! family of operations uses \lstinline!lview! to
traverse the list as long as necessary.
\begin{code}
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
\end{code}

Checking whether a given index is inside the list or looking up the respective 
element is performed by \lstinline!drop!, thus running in \Oof{i}.
\begin{code}
inBounds = inBoundsUsingDrop
lookup = lookupUsingDrop
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingDrop
\end{code}

Changing an element at a given index uses \lstinline!splitAt! to reach and 
change the desired element (taking linear time) and then \lstinline!append!s
the two parts together again.
\begin{code}
update = updateUsingSplitAt
adjust = adjustUsingSplitAt
\end{code}

The remaining operations all use the list representation internally and therefore 
obviously run in \Oof{n}, as \lstinline!toList!, \lstinline!fromList! and the 
respective list operations do. For the \lstinline!zip! family, the shortest list
determines the runtime as \lstinline!toList! is incremental.
\begin{code}
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
unzip = unzipUsingLists
unzip3 = unzip3UsingLists
unzipWith = unzipWithUsingLists
unzipWith3 = unzipWith3UsingLists
\end{code}

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
  xs1 == xs2 = toList xs1 == toList xs2

instance Show a => Show (Seq a) where
  show xs = show (toList xs)
\end{code}
}

Generating an arbitrary multi-way tree becomes a little messy, so we just
generate an arbitrary list and then use \lstinline!arbitraryWith! to avoid
duplicating work. 
%The \lstinline!coarbitrary! function distinguishes the two possible variants
%and calls the respective \lstinline!coarbitrary! for both components in the
%non-empty case.
\begin{code}  
instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = arbitrary >>= CheckSeq.arbitraryWith
\end{code}
\hide{
\begin{code}  
  coarbitrary E       = variant 0
  coarbitrary (C x q) = variant 1 . coarbitrary x . coarbitrary q
\end{code}
}

Generating an arbitrary \lstinline!CatenableList! for a given list of elements
is a little involved. If the list is empty, the result trivially is the empty
\lstinline!CatenableList!, of course. Otherwise, the head of the list becomes
the element in the root, and the tail is partitioned into an arbitrary number 
of parts for which in turn arbitrary \lstinline!CatenableList!s are generated
which are stored in the queue of child nodes. The partitioning is performed by
recursing over the list from right to left, every time taking at least one and
at most all elements to form the right-most child-node. This will result in 
trees that are unbalanced towards the right on average. However, all forms of
trees are possible: Both the number of children and the number of elements per
child are completely arbitrary.
\begin{code}  
instance CheckSeq.CheckableSequence Seq where
  arbitraryWith [] = return empty
  arbitraryWith (x:xs) = liftM (C x) (q xs)
    where q [] = return Q.empty
          q xs = do n <- choose (0, L.size xs - 1)
	            let (xs', xs'') = L.splitAt n xs
                    ls <- q xs'
                    l <- CheckSeq.arbitraryWith xs''
                    return (Q.snoc ls l)
\end{code}
\hide{
\begin{code}  
  invariant = invariant

checkme = CheckSeq.checkseq (invariant :: Seq Int -> Bool)
\end{code}
}
