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
module CatenableDeque (
    -- type of catenable deque
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
import qualified BankersDeque as D
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
moduleName = "CatenableDeque"
\end{code}
}

The catenable deques (supporting the standard deque operations and 
\lstinline!append! in constant amortised time) as described in 
\cite{okasaki_icfp97, okasaki_pfds98} are by far the most complicated sequence 
data structure implemented in the course of this project. It requires an 
implementation of normal deques (i.e. with linear time \lstinline!append!), 
for which we will use \lstinline!BankersDeque! as it is at present the only 
deque implementation that also features persistence. Furthermore, its 
\lstinline!append! runs in time linear to the length of the shorter of the two 
arguments, a property that comes in handy later on.

A catenable deque (c-deque) can have to forms, either \lstinline!Shallow! or
\lstinline!Deep!.
\begin{code}
data Seq a = Shallow !(D.Seq a)
           | Deep !(D.Seq a) 
	          (Seq (CmpdElem a)) 
		  !(D.Seq a) 
		  (Seq (CmpdElem a)) 
                  !(D.Seq a)
\end{code}
In its \lstinline!Shallow! form, it merely is a wrapper for the normal deque. 
The more interesting \lstinline!Deep! form represents a quintuple 
$(f, a, m, b, r)$, where $f$, $m$ and $r$ are normal deques again and $a$ and 
$b$ are c-deques of compound elements. $f$ and $r$ must always contain at 
least three elements, $m$ at least two; should the length of a c-deque in 
\lstinline!Deep! form be reduced to below eight, it will be converted back to 
\lstinline!Shallow! form.

The compound elements also come in two flavours, \lstinline!Simple! or 
\lstinline!Cmpd!.
\begin{code}
data CmpdElem a = Simple !(D.Seq a)
                | Cmpd !(D.Seq a) (Seq (CmpdElem a)) !(D.Seq a) deriving Show
\end{code}
The \lstinline!Simple! compound element is again just a wrapper for a normal 
deque, which is this time required to contain at least two elements, while the 
\lstinline!Cmpd! form represents a triple $(f, c, r)$, where $f$ and $r$ are 
normal deques, again with at least two elements, and $c$ is a c-deque of 
compound elements.

\begin{code}
invariant :: Seq a -> Bool
invariant (Shallow _) = True
invariant (Deep f a m b r) = D.size f >= 3
                             && invariant a
	                     && foldr (\x e -> cmpdInvariant x && e) True a
                             && D.size m >= 2
	                     && invariant b
	                     && foldr (\x e -> cmpdInvariant x && e) True b
	      	             && D.size r >= 3

cmpdInvariant :: CmpdElem a -> Bool
cmpdInvariant (Simple d) = D.size d >= 2
cmpdInvariant (Cmpd f c r) = D.size f >= 2 
                             && invariant c 
			     && foldr (\x e -> cmpdInvariant x && e) True c
			     && D.size r >=2
\end{code}

Note that the strictness flags used for all occurrences of normal deques of 
course do not interfere with the laziness used inside them. The strictness 
only avoids the overhead that would occur for the creation of the outmost 
suspensions, which would not yield any gain, as all operations suspended in 
this way run in \Oof{1} amortised time anyway.

The amortised run-time analysis will again be carried out using the banker's 
method. As only the $a$ and $b$ fields of the \lstinline!Deep! c-deque and the 
$c$ field of the \lstinline!Cmpd! can carry suspensions, debits are only 
placed on these. The maintained debit invariant for the \lstinline!Cmpd! is 
that at most four debits may be placed on the $c$ field of \lstinline!Cmpd! 
and between zero and five on the $a$ and $b$ fields, as defined by the 
following rules:
\begin{itemize}
\item $a$ and $b$ have a base allowance of zero debits.
\item If $f$ contains more than three elements, then the allowance for $a$ is 
  increased by four debits and that of $b$ by one.
\item Likewise, if $r$ contains more than three elements, then the allowance 
  for $b$ is increased by four debits and that of $a$ by one.
\end{itemize}
For operations inside the \lstinline!Shallow! form, no additional analysis 
will be necessary as they are directly mapped to the respective operations of 
the normal deque.

Creating an empty or singleton c-deque simply creates a \lstinline!Shallow! 
c-deque carrying the appropriate deque (\lstinline!empty! is directly taken 
from \cite{okasaki_pfds98}). The run-time is obviously constant.
\begin{code}
empty = Shallow D.empty
single x = Shallow (D.single x)
\end{code}

Adding a single element, either to the front or rear end, just has to add the 
element to the respective internal deque. (Both operations are described in 
\cite{okasaki_icfp97}, and \cite{okasaki_pfds98} contains Haskell code for
\lstinline!cons!.)
\begin{code}
cons x (Shallow d) = Shallow (D.cons x d)
cons x (Deep f a m b r) = Deep (D.cons x f) a m b r

snoc (Shallow d) x = Shallow (D.snoc d x)
snoc (Deep f a m b r) x = Deep f a m b (D.snoc r x)
\end{code}
As both \lstinline!cons! and \lstinline!snoc! for the \lstinline!BankersDeque! 
run in constant amortised time and obviously the debit invariant is preserved, 
both operations run in \Oof{1} amortised.

The \lstinline!append! operation has to distinguish the four major cases 
whether both c-deques are \lstinline!Shallow!, the first or the second is 
\lstinline!Deep!, or both are \lstinline!Deep!, where of course the second and 
third case are symmetric. If both c-deques are \lstinline!Shallow! and at 
least one of them contains less than four elements, we can call the 
\lstinline!append! operation of the \lstinline!BankersDeque! directly and 
create a new \lstinline!Shallow! c-deque. If they both contain four or more 
elements, a \lstinline!Deep! c-deque is built with the $a$ and $b$ components 
empty, the $m$ component containing the last element of the first c-deque and 
the first element of the of the second c-deque, and the $f$ and $r$ components 
set to the remainders of the two internal deques, respectively. (See figure 
\ref{fig_catdequeshare}.)
\begin{figure}[htbp]
\begin{center}
\includegraphics{CatenableDeque.1}
\caption{\lstinline!share! helper function for \lstinline!CatenableDeque!}
\label{fig_catdequeshare}
\end{center}
\end{figure}
This way of dividing two deques into three occurs several times, so we 
introduce a helper function \lstinline!share! to take care of this. (This 
version is adapted from the one given in \cite{okasaki_pfds98} and modified to 
make use of the \lstinline!view! operations. Likewise, the \lstinline!append! 
implementations are modified to exploit that the \lstinline!append! of the 
\lstinline!BankersDeque! runs in time linear to the length of its shorter 
argument, making it unnecessary to provide two different, specialised 
\lstinline!append!s.)
\begin{code}
share :: D.Seq a -> D.Seq a -> (D.Seq a, D.Seq a, D.Seq a)
share f r = let Just2 fi fl = D.rview f
                Just2 rh rt = D.lview r
	    in (fi, D.cons fl (D.single rh), rt)

append (Shallow d1) (Shallow d2) 
  | (D.size d1<4) || (D.size d2<4) = Shallow (D.append d1 d2)
  | otherwise = Deep f empty m empty r 
                where (f, m, r) = share d1 d2             
\end{code}
Appending a \lstinline!Shallow! and a \lstinline!Deep! c-deque again 
distinguishes whether the \lstinline!Shallow! c-deque has less than four 
elements or not. If so, its internal deque is appended to the $f$ (or $r$, 
respectively) component of the \lstinline!Deep! c-deque. Otherwise, the 
internal deque of the \lstinline!Shallow! c-deque becomes the new $f$ (or $r$, 
respectively) and the old $f$ ($r$) is added to the $a$ ($b$) component as a 
\lstinline!Simple! compound element.
\begin{code}
append (Shallow d) (Deep f a m b r)
  | D.size d<4 = Deep (D.append d f) a m b r
  | otherwise  = Deep d (cons (Simple f) a) m b r
append (Deep f a m b r) (Shallow d)
  | D.size d<4 = Deep f a m b (D.append r d)
  | otherwise  = Deep f a m (snoc b (Simple r)) d
\end{code}
The last case, appending two \lstinline!Deep! c-deques, is the most 
complicated one. Instead of translating the very concise Haskell code into 
English that is not necessarily any easier to understand, figure 
\ref{fig_catdequeappend} tries to visualise what is done (note that the : 
denotes both \lstinline!cons! and \lstinline!snoc!, respectively).
\begin{figure}[htbp]
\begin{center}
\includegraphics{CatenableDeque.2}
\caption{\lstinline!append! for two \lstinline!Deep! 
\lstinline!CatenableDeque!s}
\label{fig_catdequeappend}
\end{center}
\end{figure}
  
\begin{code}
append (Deep f1 a1 m1 b1 r1) (Deep f2 a2 m2 b2 r2)
  = let (r1', m, f2') = share r1 f2
        a1' = snoc a1 (Cmpd m1 b1 r1')
	b2' = cons (Cmpd f2' a2 m2) b2
    in Deep f1 a1' m b2' r2
\end{code}
All functions called by \lstinline!append! have constant amortised run-time: 
For \lstinline!share!, this follows directly from the constant amortised 
run-time of the called deque functions, for \lstinline!cons! and 
\lstinline!snoc!, this was established above, and for the 
deque-\lstinline!append!, at least one of its arguments has three or less 
arguments, bounding its run-time. This leaves the debits for the cases 
involving \lstinline!Deep! c-deques to be analysed. However, as the debits on 
$a$ and $b$ are at most five, we can discharge all of them (at most 20), plus 
those two that may be created by the \lstinline!cons! and \lstinline!snoc!, 
which obviously is sufficient to preserve the debit invariant, and still get 
constant amortised run-time. (A more careful analysis shows that it is 
actually enough to discharge four debits \cite{okasaki_icfp97}).

Accessing the head element is trivially done in constant amortised time using 
the respective deque operation.
\begin{code}
lhead (Shallow d) = D.lhead d
lhead (Deep f _ _ _ _) = D.lhead f
\end{code}

Removing the head element requires much more thought. The \lstinline!Shallow! 
form is quite easy, again; also
for a \lstinline!Deep! c-deque with $f$ containing four or more elements, it 
is sufficient just to remove the head of $f$. Otherwise (i.e. $f$ containing 
the minimum number of three elements), several cases have to distinguished:
\begin{itemize}
\item If $a$ is non-empty, we can remove its head element to acquire a second 
deque that we \lstinline!append! to the tail of $f$ to get back to at least 
three elements. If the head element of $a$ is a \lstinline!Simple! compound 
element, the deque it carries is the one we need. For a \lstinline!Cmpd!, its 
$f$ component is the desired deque, but we need to take care of its $c$ and 
$r$ components as well. We \lstinline!cons! the $r$ component (encapsulated in 
a \lstinline!Simple! compound element) to the tail of $a$ and then append the 
result to $c$, which is a c-deque itself, to get the new $a$.
\item If $a$ is empty, but $b$ is not, we append $m$ to the tail of $f$ to 
arrive at a resulting $f$ with at least three elements and extract the head of 
$b$ to obtain a new $m$. If the head of $b$ is a \lstinline!Simple! compound 
element, the deque it carries is our new $m$ and we are done. If it is a 
\lstinline!Cmpd!, its $r$ component becomes the new $m$, while its $f$ and $c$ 
components are joined together (by encapsulating $f$ in a \lstinline!Simple! 
compound element and \lstinline!cons!ing to $c$) to replace the formerly empty 
$a$.
\item If both $a$ and $b$ are empty, tail of $f$ and $m$ are appended, the 
result is put in a \lstinline!Shallow! c-deque, as is $r$, and these two 
c-deques are then \lstinline!append!ed to form the desired result.
\end{itemize}
In the case of $|f|=3$ and head of $a$ being a \lstinline!Cmpd! compound 
element, we immediately replace $a$'s head after removing it. In that case, it 
is of course unnecessary to preserve the minimum length of $a$'s $f$ component 
during the \lstinline!ltail!, so we define an auxiliary function that handles 
this \lstinline!cons x (ltail cd)! a little more efficiently.
\begin{code}
replacelhead :: a -> Seq a -> Seq a
replacelhead x (Shallow d) = Shallow (D.cons x (D.ltail d))
replacelhead x (Deep f a m b r) = Deep (D.cons x (D.ltail f)) a m b r
\end{code}
Putting all that together, we arrive at the rather lengthy implementation of
\lstinline!ltail! (almost literally as in \cite{okasaki_pfds98}).
\begin{code}
ltail (Shallow d) = Shallow (D.ltail d)
ltail (Deep f a m b r) 
  | D.size f>3 = Deep (D.ltail f) a m b r
  | not (null a) = case lhead a of
                     Simple d -> Deep f' (ltail a) m b r
		       where f' = D.append (D.ltail f) d
		     Cmpd f' c' r' -> Deep f'' a'' m b r
		       where f'' = D.append (D.ltail f) f'
		             a'' = append c' (replacelhead (Simple r') a)
  | not (null b) = case lhead b of
                     Simple d -> Deep f' a d (ltail b) r
		       where f' = D.append (D.ltail f) m
		     Cmpd f' c' r' -> Deep f'' a'' r' (ltail b) r
		       where f'' = D.append (D.ltail f) m
		             a'' = cons (Simple f') c'
  | otherwise = append (Shallow (D.append (D.ltail f) m)) (Shallow r)
\end{code}
In the case of a \lstinline!Shallow! c-deque, \lstinline!ltail! obviously runs 
in \Oof{1} amortised. Otherwise, by inspection, all functions invoked inside 
\lstinline!ltail! run in constant amortised time, with the possible exception
of the recursion, which is suspended. So again, the debit analysis becomes the 
critical part. Due to the recursive structure, we will argue by debits that 
are passed to the surrounding suspension; at the out-most level, these are the 
debits that are actually to be discharged. We will establish that at most five 
debits are passed to the surrounding suspension, dealing with the different 
cases distinctly.
\begin{description}
\item[$|f|>3$:] No recursive call occurs, but it may be the case that $|f|$ 
drops to three, thereby reducing the debit allowance on $a$ by four and on $b$ 
by one, making it necessary to pass up to five debits to the surrounding 
suspension.
\item [$|f|=3$, $a$ non-empty, head of $a$ is \texttt{Simple}:] If $|r|>3$, 
$a$ may carry one debit which we pass out to the surrounding suspension. As 
the $f$ component of the resulting c-deque will have more than three elements, 
we may leave four or five (depending or $r$) debits of the five we inherit 
from the recursive call on $a$, so we have to pass out one debit now if 
$|r|=3$, and hence always pass out at most one debit.
\item [$|f|=3$, $a$ non-empty, head of $a$ is \texttt{Cmpd}:] Again, if 
$|r|>3$ we have to pass one debit from $a$ on to the surrounding suspension. 
But the debits associated with the new $a$ are higher: The $c$ component may 
carry up to four debits, the \lstinline!append! produces up to four debits and 
the \lstinline!replacelhead! produces one additional debit, summing up to nine 
debits. If $|r|=3$ we have to pass five of them on, otherwise four are 
sufficient, so that at most five debits have to be passed on in total.
\item [$|f|=3$, $a$ empty, $b$ non-empty, head of $b$ is \texttt{Simple}:]
If $|r|>3$, we have to pass up to four debits from $b$ on to the surrounding 
suspension, but can leave all the five debits generated by the recursive 
call to \lstinline!ltail!, as afterwards also $|f|>3$. Otherwise, $b$ is 
guaranteed to be free of debits, but we have to pass four of the five 
generated debits out. Hence, at most four debits have to be passed out.
\item [$|f|=3$, $a$ empty, $b$ non-empty, head of $b$ is \texttt{Cmpd}:]
For the debits on $b$ the same argument holds, but we also have to considers 
the new $a$ component. The $c$ component of the compound element may carry up 
to four debits, and the \lstinline!cons! generates one additional debit. If 
$|r|=3$, only four debits are allowed on $a$, so we have to pass out one more 
debit, yielding a total of five debits to be passed on to the surrounding 
suspension.
\item [$|f|=3$, $a$ and $b$ empty:] No recursive call occurs, but the call to 
\lstinline!append! may result in up to four debits to be passed on.
\end{description}
So, indeed \lstinline!ltail! runs in \Oof{1} amortised.

For \lstinline!lview!, we use \lstinline!lhead! and \lstinline!ltail! to come 
up with a straight-forward \Oof{1} implementation.
\begin{code}
lview cd | null cd = Nothing2
         | otherwise = Just2 (lhead cd) (ltail cd)
\end{code}

Exploiting symmetry, we can now define \lstinline!rhead!, \lstinline!rtail! 
and \lstinline!rview! in a similar way, also yielding constant amortised 
run-time.
\begin{code}
rhead (Shallow d) = D.rhead d
rhead (Deep _ _ _ _ r) = D.rhead r

replacerhead (Shallow d) x = Shallow (D.snoc (D.rtail d) x)
replacerhead (Deep f a m b r) x = Deep f a m b (D.snoc (D.rtail r) x)

rtail (Shallow d) = Shallow (D.rtail d)
rtail (Deep f a m b r) 
  | D.size r>3 = Deep f a m b (D.rtail r)
  | not (null b) = case rhead b of
                     Simple d -> Deep f a m (rtail b) r'
		       where r' = D.append d (D.rtail r)
		     Cmpd f' c' r' -> Deep f a m b'' r''
		       where r'' = D.append r' (D.rtail r)
		             b'' = append (replacerhead b (Simple f')) c'
  | not (null a) = case rhead a of
                     Simple d -> Deep f (rtail a) d b r'
		       where r' = D.append m (D.rtail r)
		     Cmpd f' c' r' -> Deep f (rtail a) f' b'' r''
		       where r'' = D.append m (D.rtail r)
		             b'' = snoc c' (Simple r')
  | otherwise = append (Shallow f) (Shallow (D.append m (D.rtail r)))
  
rview cd | null cd = Nothing2
         | otherwise = Just2 (rtail cd) (rhead cd)
\end{code}

As only a \lstinline!Shallow! c-deque can be empty, the test whether a given 
c-deque is empty reduces to the respective test for normal deques (which runs 
in constant amortised time).
\begin{code}
null (Shallow d) = D.null d
null _ = False
\end{code}

Determining the size of a c-deque is a little more involved for the 
\lstinline!Deep! form.
\begin{code}
size (Shallow d) = D.size d
size (Deep f a m b r) = D.size f + foldr size' 0 a + D.size m 
                                 + foldr size' 0 b + D.size r 
  where size' (Simple d) n = D.size d + n
        size' (Cmpd f c r) n = D.size f + foldr size' 0 c + D.size r + n
\end{code}
The deque \lstinline!size! operation runs in constant time, and all deques 
are non-empty, so that these contribute at most linear run-time in total, 
as only \Oof{n} applications of the deque \lstinline!size! operation can be 
performed. As each (recursive) call to \lstinline!size'! results in at least 
one call to \lstinline!size!, the total number of these is obviously also 
linear in $n$. Furthermore, the amount of debits discharged per recursion is 
constant, thereby yielding amortised run-time \Oof{n}.

Reversing a \lstinline!Shallow! \lstinline!CatenableDeque! is carried out by
simply reversing the deque used inside. For the \lstinline!Deep! form, the 
order of the five components has to be reversed, the components themselves have
to be reversed, and for the $a$ and $b$ components, the compound elements they
contain have to be reversed.
\begin{code}
reverse (Shallow d) = Shallow (D.reverse d)
reverse (Deep f a m b r) 
  = Deep f' a' m' b' r'
      where f' = D.reverse r
            a' = reverse (map rev' b)
            m' = D.reverse m
	    b' = reverse (map rev' a)
	    r' = D.reverse f
	    rev' (Simple d) = Simple (D.reverse d)
	    rev' (Cmpd f c r) = Cmpd (D.reverse r) (reverse (map rev' c))     
                                     (D.reverse f)
\end{code}
Observing that the deque \lstinline!reverse! operation is \Oof{1}, with the 
same argument as for \lstinline!size!, linear amortised run-time can be 
established.

Conversion of a list into a \lstinline!CatenableDeque! is performed by just 
converting the list into a deque and then wrapping it up in the 
\lstinline!Shallow! constructor, therefore taking linear amortised time like
the respective deque function.
\begin{code}
fromList xs = Shallow (D.fromList xs)
\end{code}

The \lstinline!map! and \lstinline!fold! family of operations is implemented
by calls to the respective deque operations, taking special care of the 
compound elements. As the total number of debits is \Oof{n}, all of which have
to be discharged, again the $n$-fold application of the function 
argument \lstinline!g! determines the run-time.
\begin{code}
map g (Shallow d) = Shallow (D.map g d)
map g (Deep f a m b r) 
  = Deep f' a' m' b' r'
      where f' = D.map g f
            a' = map g' a
	    m' = D.map g m
	    b' = map g' b
	    r' = D.map g r
	    g' (Simple d) = Simple (D.map g d)
	    g' (Cmpd f c r) = Cmpd (D.map g f) (map g' c) (D.map g r)

foldr g e (Shallow d) = D.foldr g e d
foldr g e (Deep f a m b r) = D.foldr g (foldr g' (D.foldr g (foldr g' 
                                                   (D.foldr g e r) b) m) a) f  
  where g' (Simple d) e = D.foldr g e d        
        g' (Cmpd f c r) e = D.foldr g (foldr g' (D.foldr g e r) c) f

foldr1 g (Shallow d) = D.foldr1 g d
foldr1 g (Deep f a m b r) = D.foldr g (foldr g' (D.foldr g (foldr g' 
                                                   (D.foldr1 g r) b) m) a) f  
  where g' (Simple d) e = D.foldr g e d        
        g' (Cmpd f c r) e = D.foldr g (foldr g' (D.foldr g e r) c) f

foldl g e (Shallow d) = D.foldl g e d
foldl g e (Deep f a m b r) = D.foldl g (foldl g' (D.foldl g (foldl g' 
                                                   (D.foldl g e f) a) m) b) r 
  where g' e (Simple d) = D.foldl g e d        
        g' e (Cmpd f c r) = D.foldl g (foldl g' (D.foldl g e f) c) r

foldl1 g (Shallow d) = D.foldl1 g d
foldl1 g (Deep f a m b r) = D.foldl g (foldl g' (D.foldl g (foldl g' 
                                                   (D.foldl1 g f) a) m) b) r
  where g' e (Simple d) = D.foldl g e d        
        g' e (Cmpd f c r) = D.foldl g (foldl g' (D.foldl g e f) c) r
\end{code}

The \lstinline!copy! and \lstinline!tabulate! constructors again just wrap up
the respective deque operations in \lstinline!Shallow! constructors, preserving 
their constant amortised run-time.
\begin{code}
copy n x = Shallow (D.copy n x)
tabulate n f = Shallow (D.tabulate n f)
\end{code}

While \lstinline!drop! on a \lstinline!Shallow! \lstinline!CatenableDeque!
again just calls the respective deque operation, we distinguish two cases for 
the \lstinline!Deep! form. If $|f|-i\geq3$, we can just \lstinline!drop! $i$ 
elements from $f$ without violating the invariant. Otherwise, we 
\lstinline!drop! all but three elements, call \lstinline!ltail! to remove the 
next and then call \lstinline!drop! recursively to remove the remaining 
elements.
\begin{code}
drop i (Shallow d) = Shallow (D.drop i d)
drop i (Deep f a m b r)
  | D.size f - i >= 3 = Deep (D.drop i f) a m b r
  | otherwise = drop (i - D.size f + 2) 
                     (ltail (Deep (D.drop (D.size f - 3) f) a m b r))
\end{code}
The non-recursing cases obviously take linear time, as so does the deque 
\lstinline!drop!, and at most five debits have to be discharged in case the 
length of $f$ drops to three. As both \lstinline!ltail! as 
\lstinline!D.drop (D.size f - 3) f! run in constant amortised time, and $i$ is
reduced by at least five in the recursive call, we get amortised run-time 
\Oof{i} in total.

The case distinction for \lstinline!take! is similar. If the 
\lstinline!CatenableDeque! is \lstinline!Shallow! or the index lies within 
$f$, we can just use the deque \lstinline!take! and wrap the result up in 
a \lstinline!Shallow! \lstinline!CatenableDeque! again. Otherwise, we 
\lstinline!append! $f$ to the first $i-|f|$ elements of the 
\lstinline!CatenableDeque! that remains after dropping $|f|$ elements.
\begin{code}
take i (Shallow d) = Shallow (D.take i d)
take i d@(Deep f a m b r) 
  | i <= D.size f = Shallow (D.take i f)
  | otherwise = append (Shallow f) (take (i-D.size f) (drop (D.size f) d))
\end{code}
The non-recursing cases again run in \Oof{i} amortised. In the recursing case, 
the \lstinline!drop! runs in constant amortised time, as a closer inspection 
reveals: It recurses only once, with $i=2=\Oof{1}$. As \lstinline!append! 
also runs in constant amortised time and the $i$ is reduced by at least 
$|f|\geq3$ in the recursive call, we get total amortised run-time \Oof{i}.

Checking whether a given index is valid for the \lstinline!Shallow! form is 
again performed using the respective deque operation. For the \lstinline!Deep! 
form, we exploit the constant time \lstinline!size! operation of the deque to 
first check whether the elements stored in the $f$, $m$ and $r$ components are 
already sufficient to establish that the index is valid. If not, we use 
\lstinline!drop!, resulting in amortised run-time of \Oof{i}.
\begin{code}
inBounds _ i | i<0 = False
inBounds (Shallow d) i = D.inBounds d i
inBounds (Deep f a m b r) i
  | i<(D.size f + D.size m + D.size r) = True
  | otherwise = inBoundsUsingDrop (Deep f a m b r) i
\end{code}

Concatenating multiple \lstinline!CatenableDeque! can conveniently be 
performed by repeated application of the constant time \lstinline!append!, 
yielding an amortised run-time linear in the number of 
\lstinline!CatenableDeque!s to be concatenated. For \lstinline!concatMap!, the 
time required for the given function has to be added, of course.
\begin{code}
concat = concatUsingFoldr
concatMap = concatMapUsingFoldr
\end{code}

The \lstinline!reverseOnto! operation uses \lstinline!reverse! and 
\lstinline!append!, resulting in linear amortised run-time.
\begin{code}
reverseOnto = reverseOntoUsingReverse
\end{code}

Conversion to a list uses \lstinline!foldr! to \lstinline!cons! the result 
list together. This results in a list that can be evaluated incrementally, 
with a total run-time of \Oof{n}.
\begin{code}
toList = toListUsingFoldr
\end{code}

As it is quite complex to divide a \lstinline!CatenableDeque! evenly, the 
\lstinline!reduce! family of operations uses a list representation internally, 
giving again run-time that is determined by the $n$-fold application of the
function argument.
\begin{code}
reducer = reducerUsingReduce1
reducel = reducelUsingReduce1
reduce1 = reduce1UsingLists
\end{code}

Both \lstinline!splitAt! and \lstinline!subseq! call \lstinline!take! and 
\lstinline!drop! internally, thus having amortised run-time \Oof{i} and
\Oof{i+j}, respectively.
\begin{code}
splitAt = splitAtDefault
subseq = subseqDefault
\end{code}

The  \lstinline!lookup! family of operations just \lstinline!drop!s the first
$i$ elements to access the requested index, resulting in an amortised run-time 
of \Oof{i}.
\begin{code}
lookup = lookupUsingDrop
lookupM = lookupMUsingDrop
lookupWithDefault = lookupWithDefaultUsingDrop
\end{code}

Changing an element using \lstinline!update! or \lstinline!adjust! is 
performed by splitting the \lstinline!CatenableDeque! at the given index,
modifying the respective element and \lstinline!append!ing the two parts 
together again. In addition to the potentially expensive application of the
function argument in \lstinline!adjust!, the \lstinline!splitAt! with its 
amortised run-time \Oof{i} determines the total run-time.
\begin{code}
update = updateUsingSplitAt
adjust = adjustUsingSplitAt
\end{code}

The run-time of the \lstinline!*WithIndex! operations, which use lists
internally, is again dominated by the application of \lstinline!f! $n$ times.
\begin{code}
mapWithIndex = mapWithIndexUsingLists
foldrWithIndex = foldrWithIndexUsingLists
foldlWithIndex = foldlWithIndexUsingLists
\end{code}

\lstinline!filter! and  \lstinline!partition! use \lstinline!foldr! to 
traverse the \lstinline!CatenableDeque! and build the result(s), as usual 
having time-complexity that is determined by the $n$-fold application of 
the function argument \lstinline!f!.
\begin{code}
filter = filterUsingFoldr
partition = partitionUsingFoldr
\end{code}

The same holds for the \lstinline!*While! operation, which use 
\lstinline!lview! to traverse the \lstinline!CatenableDeque! as far as 
required.
\begin{code}
takeWhile = takeWhileUsingLview
dropWhile = dropWhileUsingLview
splitWhile = splitWhileUsingLview
\end{code}

As \lstinline!toList! is incremental, the \lstinline!zip! family of 
operations, using list representations internally, has time linear in the 
length of the shortest \lstinline!CatenableDeque! involved.
\begin{code}
zip = zipUsingLists
zip3 = zip3UsingLists
zipWith = zipWithUsingLists
zipWith3 = zipWith3UsingLists
\end{code}

For the \lstinline!unzip! operations, the traversal is performed using 
\lstinline!foldr!, and the resulting run-time once again linear or determined 
by the function argument, respectively.
\begin{code}
unzip = unzipUsingFoldr
unzip3 = unzip3UsingFoldr
unzipWith = unzipWithUsingFoldr
unzipWith3 = unzipWith3UsingFoldr
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

Generation of arbitrary \lstinline!CatenableDeque!s is a little more involved
than for the other implementations, as we need to implement the
\lstinline!Arbitrary! interface for two data types. 

Observing that the structural invariant demands several deques to have a certain
minimum size, we start of by defining a helper function to produce an arbitrary
\lstinline!BankersDeque!s with a given minimum size. We do this by simply
generating a deque of arbitrary size and then adding $n$ more elements to it to
ensure a minimum size of $n$.
\begin{code}
minLengthDeque :: (Arbitrary a) => Int -> Gen (D.Seq a)
minLengthDeque 0 = arbitrary
minLengthDeque n = do x <- arbitrary
                      xs <- minLengthDeque (n-1)
                      return (D.cons x xs)
\end{code}

With this helper function, we can easily generate an arbitrary compound element
that fulfills the invariant. As obviously no other restrictions as to what is
generated then the minimum lengths apply, any allowed compound element can be
produced. It should be noted that even for very small generation sizes, the
\lstinline!Cmpd! case may be entered resulting in a \lstinline!CmpdElem! with at
least four elements. We allow this to get more interesting test cases, i.e.
non-shallow instances.
\begin{code}  
instance Arbitrary a => Arbitrary (CmpdElem a) where
  arbitrary = oneof [ simple, sized cmpd ]
    where simple = liftM Simple (minLengthDeque 2)
	  cmpd size = resize (size `div` 3) $
	                liftM3 Cmpd (minLengthDeque 2) arbitrary (minLengthDeque 2)
\end{code}
\hide{
\begin{code}
  coarbitrary (Simple d) = variant 0 . coarbitrary d
  coarbitrary (Cmpd f c r) = variant 1 . coarbitrary f . coarbitrary c . 
                             coarbitrary r   
\end{code}
}

Generation of an arbitrary \lstinline!CatenableDeque! works in a very similar
way, except that we restrict generation to the \lstinline!Shallow! variant for
small sizes to avoid infinite recursion. Again, it easy to see that any allowed
\lstinline!CatenableDeque! can be generated.
\begin{code}
instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = oneof [ shallow, sized deep ]
    where shallow = liftM Shallow arbitrary
          deep size | size<8    = shallow 
	            | otherwise = resize (size `div` 5) $
                                    liftM5 Deep (minLengthDeque 3) 
                                                arbitrary 
                                                (minLengthDeque 2) 
                                                arbitrary 
                                                (minLengthDeque 3)
\end{code}

\hide{
\begin{code}
  coarbitrary (Shallow d) = variant 0 . coarbitrary d
  coarbitrary (Deep f a m b r) = variant 1 . coarbitrary f . coarbitrary a .
                                 coarbitrary m . coarbitrary b . coarbitrary r
\end{code}
}

Generation of a \lstinline!CatenableDeque! with given contents is trickier as we
cannot arbitrarily add elements to the deques to provide the required minimum
size. Instead, we have to divide the given elements in a legal way. We introduce
additional helper functions to accomplish this, starting with one that produces
a list of numbers that add up to a given value. As an extra argument, it takes a
list of lists of allowed values. The advantage of this compared to a list of
minimum values, which would seem sufficient at first, is that it also allows the
case where zero elements are allowed as well, but at least two otherwise. The
domain lists have to in increasing order; this permits the function to just
repeatedly remove the head element of a randomly chosen domain list until all
head elements add up to the desired value. Before removal of the head element,
it is ensured that this will not result in a too high value. Of course, this
approach can get stuck if e.g. the sum has to be increased by only one, but all
possible increases are at least two. However, in all applications that follow
below, at least one domain list will only consist of increases by one.
\begin{code}
fillToSum :: [[Int]] -> Int -> Gen [Int]
fillToSum xxs n = if s >= n then return (S.map S.lhead xxs)
                  else do i <- choose (0, length xxs -1)
                          fillToSum (S.adjust maybeTail i xxs) n
                  where s = sum (S.map S.lhead xxs)
                        maybeTail (x1:x2:xs) = if x2-x1 <= n-s then x2:xs
                                               else x1:x2:xs
\end{code}

Secondly, we define a function that splits a list into parts, where the lengths
of the parts can be restricted with the same kind of domain lists as explained
above. The function is straight-forward by using \lstinline!fillToSum!.
\begin{code}
splitToSizes :: [[Int]] -> [a] -> Gen [[a]]
splitToSizes is xs = liftM (splitMulti xs) (fillToSum is (S.size xs))
  where splitMulti [] [] = []
        splitMulti xs (i:is) = let (xs', xs'') = S.splitAt i xs
                                   xxs = splitMulti xs'' is
                               in (xs':xxs)
\end{code}

Finally we can turn to the main \lstinline!abitraryWith! function. First, we
examine the length of the input list: if it is empty, so is the result; if it
contains less than eight elements, the result has to be \lstinline!Shallow!;
otherwise, both \lstinline!Shallow! and \lstinline!Deep! form are possible.
\begin{code}
instance CheckSeq.CheckableSequence Seq where
  arbitraryWith [] = return empty
  arbitraryWith xs 
    | S.size xs < 8 = shallow
    | otherwise = oneof [ shallow, deep ]
\end{code}
While generation of the \lstinline!Shallow! form is trivial, we need to use the
helper function introduced above to split the input list into suitable parts.
The deque parts $f$, $m$ and $r$ have simple minimum sizes, while $a$ and $b$
may be empty, but cannot contain only one element, because a
\lstinline!CmpdElem! has to contain at least two elements. To generate $a$ and
$b$, we first generate a list of compound elements with \lstinline!cmpdList!
(see below), and then call \lstinline!arbitraryWith! on this recursively.
\begin{code}
    where shallow = return (Shallow (D.fromList xs))
          deep = do [f', a', m', b', r'] <- splitToSizes [[3..], 
                                                          (0:[2..]), 
                                                          [2..], 
                                                          (0:[2..]), 
                                                          [3..]] xs
                    a <- (cmpdList a' >>= CheckSeq.arbitraryWith)
	            b <- (cmpdList b' >>= CheckSeq.arbitraryWith)		          
	            return (Deep (D.fromList f') 
                                 a 
                                 (D.fromList m') 
                                 b 
                                 (D.fromList r'))
\end{code}
The list of compound elements is generated by first choosing its length, then
dividing the input elements accordingly, and finally wrapping them up in
\lstinline!CmpdElem!s.
\begin{code}
	  cmpdList :: [a] -> Gen [CmpdElem a]
	  cmpdList [] = return []
          cmpdList xs = do n <- choose (1, S.size xs `div` 2)
                           xxs <- splitToSizes (S.copy n [2..]) xs
                           mapM arbitraryCmpdWith xxs
\end{code}
Generation of an arbitrary compound element for given contents is similar to
generation of a \lstinline!CatenableDeque!. If there are less than four
elements, it can only be \lstinline!Simple!, otherwise both forms are allowed.
\begin{code}
	  arbitraryCmpdWith :: [a] -> Gen (CmpdElem a)
	  arbitraryCmpdWith xs 
	    | S.size xs < 4 = simple xs
	    | otherwise = oneof [ simple xs, cmpd xs ]
\end{code}
Again, the \lstinline!simple! case is indeed simple, while the \lstinline!cmpd! case
works in a way similar to the \lstinline!deep! case above.
\begin{code}
	  simple xs = return (Simple (D.fromList xs))
          cmpd xs = do [f', c', r'] <- splitToSizes [[2..], 
                                                     (0:[2..]), 
                                                     [2..]] xs
                       c <- (cmpdList c' >>= CheckSeq.arbitraryWith)
	               return (Cmpd (D.fromList f') c (D.fromList r'))
\end{code}
As the \lstinline!CatenableDeque! is independent of the internals of the
\lstinline!BankersDeque!, which is moreover expected to correct (or at least
checked by itself), \lstinline!fromList! can safely be used to produce the
deques instead of \lstinline!arbitraryWith!.
\hide{
\begin{code}
  invariant = invariant

checkme = CheckSeq.checkseq (invariant :: Seq Int -> Bool)
\end{code}
}
