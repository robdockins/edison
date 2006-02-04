module Data.Edison.Facade.Stack (
	Stack,
	push,
	topM, top,
	popM, pop,

	-- Imported from Sequence
	empty
)
where

import Data.Edison.Seq.Sequence
import qualified Data.Edison.Seq.ListSeq as LS

type Stack a = LS.Seq a

push :: (Sequence s) => a -> s a -> s a
push = Sequence.lcons

topM :: (Sequence s, Monad rm) => s a -> rm a
topM s
  | Sequence.null s = fail "Stack.topM: empty stack"
  | otherwise       = return (Sequence.lhead s)

top :: (Sequence s) => s a -> a
top = Sequence.lhead

popM :: (Sequence s, Monad rm) => s a -> rm (s a)
popM s
  | Sequence.null s = fail "Stack.popM: empty stack"
  | otherwise       = return (Sequence.ltail s)

pop :: (Sequence s) => s a -> s a
pop = Sequence.ltail

