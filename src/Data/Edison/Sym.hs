module Data.Edison.Sym where

import qualified Prelude as P
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Coll as A

(<|) :: S.Sequence seq => a -> seq a -> seq a
(<|) = S.lcons

(|>) :: S.Sequence seq => seq a -> a -> seq a
(|>) = P.flip S.rcons

(++) :: S.Sequence seq => seq a -> seq a -> seq a
(++) = S.append

(!) :: S.Sequence seq => seq a -> P.Int -> a
(!) = P.flip S.lookup

(|=) :: C.SetX set a => set -> set -> P.Bool
(|=) = C.subsetEq

(\\) :: C.SetX set a => set -> set -> set
(\\) = C.difference

(/\) :: C.SetX set a => set -> set -> set
(/\) = C.intersection

(\/) :: C.SetX set a => set -> set -> set
(\/) = C.union
