-- |
--   Module      :  Data.Edison.Sym
--   Copyright   :  Copyright (c) 2006 Robert Dockins
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   This module introduces a number of infix symbols which are aliases
--   for some of the operations in the sequence and set abstractions.
--   For several, the argument orders are reversed to more closely
--   match usual symbolic usage.
--
--   The symbols are intended to evoke the the operations they
--   represent.  Unfortunately, ASCII is pretty limited, and Haskell
--   only allocates a few symbols to the operator lexical class.
--   Thus, some of the operators are less evocative than one would
--   like.  A future version may introduce unicode operators, which
--   will allow a wider range of operations to be represented symbolicly.
--
--   Unlike most of the modules in Edison, this module is intended to be
--   imported unqualified.  However, the definition of @(++)@ will conflict
--   with the Prelude definition.  Either this definition or the Prelude
--   definition will need to be imported @hiding ( (++) )@.  This definition
--   subsumes the Prelude definition, and can be safely used in place of it.

module Data.Edison.Sym where

import qualified Prelude as P
import qualified Data.Edison.Seq as S
import qualified Data.Edison.Coll as C
import qualified Data.Edison.Coll as A

-- | Left (front) cons on a sequence.  The new element appears on the left.
--   Identical to 'S.lcons'.
(<|) :: S.Sequence seq => a -> seq a -> seq a
(<|) = S.lcons

-- | Right (rear) cons on a sequence.  The new element appears on the right.
--   Identical to 'S.rcons' with reversed arguments.
(|>) :: S.Sequence seq => seq a -> a -> seq a
(|>) = P.flip S.rcons

-- | Append two sequences.  Identical to 'S.append'.  Subsumes the Prelude
--   definition.
(++) :: S.Sequence seq => seq a -> seq a -> seq a
(++) = S.append

-- | Lookup an element in a sequence.  Identical to 'S.lookup' with 
--   reversed arguments.
(!) :: S.Sequence seq => seq a -> P.Int -> a
(!) = P.flip S.lookup

-- | Subset test operation.  Identical to 'C.subsetEq'.
(|=) :: C.SetX set a => set -> set -> P.Bool
(|=) = C.subsetEq

-- | Set difference.  Identical to 'C.difference'.
(\\) :: C.SetX set a => set -> set -> set
(\\) = C.difference

-- | Set intersection.  Identical to 'C.intersection'.
(/\) :: C.SetX set a => set -> set -> set
(/\) = C.intersection

-- | Set union.  Identical to 'C.union'.
(\/) :: C.SetX set a => set -> set -> set
(\/) = C.union
