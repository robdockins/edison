-- |
--   Module      :  Data.Edison.Assoc
--   Copyright   :  Copyright (c) 1998 Chris Okasaki
--   License     :  MIT; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  stable
--   Portability :  GHC, Hugs (MPTC and FD)
--
--   The /associative collection/ abstraction includes finite maps, finite
--   relations, and priority queues where the priority is separate from the
--   element.  Associative collections are defined in Edison as a set of eight
--   classes.
--
--   Note that this
--   hierarchy mirrors the hierarchy for collections, but with the addition
--   of 'Functor' as a superclass of every associative collection. See 
--   "Data.Edison.Coll" for a description of the class hierarchy.
--
--   In almost all cases, associative collections make no guarantees about
--   behavior with respect to the actual keys stored and (in the case of
--   observable maps) which keys can be retrieved.  We adopt the convention
--   that methods which create associative collections are /unambiguous/
--   with respect to the key storage behavior, but that methods which can
--   observe keys are /ambiguous/ with respect to the actual keys returned.
--
--   In all cases where an operation is ambiguous with respect to the key,
--   the operation is rendered /unambiguous/ if the @Eq@ instance on keys
--   corresponds to indistinguisability.

module Data.Edison.Assoc (
    -- * Superclass aliases
    map,

    -- * Non-observable associative collections
    AssocX(..),
    OrdAssocX(..),
    FiniteMapX(..),
    OrdFiniteMapX,

    -- * Observable associative collections
    Assoc(..),
    OrdAssoc(..),
    FiniteMap(..),
    OrdFiniteMap,

    -- * Specilizations of submap operations
    submap,
    properSubmap,
    sameMap,

    -- * Specializations of sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    lookupList,
    elementsList,
    unsafeFromOrdList,
    fromListWith,
    fromListWithKey,
    insertListWith,
    insertListWithKey,
    unionListWith,
    toList,
    keysList,
    toOrdList,
    unionListWithKey

) where

import Prelude hiding (null,map,lookup,foldr,foldl,foldr1,foldl1,filter)

import qualified Control.Monad.Fail as Fail

import Data.Edison.Prelude

import Data.Edison.Seq(Sequence)
import Data.Edison.Seq.ListSeq()


-- | Apply a function to the elements of every binding in the associative
--   collection.  Identical to @fmap@ from @Functor@.
--
--   This function is always /unambiguous/.
map :: AssocX m k => (a -> b) -> m a -> m b
map = fmap

-- | Specialization of 'submapBy' where the comparison function is
--   given by @(==)@.
submap :: (Eq a,FiniteMapX m k) => m a -> m a -> Bool
submap = submapBy (==)

-- | Specialization of 'properSubmapBy' where the comparison function
--   is given by @(==)@.
properSubmap :: (Eq a, FiniteMapX m k) => m a -> m a -> Bool
properSubmap = properSubmapBy (==)

-- | Specialization of 'sameMapBy' where the comparison function is
--   given by @(==)@.
sameMap :: (Eq a,FiniteMapX m k) => m a -> m a -> Bool
sameMap = sameMapBy (==)


-- | The root class of the associative collection hierarchy.
class (Eq k,Functor m) => AssocX m k | m -> k where

  -- | The empty associative collection.
  --
  --   This function is always /unambiguous/.
  empty          :: m a

  -- | Create an associative collection with a single binding.
  --
  --   This function is always /unambiguous/.
  singleton      :: k -> a -> m a

  -- | Create an associative collection from a  list of bindings. Which element
  --   and key are kept in the case of duplicate keys is unspecified.
  --
  --   This function is /ambiguous/ at finite map types if the sequence
  --   contains more than one equivalent key.  Otherwise it is /unambiguous/.
  fromSeq        :: Sequence seq => seq (k,a) -> m a

  -- | Add a binding to an associative collection.  For finite maps, 'insert'
  --   keeps the new element in the case of duplicate keys.
  --
  --   This function is /unambiguous/.
  insert         :: k -> a -> m a -> m a

  -- | Add a sequence of bindings to a collection.  For finite maps, which key
  --   and which element are kept in the case of duplicates is unspecified.
  --   However, if a key appears in the sequence and in the map, (one of) the
  --   elements in the list will be given preference.
  --
  --   This function is /ambiguous/ at finite map types if the sequence contains
  --   more than one equivalent key.  Otherwise it is /unambiguous/.
  insertSeq      :: Sequence seq => seq (k,a) -> m a -> m a

  -- | Merge two associative collections.  For finite maps, which element
  --   to keep in the case of duplicate keys is unspecified.
  --
  --   This function is /ambiguous/ at finite map types if the map keys are not
  --   disjoint.  Otherwise it is /unambiguous/.
  union          :: m a -> m a -> m a

  -- | Merge a sequence of associative collections.  Which element
  --   to keep in the case of duplicate keys is unspecified.
  --
  --   This function is /ambiguous/ at finite map types if the map keys are not
  --   mutually disjoint.  Otherwise it is /unambiguous/.
  unionSeq       :: Sequence seq => seq (m a) -> m a

  -- | Delete one binding with the given key, or leave the associative collection
  --   unchanged if it does not contain the key.  For bag-like associative
  --   collections, it is unspecified which binding will be removed.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears more
  --   than once in the relation.  Otherwise it is /unambiguous/.
  delete         :: k -> m a -> m a

  -- | Delete all bindings with the given key, or leave the associative collection
  --   unchanged if it does not contain the key.
  --
  --   This function is always /unambiguous/.
  deleteAll      :: k -> m a -> m a

  -- | Delete a single occurrence of each of the given keys from an associative
  --   collection.  For bag-like associative collections containing duplicate keys,
  --   it is unspecified which bindings will be removed.
  --
  --   This function is /ambiguous/ at finite relation types if any key appears both
  --   in the sequence and in the finite relation AND the number of occurrences in
  --   the sequence is less than the number of occurrences in the finite relation.
  --   Otherwise it is /unambiguous/.
  deleteSeq      :: Sequence seq => seq k -> m a -> m a

  -- | Test whether the associative collection is empty.
  --
  --   /Axioms:/
  --
  -- * @null m = (size m == 0)@
  --
  --   This function is always /unambiguous/.
  null           :: m a -> Bool

  -- | Return the number of bindings in the associative collection.
  --
  --   This function is always /unambiguous/.
  size           :: m a -> Int

  -- | Test whether the given key is bound in the associative collection.
  --
  --   This function is always /unambiguous/.
  member         :: k -> m a -> Bool

  -- | Returns the number of bindings with the given key.  For finite maps
  --   this will always return 0 or 1.
  --
  --   This function is always /unambiguous/.
  count          :: k -> m a -> Int

  -- | Find the element associated with the given key.  Signals an error if
  --   the given key is not bound.  If more than one element is bound by the
  --   given key, it is unspecified which is returned.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookup         :: k -> m a -> a

  -- | Find the element associated with the given key.  Calls 'fail' if the
  --   given key is not bound.  If more than one element is bound by the given
  --   key, it is unspecified which is returned.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupM        :: (Fail.MonadFail rm) => k -> m a -> rm a

  -- | Return all elements bound by the given key in an unspecified order.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupAll      :: Sequence seq => k -> m a -> seq a

  -- | Find the element associated with the given key; return the element
  --   and the collection with that element deleted.  Signals an error if
  --   the given key is not bound.  If more than one element is bound by the
  --   given key, it is unspecified which is deleted and returned.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupAndDelete :: k -> m a -> (a, m a)

  -- | Find the element associated with the given key; return the element
  --   and the collection with that element deleted.  Calls @fail@ if
  --   the given key is not bound.  If more than one element is bound by the
  --   given key, it is unspecified which is deleted and returned.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupAndDeleteM :: (Fail.MonadFail rm) => k -> m a -> rm (a, m a)

  -- | Find all elements bound by the given key; return a sequence containing
  --   all such bound elements in an unspecified order and the collection
  --   with all such elements deleted.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupAndDeleteAll :: (Sequence seq) => k -> m a -> (seq a,m a)

  -- | Return the element associated with the given key.  If no such element
  --   is found, return the default.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  lookupWithDefault  :: a    -- ^ default element
                     -> k    -- ^ the key to look up
                     -> m a  -- ^ the associative collection
                     -> a

  -- | Change a single binding for the given key by applying a function to its
  --   element.  If the key binds more than one element, it is unspecified which
  --   will be modified.  If the key is not found in the collection, it is returned
  --   unchanged.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  adjust         :: (a -> a) -> k -> m a -> m a

  -- | Change all bindings for the given key by applying a function to its
  --   elements.  If the key is not found in the collection, it is returned
  --   unchanged.
  --
  --   This function is always /unambiguous/.
  adjustAll      :: (a -> a) -> k -> m a -> m a

  -- | Searches for a matching key in the collection.  If the key is found,
  --   the given function is called to adjust the value.  If the key is not
  --   found, a new binding is inserted with the given element. If the given
  --   key is bound more than once in the collection, it is unspecified
  --   which element is adjusted.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  adjustOrInsert    :: (a -> a) -> a -> k -> m a -> m a

  -- | Searches for all matching keys in the collection.  If the key is found,
  --   the given function is applied to all its elements to adjust their values.
  --   If the key is not found, a new binding is inserted with the given element.
  --
  --   This function is always /unambiguous/.
  adjustAllOrInsert :: (a -> a) -> a -> k -> m a -> m a

  -- | Change or delete a single binding for the given key by applying a function
  --   to its element.  If the function returns @Nothing@, then the binding
  --   will be deleted.  If the key binds more than one element, it is unspecified which
  --   will be modified.  If the key is not found in the collection, it is returned
  --   unchanged.
  --
  --   This function is /ambiguous/ at finite relation types if the key appears
  --   more than once in the finite relation.  Otherwise, it is /unambiguous/.
  adjustOrDelete    :: (a -> Maybe a) -> k -> m a -> m a

  -- | Change or delete all bindings for the given key by applying a function to
  --   its elements.  For any element where the function returns @Nothing@, the
  --   corresponding binding is deleted.  If the key is not found in the collection,
  --   it is returned unchanged.
  --
  --   This function is always /unambiguous/.
  adjustOrDeleteAll :: (a -> Maybe a) -> k -> m a -> m a

  -- | Combine all the elements in the associative collection, given a combining
  --   function and an initial value.  The elements are processed in an
  --   unspecified order.  /Note/ that 'fold' ignores the keys.
  --
  --   @fold f@ is /unambiguous/ iff @f@ is fold-commutative.
  fold           :: (a -> b -> b) -> b -> m a -> b

  -- | A strict variant of 'fold'.
  --
  --   @fold' f@ is /unambiguous/ iff @f@ is fold-commutative.
  fold'          :: (a -> b -> b) -> b -> m a -> b

  -- | Combine all the elements in a non-empty associative collection using the
  --   given combining function.  Signals an error if the associative collection
  --   is empty.  The elements are processed in an unspecified order.  An
  --   implementation may choose to process the elements linearly or in a
  --   balanced fashion (like 'reduce1' on sequences).  /Note/ that 'fold1'
  --   ignores the keys.
  --
  --   @fold1 f@ is /unambiguous/ iff @f@ is fold-commutative.
  fold1          :: (a -> a -> a) -> m a -> a

  -- | A strict variant of 'fold1'.
  --
  --   @fold1' f@ is /unambiguous/ iff @f@ is fold-commutative.
  fold1'         :: (a -> a -> a) -> m a -> a

  -- | Extract all bindings whose elements satisfy the given predicate.
  --
  --   This function is always /unambiguous/.
  filter         :: (a -> Bool) -> m a -> m a

  -- | Split an associative collection into those bindings which satisfy the
  --   given predicate, and those which do not.
  --
  --   This function is always /unambiguous/.
  partition      :: (a -> Bool) -> m a -> (m a, m a)

  -- | Returns all the elements in an associative collection, in an unspecified
  --   order.
  --
  --   This function is /ambiguous/ iff the associative collection contains
  --   more than one element.
  elements       :: Sequence seq => m a -> seq a

  -- | Semanticly, this function is a partial identity function.  If the
  --   datastructure is infinite in size or contains exceptions or non-termination
  --   in the structure itself, then @strict@ will result in bottom.  Operationally,
  --   this function walks the datastructure forcing any closures.  Elements contained
  --   in the map are /not/ forced.
  --
  --   This function is always /unambiguous/.
  strict :: m a -> m a

  -- | Similar to 'strict', this function walks the datastructure forcing closures.
  --   However, @strictWith@ will additionally apply the given function to the
  --   map elements, force the result using @seq@, and then ignore it.
  --   This function can be used to perform various levels of forcing on the
  --   sequence elements.  In particular:
  --
  -- > strictWith id xs
  --
  --   will force the spine of the datastructure and reduce each element to WHNF.
  --
  --   This function is always /unambiguous/.
  strictWith :: (a -> b) -> m a -> m a

  -- | A method to facilitate unit testing.  Returns 'True' if the structural
  --   invariants of the implementation hold for the given associative
  --   collection.  If this function returns 'False', it represents a bug;
  --   generally, either the implementation itself is flawed, or an unsafe
  --   operation has been used while violating the preconditions.
  structuralInvariant :: m a -> Bool

  -- | Returns the name of the module implementing this associative collection.
  instanceName   :: m a -> String


-- | An associative collection where the keys additionally have an ordering
--   relation.
class (AssocX m k, Ord k) => OrdAssocX m k | m -> k where
  -- | Remove the binding with the minimum key, and return its element together
  --   with the remaining associative collection.  Calls 'fail' if the
  --   associative collection is empty.  Which binding is removed if there
  --   is more than one minimum is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  minView            :: (Fail.MonadFail rm) => m a -> rm (a, m a)

  -- | Find the binding with the minimum key and return its element. Signals
  --   an error if the associative collection is empty.  Which element is chosen
  --   if there is more than one minimum is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  minElem            :: m a -> a

  -- | Remove the binding with the minimum key and return the remaining
  --   associative collection, or return empty if it is already empty.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  deleteMin          :: m a -> m a

  -- | Insert a binding into an associative collection with the precondition
  --   that the given key is @\<=@ any existing keys already in the collection.
  --   For finite maps, this precondition is strengthened to @\<@.
  --
  --   This function is /unambiguous/ under the preconditions.
  unsafeInsertMin    :: k -> a -> m a -> m a

  -- | Remove the binding with the maximum key, and return its element together
  --   with the remaining associative collection.  Calls 'fail' if the
  --   associative collection is empty.  Which binding is removed if there
  --   is more than one maximum is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  maxView            :: (Fail.MonadFail rm) => m a -> rm (a, m a)

  -- | Find the binding with the maximum key and return its element.  Signals
  --   an error if the associative collection is empty.  Which element is chosen
  --   if there is more than one maximum is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  maxElem            :: m a -> a

  -- | Remove the binding with the maximum key and return the remaining
  --   associative collection, or return empty if it is already empty.
  --
  --   This function is /ambiguous/ at finite relation types if the finite relation
  --   contains more than one minimum key.  Otherwise it is /unambiguous/.
  deleteMax          :: m a -> m a

  -- | Insert a binding into an associative collection with the precondition
  --   that the given key is @>=@ any existing keys already in the collection.
  --   For finite maps, this precondition is strengthened to @>@.
  --
  --   This function is /unambiguous/ under the precondition.
  unsafeInsertMax    :: k -> a -> m a -> m a

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with right associativity.  For finite maps, the order
  --   is increasing.
  --
  --   @foldr f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldr              :: (a -> b -> b) -> b -> m a -> b

  -- | A strict variant of 'foldr'.
  --
  --   @foldr' f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldr'             :: (a -> b -> b) -> b -> m a -> b

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with left associativity.  For finite maps, the order
  --   is increasing.
  --
  --   @foldl f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldl              :: (b -> a -> b) -> b -> m a -> b

  -- | A strict variant of 'foldl'.
  --
  --   @foldl' f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldl'             :: (b -> a -> b) -> b -> m a -> b

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with right associativity.  Signals an error if the
  --   associative collection is empty.  For finite maps, the order is
  --   increasing.
  --
  --   @foldr1 f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldr1             :: (a -> a -> a) -> m a -> a

  -- | A strict variant of 'foldr1'.
  --
  --   @foldr1' f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldr1'            :: (a -> a -> a) -> m a -> a

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with left associativity.  Signals an error if the
  --   associative collection is empty.  For finite maps, the order is
  --   increasing.
  --
  --   @foldl1 f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldl1             :: (a -> a -> a) -> m a -> a

  -- | A strict variant of 'foldl1'.
  --
  --   @foldl1' f@ is /unambiguous/ if @f@ is fold-commutative, at finite
  --   map types, or at finite relation types if the relation contains no
  --   duplicate keys.  Otherwise it is /ambiguous/.
  foldl1'            :: (a -> a -> a) -> m a -> a

  -- | Convert a sequence of bindings into an associative collection with the
  --   precondition that the sequence is sorted into non-decreasing order by
  --   key.  For finite maps, this precondition is strengthened to increasing
  --   order.
  --
  --   This function is /unambiguous/ under the precondition.
  unsafeFromOrdSeq   :: Sequence seq => seq (k,a) -> m a

  -- | Merge two associative collections with the precondition that every key
  --   in the first associative collection is @\<=@ every key in the second
  --   associative collection.  For finite maps, this precondition is
  --   strengthened to @\<@.
  --
  --   This function is /unambiguous/ under the precondition.
  unsafeAppend       :: m a -> m a -> m a

  -- | Extract all bindings whose keys are @\<@ the given key.
  --
  --   This function is always /unambiguous/.
  filterLT           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @\<=@ the given key.
  --
  --   This function is always /unambiguous/.
  filterLE           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @>@ the given key.
  --
  --   This function is always /unambiguous/.
  filterGT           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @>=@ the given key.
  --
  --   This function is always /unambiguous/.
  filterGE           :: k -> m a -> m a

  -- | Split an associative collection into two sub-collections, containing
  --   those bindings whose keys are @\<@ the given key and those which are @>=@.
  --
  --   This function is always /unambiguous/.
  partitionLT_GE     :: k -> m a -> (m a, m a)

  -- | Split an associative collection into two sub-collections, containing
  --   those bindings whose keys are @\<=@ the given key and those which are @>@.
  --
  --   This function is always /unambiguous/.
  partitionLE_GT     :: k -> m a -> (m a, m a)

  -- | Split an associative collection into two sub-collections, containing
  --   those bindings whose keys are @\<@ the given key and those which are @>@.
  --   All bindings with keys equal to the given key are discarded.
  --
  --   This function is always /unambiguous/.
  partitionLT_GT     :: k -> m a -> (m a, m a)

-- | An associative collection where the keys form a set; that is, each key
--   appears in the associative collection at most once.

class AssocX m k => FiniteMapX m k | m -> k where

  -- | Same as 'fromSeq', but with a combining function to resolve duplicates.
  --
  --   This function is always /unambiguous/.
  fromSeqWith        :: Sequence seq => (a -> a -> a) -> seq (k,a) -> m a

  -- | Same as 'fromSeq', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  --
  --   This function is always /unambiguous/.
  fromSeqWithKey     :: Sequence seq => (k -> a -> a -> a) -> seq (k,a) -> m a

  -- | Same as 'insert', but with a combining function to resolve duplicates.
  --
  --   This function is /unambiguous/.
  insertWith         :: (a -> a -> a) -> k -> a -> m a -> m a

  -- | Same as 'insert', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  --   The key passed to the combining function is always the same as the
  --   given key.
  --
  --   This function is /unambiguous/.
  insertWithKey      :: (k -> a -> a -> a) -> k -> a -> m a -> m a

  -- | Same as 'insertSeq', but with a combining function to resolve duplicates.
  --
  --   This function is /unambiguous/.
  insertSeqWith      :: Sequence seq =>
                           (a -> a -> a) -> seq (k,a) -> m a -> m a

  -- | Same as 'insertSeq', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  --
  --   This function is /unambiguous/.
  insertSeqWithKey   :: Sequence seq =>
                           (k -> a -> a -> a) -> seq (k,a) -> m a -> m a

  -- | Left biased union.
  --
  --   /Axioms:/
  --
  -- * @unionl = unionwith (\\x y -> x)@
  --
  --   This function is /unambiguous/.
  unionl             :: m a -> m a -> m a

  -- | Right biased union.
  --
  --   /Axioms:/
  --
  -- * @unionr = unionWith (\\x y -> y)@
  --
  --   This function is /unambiguous/.
  unionr             :: m a -> m a -> m a

  -- | Same as 'union', but with a combining function to resolve duplicates.
  --
  --   This function is /unambiguous/.
  unionWith          :: (a -> a -> a) -> m a -> m a -> m a

  -- | Same as 'unionSeq', but with a combining function to resolve duplicates.
  --
  --   This function is /unambiguous/.
  unionSeqWith       :: Sequence seq => (a -> a -> a) -> seq (m a) -> m a

  -- | Compute the intersection of two finite maps.  The resulting finite map
  --   will contain bindings where the keys are the set intersection of the
  --   keys in the argument finite maps.  The combining function computes
  --   the value of the element given the bound elements from the argument
  --   finite maps.
  --
  --   This function is /unambiguous/.
  intersectionWith   :: (a -> b -> c) -> m a -> m b -> m c

  -- | Computes the difference of two finite maps; that is, all bindings
  --   in the first finite map whose keys to not appear in the second.
  --
  --   This function is always /unambiguous/.
  difference         :: m a -> m b -> m a

  -- | Test whether the set of keys in the first finite map is a proper subset
  --   of the set of keys of the second; that is, every key present in
  --   the first finite map is also a member of the second finite map AND
  --   there exists some key in the second finite map which is not present
  --   in the first.
  --
  --   This function is always /unambiguous/.
  properSubset       :: m a -> m b -> Bool

  -- | Test whether the set of keys in the first finite map is a subset of
  --   the set of keys of the second; that is, if every key present in the first
  --   finite map is also present in the second.
  --
  --   This function is always /unambiguous/.
  subset             :: m a -> m b -> Bool

  -- | Test whether the first map is a submap of the second map given a comparison
  --   function on elements; that is, if every key present in the first map is also
  --   present in the second map and the comparison function returns true when applied
  --   two the bound elements.
  --
  --   This function is always /unambiguous/.
  submapBy           :: (a -> a -> Bool) -> m a -> m a -> Bool

  -- | Test whether the first map is a proper submap of the second map given a comparison
  --   function on elements; that is, if every key present in the first map is also
  --   present in the second map and the comparison function returns true when applied
  --   two the bound elements AND there exiss some key in the second finite map which
  --   is not present in the first.
  --
  --   This function is always /unambiguous/.
  properSubmapBy     :: (a -> a -> Bool) -> m a -> m a -> Bool

  -- | Test whether the first map is the \"same\" map as the second map given a comparison
  --   function on elements; that is, if the first and second maps have the same set of keys
  --   and the comparison function returns true when applied to corresponding elements.
  --
  --   This function is always /unambiguous/.
  sameMapBy          :: (a -> a -> Bool) -> m a -> m a -> Bool

-- | Finite maps where the keys additionally have an ordering relation.
--   This class introduces no new methods.
class (OrdAssocX m k, FiniteMapX m k) => OrdFiniteMapX m k | m -> k

-- | Associative collections where the keys are observable.
class AssocX m k => Assoc m k | m -> k where
  -- | Extract the bindings of an associative collection into a
  --   sequence. The bindings are emitted in an unspecified order.
  --
  --   This function is /ambiguous/ with respect to the sequence order
  --   iff the associative collection contains more than one binding.
  --   Furthermore, it is /ambiguous/ with respect to the actual key
  --   returned, unless the @Eq@ instance on keys corresponds to
  --   indistinguisability.
  toSeq             :: Sequence seq => m a -> seq (k,a)

  -- | Extract the keys of an associative collection into a sequence.
  --   The keys are emitted in an unspecified order.  For finite relations,
  --   keys which appear multiple times in the relation will appear as many
  --   times in the extracted sequence.
  --
  --   This function is /ambiguous/ with respect to the sequence order
  --   iff the associative collection contains more than one binding.
  --   Furthermore, it is /ambiguous/ with respect to the actual key
  --   returned, unless the @Eq@ instance on keys corresponds to
  --   indistinguisability.
  keys              :: Sequence seq => m a -> seq k

  -- | Apply a function to every element in an associative collection.  The
  --   mapped function additionally takes the value of the key.
  --
  --   This function is /ambiguous/ with respect to the actual keys
  --   observed, unless the @Eq@ instance on keys corresponds to
  --   indistinguisability.
  mapWithKey        :: (k -> a -> b) -> m a -> m b

  -- | Combine all the elements in the associative collection, given a combining
  --   function and an initial value.  The elements are processed in an
  --   unspecified order.  The combining function additionally takes the
  --   value of the key.
  --
  --   @foldWithKey f@ is /unambiguous/ iff @f@ is fold-commutative and
  --   the @Eq@ instance on keys corresponds to indistinguisability.
  foldWithKey       :: (k -> a -> b -> b) -> b -> m a -> b

  -- | A strict variant of 'foldWithKey'.
  --
  --   @foldWithKey' f@ is /unambiguous/ iff @f@ is fold-commutative and
  --   the @Eq@ instance on keys corresponds to indistinguisability.
  foldWithKey'      :: (k -> a -> b -> b) -> b -> m a -> b

  -- | Extract all bindings from an associative collection which satisfy the
  --   given predicate.
  --
  --   This function is /ambiguous/ with respect to the actual keys
  --   observed, unless the @Eq@ instance on keys corresponds to
  --   indistinguisability.
  filterWithKey     :: (k -> a -> Bool) -> m a -> m a

  -- | Split an associative collection into two sub-collections containing those
  --   bindings which satisfy the given predicate and those which do not.
  --
  --   This function is /ambiguous/ with respect to the actual keys
  --   observed, unless the @Eq@ instance on keys corresponds to
  --   indistinguisability.
  partitionWithKey  :: (k -> a -> Bool) -> m a -> (m a, m a)

-- | An associative collection with observable keys where the keys additionally
--   have an ordering relation.

class (Assoc m k, OrdAssocX m k) => OrdAssoc m k | m -> k where
  -- | Delete the binding with the minimum key from an associative
  --   collection and return the key, the element and the remaining
  --   associative collection.  Calls 'fail' if the associative collection
  --   is empty.  Which binding is chosen if there are multiple minimum keys
  --   is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if more than one
  --   minimum key exists in the relation.  Furthermore, it is /ambiguous/
  --   with respect to the actual key observed unless the @Eq@ instance on
  --   keys corresponds to indistinguisability.
  minViewWithKey  :: (Fail.MonadFail rm) => m a -> rm ((k, a), m a)

  -- | Find the binding with the minimum key in an associative collection and
  --   return the key and the element.  Signals an error if the associative
  --   collection is empty.  Which binding is chosen if there are multiple
  --   minimum keys is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if more than one
  --   minimum key exists in the relation.  Furthermore, it is /ambiguous/
  --   with respect to the actual key observed unless the @Eq@ instance on
  --   keys corresponds to indistinguisability.
  minElemWithKey  :: m a -> (k,a)

  -- | Delete the binding with the maximum key from an associative
  --   collection and return the key, the element and the remaining
  --   associative collection.  Calls 'fail' if the associative collection
  --   is empty.  Which binding is chosen if there are multiple maximum keys
  --   is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if more than one
  --   maximum key exists in the relation.  Furthermore, it is /ambiguous/
  --   with respect to the actual key observed unless the @Eq@ instance on
  --   keys corresponds to indistinguisability.
  maxViewWithKey  :: (Fail.MonadFail rm) => m a -> rm ((k, a), m a)

  -- | Find the binding with the maximum key in an associative collection and
  --   return the key and the element.  Signals an error if the associative
  --   collection is empty.  Which binding is chosen if there are multiple
  --   maximum keys is unspecified.
  --
  --   This function is /ambiguous/ at finite relation types if more than one
  --   maximum key exists in the relation.  Furthermore, it is /ambiguous/
  --   with respect to the actual key observed unless the @Eq@ instance on
  --   keys corresponds to indistinguisability.
  maxElemWithKey  :: m a -> (k,a)

  -- | Fold over all bindings in an associative collection in non-decreasing
  --   order by key with right associativity, given a combining function
  --   and an initial value.  For finite maps, the order is increasing.
  --
  --   @foldrWithKey f@ is /ambiguous/ at finite relation types if
  --   the relation contains more than one equivalent key and
  --   @f@ is not fold-commutative OR if the @Eq@ instance on keys
  --   does not correspond to indistingusihability.
  foldrWithKey    :: (k -> a -> b -> b) -> b -> m a -> b

  -- | A strict variant of 'foldrWithKey'.
  --
  --   @foldrWithKey' f@ is /ambiguous/ at finite relation types if
  --   the relation contains more than one equivalent key and
  --   @f@ is not fold-commutative OR if the @Eq@ instance on keys
  --   does not correspond to indistingusihability.  Otherwise it
  --   is /unambiguous/.
  foldrWithKey'   :: (k -> a -> b -> b) -> b -> m a -> b

  -- | Fold over all bindings in an associative collection in non-decreasing
  --   order by key with left associativity, given a combining function
  --   and an initial value.  For finite maps, the order is increasing.
  --
  --   @foldlWithKey f@ is /ambiguous/ at finite relation types if
  --   the relation contains more than one equivalent key and
  --   @f@ is not fold-commutative OR if the @Eq@ instance on keys
  --   does not correspond to indistingusihability. Otherwise it
  --   is /unambiguous/.
  foldlWithKey    :: (b -> k -> a -> b) -> b -> m a -> b

  -- | A strict variant of 'foldlWithKey'.
  --
  --   @foldlWithKey' f@ is /ambiguous/ at finite relation types if
  --   the relation contains more than one equivalent key and
  --   @f@ is not fold-commutative OR if the @Eq@ instance on keys
  --   does not correspond to indistinguishability.  Otherwise it
  --   is /unambiguous/.
  foldlWithKey'   :: (b -> k -> a -> b) -> b -> m a -> b

  -- | Extract the bindings of an associative collection into a sequence, where
  --   the bindings are in non-decreasing order by key.  For finite maps, this
  --   is increasing order.
  --
  --   This function is /ambiguous/ at finite relation types if the relation
  --   contains more than one equivalent key, or if the @Eq@ instance on
  --   keys does not correspond to indistinguishability.
  toOrdSeq        :: Sequence seq => m a -> seq (k,a)

-- | Finite maps with observable keys.
class (Assoc m k, FiniteMapX m k) => FiniteMap m k | m -> k where
  -- | Same as 'union', but with a combining function to resolve duplicates.
  --   The combining function additionally takes the key.  Which key is kept
  --   and passed into the combining function is unspecified.
  --
  --   This function is /unambiguous/ provided that the @Eq@ instance on keys
  --   corresponds to indistinguishability.
  unionWithKey      :: (k -> a -> a -> a) -> m a -> m a -> m a

  -- | Same as 'unionSeq', but with a combining function to resolve duplicates.
  --   The combining function additionally takes the key.  Which key is
  --   kept and passed into the combining function is unspecified.
  --
  --   This function is /unambiguous/ provided that the @Eq@ instance on keys
  --   corresponds to indistinguishability.
  unionSeqWithKey   :: Sequence seq => (k -> a -> a -> a) -> seq (m a) -> m a

  -- | Same as 'intersectionWith', except that the combining function
  --   additionally takes the key value for each binding.  Which key is
  --   kept and passed into the combining function is unspecified.
  --
  --   This function is /unambiguous/ provided the @Eq@ instance on keys
  --   corresponds to indistinguishability.
  intersectionWithKey  :: (k -> a -> b -> c) -> m a -> m b -> m c

-- | Finite maps with observable keys where the keys additionally
--   have an ordering relation.  This class introduces no new methods.
class (OrdAssoc m k, FiniteMap m k) => OrdFiniteMap m k | m -> k


-- specialize sequence operations to lists

fromList          :: AssocX m k => [(k,a)] -> m a
insertList        :: AssocX m k => [(k,a)] -> m a -> m a
unionList         :: AssocX m k => [m a] -> m a
deleteList        :: AssocX m k => [k] -> m a -> m a
lookupList        :: AssocX m k => k -> m a -> [a]
elementsList      :: AssocX m k => m a -> [a]
unsafeFromOrdList :: OrdAssocX m k => [(k,a)] -> m a
fromListWith      :: FiniteMapX m k => (a -> a -> a) -> [(k,a)] -> m a
fromListWithKey   :: FiniteMapX m k => (k -> a -> a -> a) -> [(k,a)] -> m a
insertListWith    :: FiniteMapX m k =>
                         (a -> a -> a) -> [(k,a)] -> m a -> m a
insertListWithKey :: FiniteMapX m k =>
                         (k -> a -> a -> a) -> [(k,a)] -> m a -> m a
unionListWith     :: FiniteMapX m k => (a -> a -> a) -> [m a] -> m a
toList            :: Assoc m k => m a -> [(k,a)]
keysList          :: Assoc m k => m a -> [k]
toOrdList         :: OrdAssoc m k => m a -> [(k,a)]
unionListWithKey  :: FiniteMap m k => (k -> a -> a -> a) -> [m a] -> m a

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
lookupList = lookupAll
elementsList = elements
unsafeFromOrdList = unsafeFromOrdSeq
fromListWith = fromSeqWith
fromListWithKey = fromSeqWithKey
insertListWith = insertSeqWith
insertListWithKey = insertSeqWithKey
unionListWith = unionSeqWith
toList = toSeq
keysList = keys
toOrdList = toOrdSeq
unionListWithKey = unionSeqWithKey
