-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | The /associative collection/ abstraction includes finite maps, finite
--   relations, and priority queues where the priority is separate from the
--   element.  Associative collections are defined in Edison as a set of eight
--   classes, organized in the hierarchy shown here FIXME.  Notice that this
--   hierarchy mirrors the hierarchy for collections, but with the addition
--   of 'Functor' as a superclass of every associative collection.

module Data.Edison.Assoc ( -- associative collections
    -- * Non-observable classes
    AssocX(..),
    OrdAssocX(..),
    FiniteMapX(..),
    OrdFiniteMapX,

    -- * Observable classes
    Assoc(..),
    OrdAssoc(..),
    FiniteMap(..),
    OrdFiniteMap,

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

import Data.Edison.Prelude

import Data.Edison.Seq(Sequence)
import Data.Edison.Seq.ListSeq()

class (Eq k,Functor m) => AssocX m k | m -> k where

  -- | The empty associative collection.
  empty          :: m a

  -- | Create an associative collection with a single binding.
  single         :: k -> a -> m a

  -- | Create an associative collection from a  list of bindings. Which element
  --   and key are kept in the case of duplicates is unspecified.
  fromSeq        :: Sequence seq => seq (k,a) -> m a

  -- | Add a binding to an associative collection.  For finite maps, 'insert'
  --   keeps the new element in the case of duplicate keys.  Which key is kept
  --   is unspecified.
  insert         :: k -> a -> m a -> m a

  -- | Add a sequence of bindings to a collection.  Which key and which element
  --   are kept in the case of duplicates is unspecified.
  insertSeq      :: Sequence seq => seq (k,a) -> m a -> m a

  -- | Merge two associative collections.  Which element and which key to keep
  --   in the case of duplicate keys is unspecified.
  union          :: m a -> m a -> m a

  -- | Merge a sequence of associative collections.  Which element and which key
  --   to keep in the case of duplicate keys is unspecified.
  unionSeq       :: Sequence seq => seq (m a) -> m a

  -- | Delete one binding with the given key, or leave the associative collection
  --   unchanged if it does not contain the key.  For bag-like associative
  --   collections, it is unspecified which binding will be removed.
  delete         :: k -> m a -> m a

  -- | Delete all bindings with the given key, or leave the associative collection
  --   unchanged if it does not contain the key.
  deleteAll      :: k -> m a -> m a

  -- | Delete a single occurance of each of the given keys from an associative
  --   collection.  For bag-like associtive collections, it is unspecified which
  --   binding will be removed.
  deleteSeq      :: Sequence seq => seq k -> m a -> m a

  -- | Test whether the associative collection is empty.
  --
  --   /Axioms:/
  --
  -- * @null m = (size m == 0)@
  null           :: m a -> Bool

  -- | Return the number of bindings in the associative collection.
  size           :: m a -> Int

  -- | Test whether the given key is bound in the associative collection.
  member         :: m a -> k -> Bool
 
  -- | Returns the number of bindings with the given key.  For set-like 
  --   associative collections, this will always return 0 or 1.
  count          :: m a -> k -> Int

  -- | Find the element associated with the given key.  Signals an error if
  --   the given key is not bound.  If more than one element is bound by the
  --   given key, it is unspecified which is returned.
  lookup         :: m a -> k -> a
 
  -- | Find the element associated with the given key.  Calls 'fail' if the
  --   given key is not bound.  If more than one element is bound by the given
  --   key, it is unspecified which is returned.
  lookupM        :: (Monad rm) => m a -> k -> rm a

  -- | Return all elements bound by the given key in an unspecified order.
  lookupAll      :: Sequence seq => m a -> k -> seq a

  -- | Return the element associated with the given key.  If no such element
  --   is found, return the default.
  lookupWithDefault  :: a    -- ^ default element
                     -> m a  -- ^ the associative collection
                     -> k    -- ^ the key to look up
                     -> a

  -- | Change a single binding for the given key by applying a function to its
  --   element.  If the key binds more than one element, it is unspecified which
  --   will be modified.
  adjust         :: (a -> a) -> k -> m a -> m a

  -- | Change all bindings for the given key by applying a function to its
  --   elements.
  adjustAll      :: (a -> a) -> k -> m a -> m a

  -- | Apply a function to the elements of every binding in the associative
  --   collection.  Defaults to 'fmap' from the Functor instance.
  map            :: (a -> b) -> m a -> m b
  map = fmap

  -- | Combine all the elements in the associative collection, given a combining
  --   function and an initial value.  The elements are processed in an
  --   unspecified order.  /Note/ that 'fold' ignores the keys.
  fold           :: (a -> b -> b) -> b -> m a -> b

  -- | Combine all the elements in a non-empty associative collection using the
  --   given combining function.  Signals an error if the associative collection
  --   is empty.  The elements are processed in an unspecified order.  An
  --   implementation may choose to process the elements linearly or in a
  --   balanced fashion (like 'reduce1' on sequences).  /Note/ that 'fold1'
  --   ignores the keys.
  fold1          :: (a -> a -> a) -> m a -> a

  -- | Extract all bindings whose elements satify the given predicate.
  filter         :: (a -> Bool) -> m a -> m a
 
  -- | Split an associative collection into those bindings which satisfy the
  --   given predicate, and those which do not.
  partition      :: (a -> Bool) -> m a -> (m a, m a)
  
  -- | Returns all the elements in an associative collection, in an unspecified
  --   order.
  elements       :: Sequence seq => m a -> seq a

  -- | Returns the name of the module implementing this associative collection.
  instanceName   :: m a -> String

-- | An associative collection where the keys additionally have an ordering
--   relation.
class (AssocX m k, Ord k) => OrdAssocX m k | m -> k where
  -- | Remove the binding with the minimum key, and return its element together
  --   with the remaining associative collection.  Calls 'fail' if the 
  --   associative collection is empty.  Which binding is removed if there
  --   is more than one minimum is unspecified.
  minView            :: (Monad rm) => m a -> rm (a, m a)

  -- | Find the binding with the minimum key and return its element. Signals
  --   and error if the associative collection is empty.  Which element is chosen
  --   if there is more than one minimum is unspecified.
  minElem            :: m a -> a

  -- | Remove the binding with the minimum key and return the remaining
  --   associative collection, or return empty if it is already empty.
  deleteMin          :: m a -> m a

  -- | Insert a binding into an associative collection with the precondition
  --   that the given key is @\<=@ any existing keys already in the collection.
  --   For finite maps, this precondition is strengthened to @\<@.
  unsafeInsertMin    :: a -> a -> m a -> m a

  -- | Remove the biniding with the maximum key, and return its element together
  --   with the remaining associative collection.  Calls 'fail' if the
  --   associative collection is empty.  Which binding is removed if there
  --   is more than one maxmimum is unspecified.
  maxView            :: (Monad rm) => m a -> rm (m a, a)

  -- | Find the binding with the maximum key and return its element.  Signals
  --   an error if the associative collection is empty.  Which element is chosen
  --   if there is more than one maximum is unspecified.
  maxElem            :: m a -> a

  -- | Remove the binding with the maximum key and return the remaining
  --   associative collection, or return empty if it is already empty.
  deleteMax          :: m a -> m a

  -- | Insert a binding into an associative collection with the precondition
  --   that the given key is @>=@ any existing keys alread in the collection.
  --   For finite maps, this precondition is strengthened to @>@.
  unsafeInsertMax    :: m a -> k -> a -> m a

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with right associativity.  For finite maps, the order
  --   is increasing.
  foldr              :: (a -> b -> b) -> b -> m a -> b

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with left associativity.  For finite maps, the order
  --   is increasing.
  foldl              :: (b -> a -> b) -> b -> m a -> b

  -- | Fold across the elements of an associative collection in non-decreasing
  --   order by key with right associativity.  Signals an error if the
  --   associative collection is empty.  For finite maps, the order is
  --   increasing.
  foldr1             :: (a -> a -> a) -> m a -> m a

  -- | Fold across the elements of an associative collction in non-decreasing
  --   order by key with left associativity.  Signals an error if the 
  --   associative collction is empty.  For finite maps, the order is
  --   increasing.
  foldl1             :: (a -> a -> a) -> m a -> m a

  -- | Convert a sequence of bindings into an associative collection with the
  --   precondition that the sequence is sorted into non-decreasing order by
  --   key.  For finite maps, this precondition is strengthened to increasing
  --   order.
  unsafeFromOrdSeq   :: Sequence seq => seq (k,a) -> m a

  -- | Merge two associative collections with the precondition that every key
  --   in the first associative collection is @\<=@ every key in the second
  --   associative collction.  For finite maps, this precondition is
  --   strengthened to @\<@.
  unsafeAppend       :: m a -> m a -> m a

  -- | Extract all bindings whose keys are @\<@ the given key.
  filterLT           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @\<=@ the given key.
  filterLE           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @>@ the given key.
  filterGT           :: k -> m a -> m a

  -- | Extract all bindings whose keys are @>=@ the given key.
  filterGE           :: k -> m a -> m a

  -- | Split an associative collection into two subcollections, containing
  --   those bindings whose keys are @\<@ the given key and those which are @>=@.  
  partitionLT_GE     :: k -> m a -> (m a, m a)

  -- | Split an associative collection into two subcollections, containing
  --   those bindings whose keys are @\<=@ the given key and those which are @>@.
  partitionLE_GT     :: k -> m a -> (m a, m a)

  -- | Split an associative collection into two subcollections, containing
  --   those bindings whose keys are @\<@ the given key and those which are @>@.
  --   All bindings with keys equal to the given key are discarded.
  partitionLT_GT     :: k -> m a -> (m a, m a)

-- | An associative collection where the keys form a set; that is, each key
--   appears in the associative collection at most once.
--
--   /WARNING: each of the following \"with\" functions is unsafe/.
--   The passed in combining functions are used to choose which element is kept
--   in the case of duplicates.  They are required to satify the precondition
--   that, given two equal elements, they return a third element equal to the
--   other two.  Usually, the combining function just returns its first or second
--   argument, but it can combine elements in non-trivial ways.
--
--   The combining function should usually be associative.  In any case, the
--   elements will be combined from left-to-right ordered by key, but with
--   an unspecified associativity.

class AssocX m k => FiniteMapX m k | m -> k where

  -- | Same as 'fromSeq', but with a combining function to resolve duplicates.
  fromSeqWith        :: Sequence seq => (a -> a -> a) -> seq (k,a) -> m a

  -- | Same as 'fromSeq', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  fromSeqWithKey     :: Sequence seq => (k -> a -> a -> a) -> seq (k,a) -> m a

  -- | Same as 'insert', but with a combining function to resolve duplicates.
  insertWith         :: (a -> a -> a) -> k -> a -> m a -> m a

  -- | Same as 'insert', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  insertWithKey      :: (k -> a -> a -> a) -> k -> a -> m a -> m a

  -- | Same as 'insertSeq', but with a combining function to resolve duplicates.
  insertSeqWith      :: Sequence seq => 
                           (a -> a -> a) -> seq (k,a) -> m a -> m a

  -- | Same as 'insertSeq', but with a combining function to resolve duplicates;
  --   the combining function takes the key in addition to the two elements.
  insertSeqWithKey   :: Sequence seq => 
                           (k -> a -> a -> a) -> seq (k,a) -> m a -> m a

  -- | Left biased union.
  --
  --   /Axioms:/
  --
  -- * @unionl = unionwith (\\x y -> x)@
  unionl             :: m a -> m a -> m a

  -- | Right biased union.
  --
  --   /Axioms:/
  --
  -- * @unionr = unionWith (\\x y -> y)@
  unionr             :: m a -> m a -> m a
 
  -- | Same as 'union', but with a combining function to resolve duplicates.
  unionWith          :: (a -> a -> a) -> m a -> m a -> m a

  -- | Same as 'unionSeq', but with a combining function to resolve duplicates.
  unionSeqWith       :: Sequence seq => (a -> a -> a) -> seq (m a) -> m a

  -- | Same as 'intersect', but with a combining function to resolve duplicates.
  intersectWith      :: (a -> b -> c) -> m a -> m b -> m c

  -- | Computes the difference of two finite maps; that is, all bindings
  --   in the first finite map whose keys to not appear in the second.
  difference         :: m a -> m b -> m a

  -- | Test whether the set of keys in the first finite map is a proper subset
  --   of the set of keys of the second; that is, every key present in
  --   the first finite map is also a member of the second finite map AND
  --   there exists some key in the second finite map which is not present
  --   in the first.
  subset             :: m a -> m b -> Bool    

  -- | Test whether the set of keys in nthe first finite mape is a subset of
  --   the set of keys of the second; that is, if every key present in the first
  --   finite map is also present in the second.
  subsetEq           :: m a -> m b -> Bool    

-- | Finite maps where the keys additionally have an ordering relation.
--   This class introduces no new methods.
class (OrdAssocX m k, FiniteMapX m k) => OrdFiniteMapX m k | m -> k

-- | Associative collections where the keys are observable.
class AssocX m k => Assoc m k | m -> k where
  -- | Extract the bindings of an associative collection into a
  --   sequence. The bindings are emitted in an unspecified order.
  toSeq             :: Sequence seq => m a -> seq (k,a)

  -- | Extract the keys of an associative collection into a sequence.
  --   The keys are emitted in an unspecified order.
  keys              :: Sequence seq => m a -> seq k
  
  -- | Apply a function to every element in an associative collection.  The
  --   mapped function additionally takes the value of the key.
  mapWithKey        :: (k -> a -> b) -> m a -> m b

  -- | Combine all the elements in the associative collection, given a combining
  --   function and an initial value.  The elements are processed in an
  --   unspecified order.  The combining function additionally takes the
  --   value of the key.
  foldWithKey       :: (k -> a -> b -> b) -> b -> m a -> b

  -- | Extract all bindings from an associative collection which satisfy the
  --   given predicate.
  filterWithKey     :: (k -> a -> Bool) -> m a -> m a

  -- | Split an associative collection into two subcollections containing those
  --   bindings which satisfy the given predicate and those which do not.
  partitionWithKey  :: (k -> a -> Bool) -> m a -> (m a, m a)

-- | An associative collection with observable keys where the keys additionally
--   have an ordering relation.

class (Assoc m k, OrdAssocX m k) => OrdAssoc m k | m -> k where
  -- | Delete the binding with the minimum key from an associative
  --   collection and return the key, the element and the remaining
  --   associative collection.  Calls 'fail' if the associative collection 
  --   is empty.  Which binding is chosen if there are multiple minimum keys
  --   is unspecified.
  minViewWithKey  :: (Monad rm) => m a -> rm (k, a, m a)

  -- | Find the binding with the minimum key in an associative collection and
  --   return the key and the element.  Signals an error if the associative
  --   collection is empty.  Which binding is chosen if there are multiple
  --   minimum keys is unspecified.
  minElemWithKey  :: m a -> (k,a)

  -- | Delete the binding with the maximum key from an associative
  --   collection and return the key, the element and the remaining
  --   associative collection.  Calls 'fail' if the associative collection 
  --   is empty.  Which binding is chosen if there are multiple maximumkeys
  --   is unspecified.
  maxViewWithKey  :: (Monad rm) => m a -> rm (m a, k, a)

  -- | Find the binding with the maximum key in an associative collection and
  --   return the key and the element.  Signals an error if the associative
  --   collection is empty.  Which binding is chosen if there are multiple
  --   maximum keys is unspecified.
  maxElemWithKey  :: m a -> (k,a)

  -- | Fold over all bindings in an associative collection in non-decreasing
  --   order by key with right associativity, given a combining function
  --   and an initial value.  For finite maps, the order is increasing.
  foldrWithKey    :: (k -> a -> b -> b) -> b -> m a -> b

  -- | Fold over all bindings in an associative collection in non-decreasing
  --   order by key with left associativity, given a combining function
  --   and an initial value.  For finite maps, the order is increasing.
  foldlWithKey    :: (b -> k -> a -> b) -> b -> m a -> b

  -- | Extract the bindings of an associative collection into a sequence, where
  --   the bindings are in non-decreasing order by key.  For finite maps, this
  --   is increasing order.
  toOrdSeq        :: Sequence seq => m a -> seq (k,a)

-- | Finite maps with observable keys.  /WARNING/ see 'FiniteMapX' for
--   notes about the unsafe \"With\" functions.
class (Assoc m k, FiniteMapX m k) => FiniteMap m k | m -> k where
  -- | Same as 'union', but with a combining function to resolve duplicates.
  --   The combining function additionally takes the key.
  unionWithKey      :: (k -> a -> a -> a) -> m a -> m a -> m a

  -- | Same as 'unionSeq', but with a combining function to resolve duplicates.
  --   The combining function additionally takes the key.
  unionSeqWithKey   :: Sequence seq => (k -> a -> a -> a) -> seq (m a) -> m a

  -- | Same as 'intersect', but with a combining function to resolve duplicates.
  --   The combining function additionally takes the key.
  intersectWithKey  :: (k -> a -> b -> c) -> m a -> m b -> m c

-- | Finite maps with observable keys where the keys additionally
--   have an ordering relation.  This class introduces no new methods.
class (OrdAssoc m k, FiniteMap m k) => OrdFiniteMap m k | m -> k


-- specialize sequence operations to lists

fromList          :: AssocX m k => [(k,a)] -> m a
insertList        :: AssocX m k => [(k,a)] -> m a -> m a
unionList         :: AssocX m k => [m a] -> m a
deleteList        :: AssocX m k => [k] -> m a -> m a
lookupList        :: AssocX m k => m a -> k -> [a]
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


{-
Leave out until somebody asks for:
witness????
compose????

  nub           :: m a -> m a  -- ???
  nubWith       :: (a -> a -> a) -> m a -> m a
  nubWithKey :: (k -> a -> a -> a) -> m a -> m a

  group         :: m a -> m [a] -- ???
?????  unsafeMapMonotonim :: (a -> a) -> m a -> m a


-- adjustPartial??? (adjustOrDelete???)
-- adjustAll       :: (a -> a) -> k -> m a -> m a
-- unionMap???
-- mapPartial???

  anyViewKey :: (Monad rm) => m a -> rm (k, a, m a)
  anyKeyElem :: m a -> (k,a) -- signals error if collection is empty
  deleteAny :: m a -> m a -- could go in AssocX but no point
    -- anyKeyElem and deleteAny must be consistent
    -- do they need to be consistent with anyView?

-- unionMap???
-- mapPartial???

  deleteAllList :: [k] -> m a -> m a

  disjoint      :: m a -> m b -> Bool

-}

