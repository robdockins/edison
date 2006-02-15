-- Copyright (c) 1998 Chris Okasaki.  
-- See COPYRIGHT file for terms and conditions.

-- | The /collection/ abstraction includes sets, bags and priority queues
--   (heaps).  Collections are defined in Edison as a set of eight classes,
--   organized in the heirarchy shown here FIXME.
--
--   All collections assume at least an equality relation of elements, and
--   may also assume an ordering relation.
--
--   The hierarchy contains a root class 'CollX' together with seven
--   subclasses satifying one or more of three common sub-properties:
--
-- * /Uniqueness/ Each element in the collection is unique (no two
--   elements in the collection are equal).  These subclasses, indicated
--   by the name @Set@, represent sets rather than bags (multi-sets).
--
-- * /Ordering/ The elements have a total ordering and it is possible to
--   process the elements in non-decreasing order. These subclasses,
--   indicates by the @Ord@ prefix, typically represent either priority
--   queues (heaps) or sets\/bags implemented as binary search trees.
--
-- * /Observability/ An observable collection is one in which it is
--   possible to view the elements in a collection.  The @X@ suffix
--   indicates a lack of observability.  This property is discussed is
--   greater detail below.
--
--   Because collections encompass a wide range of abstractions, there is no
--   single name that is suitable for all collection type constructors.
--   However, most modules implementing collections will define a type
--   constructor named either @Bag@, @Set@, or @Heap@.
--
--   /Notes on observability/
--
--   Note that the equality relation defined by the 'Eq' class is not
--   necessarily true equality.  Very often it is merely an equivalance
--   relation, where two equivalant values may be distinguishable by other
--   means.  For example, we might consider two binary trees to be equal
--   if they contain the same elements, even if their shapes are different.
--
--   Because of this phenomenon, implementations of observable collections
--   (ie, collections where it is possible to inspect the elements) are rather
--   constrained.  Such an implementation must retain the actual elements that
--   were inserted.  For example, it is not possible in general to represent an
--   observable bag as a finite map from elements to counts, because even if we
--   know that a given bag contains, say, three elements from some equivalence
--   class, we do not necessarily know /which/ three.
--
--   On the other hand, implementations of /non-observable/ collections have
--   much greater freedom to choose abstract representations of each
--   equivalence class.  For example, representing a bag as a finite map from
--   elements to counts works fine if we never need to know /which/
--   representatives from an equivalence class are actually present.  As
--   another example, consider the 'UniqueHash' class defined in
--   "Data.Edison.Prelude".  If we know that the 'hash' function yields a 
--   unique integer for each equivalance class, then we can represent a
--   collection of hashable elements simply as a collection of integers.  With
--   such a representation, we can still do many useful things like testing for
--   membership; we just can't support functions like 'fold' or 'filter' that
--   require the elements themselves, rather than the hashed values.

module Data.Edison.Coll (
    -- * Non-Observable Classes
    CollX(..),
    OrdCollX(..),
    SetX(..),
    OrdSetX,

    -- * Observable Classes
    Coll(..),
    OrdColl(..),
    Set(..),
    OrdSet,

    -- * Specializations of all the sequence operations to lists
    fromList,
    insertList,
    unionList,
    deleteList,
    unsafeFromOrdList,
    toList,
    lookupList,
    toOrdList,
    fromListWith,
    insertListWith,
    unionListWith,

) where

import Prelude hiding (null,foldr,foldl,foldr1,foldl1,lookup,filter)
import Data.Edison.Prelude
import Data.Edison.Seq(Sequence)
import Data.Edison.Seq.ListSeq()

-- | This is the root class of the collection hierarchy.  However, it
--   is perfectly adequate for many applications that use sets or bags.

class Eq a => CollX c a | c -> a where

  -- | Thhe empty collection
  empty          :: c

  -- | create a singleton collection
  single         :: a -> c

  -- | Convert a sequence to a collection.  For sets, it is unspecified
  --   which element is kept in case of duplicates.
  fromSeq        :: Sequence seq => seq a -> c

  -- | Insert an element into a collection.  For sets, if an equal element
  --   is already in the set, the newly inserted element is kept, and the
  --   old element is discarded.
  insert         :: a -> c -> c

  -- | Insert a sequence of elements into a collection.  For sets,
  --   the behavior with regard to multiple equal elements is unspecified.
  insertSeq      :: Sequence seq => seq a -> c -> c

  -- | Merge two collections.  For sets, it is unspecified which element is
  --   kept in the case of duplicates.
  union          :: c -> c -> c

  -- | Merge a sequence of collections.  For sets, it is unspecified which
  --   element is kept in the case of duplicates.
  unionSeq       :: Sequence seq => seq c -> c

  -- | Delete a single occurance of the given element from a collection.
  --   For bags, it is unspecified which element will be deleted.
  delete         :: a -> c -> c

  -- | Delete all occurances of an element from a collection.  For sets
  --   this operation is identical to 'delete'.
  deleteAll      :: a -> c -> c

  -- | Delete a single occurance of each of the given elements from
  --   a collection.  For bags, there may be multiple occurances of a
  --   given element in the collection, in which case it is unspecified
  --   which is deleted.
  deleteSeq      :: Sequence seq => seq a -> c -> c

  -- | Test whether the collection is empty.
  --
  -- /Axioms:/
  --
  -- * @null xs = (size xs == 0)@
  null           :: c -> Bool

  -- | Return the number of elements in the collection.
  size           :: c -> Int

  -- | Test whether the given element is in the collection.
  --
  -- /Axioms:/
  --
  -- * @member x xs = (count x xs > 0)@
  member         :: a -> c -> Bool

  -- | Count how many copies of the given element are in the collection.
  --   For sets, this will always return 0 or 1.
  count          :: a -> c -> Int

  -- | The name of the module implementing @c@
  instanceName   :: c -> String


-- | Collections for which the elements have an ordering relation.

class (CollX c a, Ord a) => OrdCollX c a | c -> a where

  -- | Delete the minimum element from the collection.  If there is more
  --   than one minimum, it is unspecified which is deleted.
  deleteMin          :: c -> c

  -- | Delete the maximum element from the collection.  If there is more
  --   than one maximum, it is unspecified which is deleted.
  deleteMax          :: c -> c

  -- | Insert an element into a collection which is guaranteed to be
  --   @\<=@ any existing elements in the collection.  For sets, the
  --   precondition is strengthened to @\<@.
  unsafeInsertMin    :: a -> c -> c

  -- | Insert an element into a collection which is guaranteed to be
  --   @>=@ any existing elements in the collection.  For sets, the 
  --   precondition is strenghtened to @>@.
  unsafeInsertMax    :: a -> c -> c

  -- | Convert a sequence in non-decreasing order into a collection.
  --   For sets, the sequence must be in increasing order.
  unsafeFromOrdSeq   :: Sequence seq => seq a -> c

  -- | Union two collections where every element in the first
  --   collection is @\<=@ every element in the second collection.
  --   For sets, this precondition is strengthened to @\<@.
  unsafeAppend       :: c -> c -> c

  -- | Extract the subcollection of elements @\<@ the given element.
  --
  -- /Axioms:/
  --
  -- * @filterLT x xs = filter (\< x) xs@
  filterLT           :: a -> c -> c

  -- | Extract the subcollection of elements @\<=@ the given element.
  --
  -- /Axioms:/
  --
  -- * @filterLE x xs = filter (\<= x) xs@
  filterLE           :: a -> c -> c

  -- | Extract the subcollection of elements @>@ the given element.
  --
  -- /Axioms:/
  --
  -- * @filterGT x xs = filter (> x) xs@
  filterGT           :: a -> c -> c

  -- | Extract the subcollection of elements @>=@ the given element.
  --
  -- /Axioms:/
  --
  -- * @filterGE x xs = filter (>= x) xs@
  filterGE           :: a -> c -> c

  -- | Split a collection into those elements @\<@ a given elemnt and
  --   those @>=@.
  --
  -- /Axioms:/
  --
  -- * @partitionLT_GE xs = partition (\<) xs@
  partitionLT_GE     :: a -> c -> (c, c)

  -- | Split a collection into those elements @\<=@ a given element and
  --   those @>@.
  --
  -- /Axioms:/
  --
  -- * @partitionLE_GT xs = partition (\<=) xs@
  partitionLE_GT     :: a -> c -> (c, c)

  -- | Split a collection into those elements @\<@ a given element and
  --   those @>@.  All elements equal to the given element are discarded.
  --
  -- /Axioms:/
  --
  -- *@partitionLT_GT x xs = (filterLT x xs,filterGT x xs)@
  partitionLT_GT     :: a -> c -> (c, c)


-- | A collection where the set property is maintained; that is, a set
--   contains at most one element of the equivalance class formed by the
--   'Eq' instance on the elements.
class CollX c a => SetX c a | c -> a where

  -- | Computes the intersection of two sets.  It is unspecified which 
  --   element when equal elements appear in each set.
  intersect   :: c -> c -> c

  -- | Computes the difference of two sets; that is, all elements in
  --   the first set which are not in the second set.
  difference  :: c -> c -> c

  -- | Test whether the first set is a proper subset of the second set;
  --   that is, if every element in the first set is also a member of the
  --   second set AND there exists some element in the second set which
  --   is not present in the first.
  subset      :: c -> c -> Bool    

  -- | Test whether the first set is a subset of the second set; that is, if
  --   every element in the first set is also a member of the second set.
  subsetEq    :: c -> c -> Bool

    -- test whether the first set is a proper subset of the second,
    -- or whether it is a (possibly improper) subset.

-- | Sets where the elements also have an ordering relation.
--   This class contains no methods; it is only an abbreviation for
--   the context @(OrdCollX c a,SetX c a)@.

class (OrdCollX c a, SetX c a) => OrdSetX c a | c -> a
  -- no methods

-- | Collections with observable elements.  See the module documentation for
--   comments on observability.

class CollX c a => Coll c a | c -> a where

  -- | List the elements of the collection in an unspecified order.
  toSeq      :: Sequence seq => c -> seq a

  -- | Lookup one element equal to the given element.  If no elements
  --   exist in the collection equal to the given element, an error is
  --   signaled.  If multiple copies of the given element exist in the
  --   collection, it is unspecified which is returned.
  lookup     :: a -> c -> a

  -- | Lookup one element equal to the given element.  If no elements
  --   exist in the collection equal to the given element, 'fail' is called.
  --   If multiple copies of the given element exist in the collection, it
  --   is unspecified which is returned.
  lookupM    :: (Monad m) => a -> c -> m a

  -- | Return a sequence containing all elements in the collection equal to
  --   the given element in an unspecified order.
  lookupAll  :: Sequence seq => a -> c -> seq a

  -- | Lookup one element equal to the (second) given element in the collection.
  --   If no elements exist in the collection equal to the given element, then
  --   the default element is returned.
  lookupWithDefault  :: a -- ^ deault element
                     -> a -- ^ item to lookup
                     -> c -- ^ collection
                     -> a

  -- | Fold over all the elements in a collection in an unspecified order.
  fold       :: (a -> b -> b) -> b -> c -> b

  -- | Fold over all the elements in a collection in an unspecified order.
  --   An error is signaled if the collection is empty.
  fold1      :: (a -> a -> a) -> c -> a

  -- | Remove all elements not satisfying the predicate.
  filter     :: (a -> Bool) -> c -> c

  -- | Returns two collections, the first containing all the elements
  --   satisfying the predicate, and the second containing all the
  --   elements not satisfying the predicate.
  partition  :: (a -> Bool) -> c -> (c, c)



-- | Collections with observable elements where the elements additionally
--   have an ordering relation.  See the module documentation for comments
--   on observability.

class (Coll c a, OrdCollX c a) => OrdColl c a | c -> a where

  -- | Return the minimum element in the collection, together with
  --   the collection without that element.  If there are multiple
  --   copies of the minimum element, it is unspecified which is chosen.
  --   /Note/ that 'minView', 'minElem', and 'deleteMin' may make different
  --   choices.
  minView    :: (Monad m) => c -> m (a, c)

  -- | Return the minimum element in the collection.  If there are multipl
  --   copies of the minimum element, it is unspecified which is chosen.
  --   /Note/ that 'minView', 'minElem', and 'deleteMin' may make different
  --   choices.
  minElem    :: c -> a

  -- | Return the maximum element in the collection, together with 
  --   the collection without that element.  If there are multiple
  --   copies of the maximum element, it is unspecified which is chosen.
  --   /Note/ that 'maxView', 'maxElem' and 'deleteMax' may make different
  --   choices.
  maxView    :: (Monad m) => c -> m (a, c)

  -- | Return the maximum element in the collection.  If there are multiple
  --   copies of the maximum element, it is unspecified which is chosen.
  --   /Note/ that 'maxView', 'maxElem' and 'deleteMax' may make different
  --   choices.
  maxElem    :: c -> a

  -- | Fold arcoss the elements in non-decreasing order with right
  --   associativity. (For sets, this will always be increasing order)
  foldr      :: (a -> b -> b) -> b -> c -> b

  -- | Fold arcoss the elements in non-decreasing order with left
  --   associativity. (For sets, this will always be increasing order)
  foldl      :: (b -> a -> b) -> b -> c -> b

  -- | Fold across the elements in non-decreasing order with right
  --   associativity, or signal an error if the collection is empty.
  --   (For sets, this will always be increasing order)
  foldr1     :: (a -> a -> a) -> c -> a

  -- | Fold across the elements in non-decreasing order with left
  --   associativity, or signal an error if the collection is empty.
  --   (For sets, this will always be increasing order)
  foldl1     :: (a -> a -> a) -> c -> a

  -- | List the elements in non-decreasing order. (For sets, this will always
  --   be increasing order)
  toOrdSeq   :: Sequence seq => c -> seq a


-- | Collections with observable elements where the set property is maintained;
--   that is, a set contains at most one element of the equivalance class
--   formed by the 'Eq' instance on the elements.
--
--   /WARNING: Each of the following \"With\" functions is unsafe./ 
--   The passed in combining functions are used to choose which element is kept
--   in the case of duplicates. They are required to satisfy the precondition
--   that, given two equal elements, they return a third element equal to the
--   other two.  Usually, the combining function just returns its first or
--   second argument, but it can combine elements in non-trivial ways.
--
--   The combining function should usually be associative.  In any case, the
--   elements will be combined from left-to-right, but with an unspecified
--   associativity.
--
--   For example, if @x == y == z@,
--   then @fromSeqWith (+) [x,y,z]@ equals either
--     @single (x + (y + z))@
--   or
--     @single ((x + y) + z)@

class (Coll c a, SetX c a) => Set c a | c -> a where

  -- | Same as 'fromSeq' but with a combining function to resolve duplicates.  
  fromSeqWith     :: Sequence seq => (a -> a -> a) -> seq a -> c

  -- | Same as 'insert' but with a combining function to resolve duplicates.
  insertWith      :: (a -> a -> a) -> a -> c -> c

  -- | Same as 'insertSeq' but with a combining function to resolve duplicates.
  insertSeqWith   :: Sequence seq => (a -> a -> a) -> seq a -> c -> c

  -- | Left biased union.
  --
  --   /Axioms:/
  --
  -- * @unionl = unionWith (\\x y -> x)@
  unionl          :: c -> c -> c
 
  -- | Right biased union.
  --
  --   /Axioms:/
  --
  -- * @unionr = unionWith (\\x y -> y)@
  unionr          :: c -> c -> c


  -- | Same as 'union', but with a combining function to resolve duplicates.    
  unionWith       :: (a -> a -> a) -> c -> c -> c

  -- | Same as 'unionSeq', but with a combining function to resolve duplicates.
  unionSeqWith    :: Sequence seq => (a -> a -> a) -> seq (c) -> c

  -- | Same as 'intersect', but with a combining function to resolve duplicates.
  intersectWith   :: (a -> a -> a) -> c -> c -> c

-- | Collections with observable elements where the set property is maintained
--   and where additinally, there is an ordering relation on the elements.
--   This class introduces no new methods, and is simply an abbreviation 
--   for the context:
--
--   @(OrdColl c a,Set c a)@

class (OrdColl c a, Set c a) => OrdSet c a | c -> a
  -- no methods


-- specialize all the sequence operations to lists

fromList          :: CollX c a => [a] -> c
insertList        :: CollX c a => [a] -> c -> c
unionList         :: CollX c a => [c] -> c
deleteList        :: CollX c a => [a] -> c -> c
unsafeFromOrdList :: OrdCollX c a => [a] -> c
toList            :: Coll c a => c -> [a]
lookupList        :: Coll c a => a -> c -> [a]
toOrdList         :: OrdColl c a => c -> [a]
fromListWith      :: Set c a => (a -> a -> a) -> [a] -> c
insertListWith    :: Set c a => (a -> a -> a) -> [a] -> c -> c
unionListWith     :: Set c a => (a -> a -> a) -> [c] -> c

fromList = fromSeq
insertList = insertSeq
unionList = unionSeq
deleteList = deleteSeq
unsafeFromOrdList = unsafeFromOrdSeq
toList = toSeq
lookupList = lookupAll
toOrdList = toOrdSeq
fromListWith = fromSeqWith
insertListWith = insertSeqWith
unionListWith = unionSeqWith

