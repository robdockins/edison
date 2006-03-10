-- |
--   Module      :  Data.Edison
--   Copyright   :  Copyright (c) 2006 Robert Dockins
--   License     :  BSD3; see COPYRIGHT file for terms and conditions
--
--   Maintainer  :  robdockins AT fastmail DOT fm
--   Stability   :  provisional
--   Portability :  non-portable (MPTC and FD)
--
--   Edison is a library of purely functional data structures written by 
--   Chris Okasaki.  It is named after Thomas Alva Edison and for the
--   mnemonic value /ED/i/S/on (/E/fficent /D/ata /S/tructures).
--
--   Edison provides several families of abstractions, each with 
--   multiple implementations.  The main abstractions provided by Edison are:
--
--   * /Sequences/ such as stacks, queues, and dequeues,
--
--   * /Collections/ such as sets, bags and heaps, and
--
--   * /Associative Collections/ such as finite maps and priority queues
--     where the priority and element are distinct.
--
--   /Conventions:/
--
--   Each data structure is implemented as a separate module.  These modules
--   should always be imported @qualified@ to prevent a flood of name clashes,
--   and it is recommended to rename the module using the @as@ keyword to reduce
--   the overhead of qualified names and to make substituting one implementation
--   for another as painless as possible.
--
--   Names have been chosen to match standard usage as much as possible.  This
--   means that operations for abstractions frequently share the same name
--   (for example, @empty@, @null@, @size@, etc).  It also means that in many
--   cases names have been reused from the Prelude.  However, the use of
--   @qualified@ imports will prevent name reuse from becoming name clashes.  If
--   for some reason you chose to import an Edison data structure unqualified,
--   you will likely need to import the Prelude @hiding@ the relevant names.
--
--   Edison modules also frequently share type names.  For example, most sequence
--   type constructors are named @Seq@.  This additionally aids substituting 
--   implementations by simply importing a different module.
--
--   Argument orders are selected with the following points in mind:
--
--   * /Partial application:/ arguments more likely to be static usually
--     appear before other arguments.
--
--   * /Collection appears last:/ in all cases where a function queries a
--     single collection or modifies an existing collection, the collection
--     argument will appear last.  This is something of a de facto standard
--     and lends a degree of consistency to the API.
--
--   * /Most natural order:/ where the function represents a well-known
--     mathematical function on more than one collection, the arguments
--     are chosen to match the most usual argument order for the function.
--
--   /Type classes:/
--
--   Each family of abstractions is defined as a set of classes: a main class
--   that every implementation of that abstraction should support and several
--   auxiliary subclasses that an implementation may or may not support. However,
--   not all applications require the power of type classes, so each method
--   is also directly accessible from the implementation module.  Thus you can
--   choose to use overloading or not, as appropriate for your particular
--   application.
--
--   Documentation about the behavior of data structure operations is defined
--   in the modules "Data.Edison.Seq", "Data.Edison.Coll" and
--   "Data.Edison.Assoc".  Implementations are required to respect
--   the descriptions and axioms found in these modules.  In some cases time
--   complexity is also given.  Implementations may differ from these time
--   complexities; if so, the differences will be given in the documentation for
--   the individual implementation module.
--
--   /Notes on unsafe functions:/
--
--   There are a number of different notions of what constitutes an unsafe function.
--   In Haskell, a function is generally called \"unsafe\" if it can subvert
--   type safety or referential integrity, such as @unsafePerformIO@.  In Edison,
--   however, we downgrade the meaning of \"unsafe\" somewhat.  An \"unsafe\" Edison
--   function is one which, if misused, can violate the structural invariants of
--   a data structure.  Misusing an Edison \"unsafe\" function should never cause
--   your runtime to crash or break referential integrity, but it may cause later
--   uses of a data structure to behave in undefined ways.  Almost all unsafe functions
--   in Edison are labeled with the @unsafe@ prefix.  An exception to this rule is the
--   @With@ functions in the 'Set' class, which are also unsafe but do not have
--   the prefix.  Unsafe functions will have explicit preconditions listed in their
--   documentation.
--
--   /Notes on ambiguous functions:/
--
--   Edison also contains a class of functions which are labeled \"ambiguous\".  These
--   functions cannot violate the structural integrity of a data structure, but, under
--   some conditions, the result of applying an ambiguous function is not well defined.
--   For example, the 'AssocX' class contains the @fold@ function, which folds over the
--   elements in the set in an arbitrary order.  If the combining function passed to
--   @fold@ is not commutative and associative, then the result of the fold is not
--   well-defined.  To aid programmers, each API function is labeled /well-defined/ or
--   /ambiguous/ in its documentation.  If a function is well-defined only under
--   some circumstances, that will also be explicitly stated.
--
--   /How to choose a fold:/
--
--   /Folds/ are an important class of operations on data structures in a functional
--   language; they perform essentially the same function that iterators perform in
--   imperative languages.  Edison provides a dizzying array of folds which (hopefully)
--   correspond to all the various ways a programmer might want to fold over a data
--   structure.  However, it can be difficult to know which fold to choose for a
--   particular application.  In general, you should choose a fold which provides
--   the /fewest/ guarantees necessary for correctness.  The folds which have fewer
--   guarantees give data structure implementers more leeway to provide efficient
--   implementations.  For example, if you which to fold a commutative, associative
--   function, you should chose @fold@ (which does not guarantee an order) over @foldl@
--   or @foldr@, which specify particular orders.  Also, if your function is strict in
--   one or both arguments, you should prefer the strict folds (eg, @fold'@); they will
--   often provide better space behavior.  /Be aware/, however, that the \"strict\" folds
--   are not /necessarily/ more strict than the \"non-strict\" folds; they merely give
--   implementers the option to provide additional strictness if it improves performance.
--   For associative collections, only use with @WithKey@ folds if you need the value
--   of the key.
--
--   /About this module:/
--  
--   This module re-exports the various data structure abstraction classes, but
--   not their methods. This allows you to write type signatures which have
--   contexts that mention Edison type classes without having to import the
--   appropriate modules @qualified@.  The class methods are not exported to
--   avoid name clashes.  Obviously, to use the methods of these classes, you
--   will have to import the appropriate modules.  This module additionally
--   re-exports the entire "Data.Edison.Prelude" module.
--
--   /Miscellaneous points:/
--
--   Some implementations export a few extra functions beyond those included
--   in the relevant classes.  These are typically operations that are
--   particularly efficient for that implementation, but are not general enough
--   to warrant inclusion in a class.
--
--   Since qualified infix symbols are fairly ugly, they have been largely avoided.
--   However, the "Data.Edison.Sym" module defines a number of infix operators
--   which alias the prefix operators; this module is intended to be imported
--   unqualified.
--
--   Most of the operations on most of the data structures are strict.  This is
--   inevitable for data structures with non-trivial invariants. Even given
--   that, however, many of the operations are stricter than necessary.  In
--   fact, operations are never deliberately made lazy unless the laziness is
--   required by the algorithm, as can happen with amortized data structures.
--
--   Note, however, that the various sequence implementations are always lazy
--   in their elements.  Similarly, associative collections are always lazy in
--   their elements (but usually strict in their keys).  Non-associative 
--   collections are usually strict in their elements.

module Data.Edison (

-- * Sequence class
  Sequence

-- * Collection classes
-- ** Non-observable collections
, CollX
, OrdCollX
, SetX
, OrdSetX
-- ** Observable collections
, Coll
, OrdColl
, Set
, OrdSet

-- * Associative collection classes
-- ** Non-observable associative collections
, AssocX
, OrdAssocX
, FiniteMapX
, OrdFiniteMapX
-- ** Observable associative collections
, Assoc
, OrdAssoc
, FiniteMap
, OrdFiniteMap

, module Data.Edison.Prelude
) where

import Data.Edison.Prelude
import Data.Edison.Seq
import Data.Edison.Coll
import Data.Edison.Assoc
