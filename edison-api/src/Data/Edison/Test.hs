-- | An interface for testing internal invariants of data structures.

module Data.Edison.Test where

-- | Wrap a data structure in this newtype to equip it with alternative
-- 'Arbitrary' and 'Show' instances that are better suited for testing
-- the data structure's invariants, using @FlexibleInstances@.
-- - 'Arbitrary' may have probability distributions that better cover
--   the set of valid shapes than @fromSeq <$> arbitrary@.
-- - 'Show' instances may show the internal structure instead
--   of the client's view.
--
-- Although such 'Show' instances expose implementation details, users have to
-- really go out of their way to import this module and use 'InternalTest' to
-- actually observe them.
--
-- Usage:
--
-- @
-- instance Arbitrary (InternalTest (FM k v)) where
--   arbitrary = ...
--   shrink = ...
--
-- instance Show (InternalTest (FM k v)) where
--   show = ...
-- @
newtype InternalTest a = InternalTest a
  
