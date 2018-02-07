{-# LANGUAGE CPP #-}
------------------------------------------------------------------------------
-- | 
-- Maintainer	: Ralf Laemmel, Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'StrategyLib', a library of functional strategy
-- combinators, including combinators for generic traversal. This module
-- defines additional instances of the Monoid class.

------------------------------------------------------------------------------

module Data.Generics.Strafunski.StrategyLib.MoreMonoids where

import Data.Monoid

------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 804
instance Num a => Semigroup a where
  (<>) = mappend
#endif

-- | Any 'Num' is a 'Monoid'.
instance Num a => Monoid a where
 mempty = 0
 mappend = (+)

-----------------------------------------------------------------------------
