-- | Useful operator (++) = mappend.

module Data.Monoid.Operator where

import Prelude hiding ((++))
import Data.Monoid (Monoid)
import Data.Monoid (mappend)

-- | Operator for mappend.
(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++
