module Higher.Monoid where

import Higher.Semigroup

class Semigroup a => Monoid a where
  zero :: a x
