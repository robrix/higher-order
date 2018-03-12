module Higher.Monoid where

import Control.Applicative (Alternative(..))
import Higher.Semigroup
import Prelude hiding (Monoid)

class Semigroup a => Monoid a where
  zero :: a x


instance Monoid Maybe where
  zero = Nothing

instance Monoid [] where
  zero = []

instance Monoid (Endo a) where
  zero = Endo id

instance Alternative f => Monoid (Alt f) where
  zero = Alt empty
