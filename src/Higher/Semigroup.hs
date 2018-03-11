module Higher.Semigroup where

import Control.Applicative ((<|>))

class Semigroup a where
  (<>) :: a x -> a x -> a x


instance Semigroup Maybe where
  (<>) = (<|>)

instance Semigroup [] where
  (<>) = (<|>)
