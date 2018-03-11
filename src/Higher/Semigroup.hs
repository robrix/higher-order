module Higher.Semigroup where

import Control.Applicative ((<|>))

class Semigroup a where
  (<>) :: a x -> a x -> a x


instance Semigroup Maybe where
  (<>) = (<|>)

instance Semigroup [] where
  (<>) = (<|>)


newtype Endo a x = Endo { appEndo :: a x -> a x }

instance Semigroup (Endo a) where
  Endo a <> Endo b = Endo (a . b)
