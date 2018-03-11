module Higher.Semigroup where

class Semigroup a where
  (<>) :: a x -> a x -> a x
