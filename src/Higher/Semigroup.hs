module Higher.Semigroup where

class Semigroup s where
  (<>) :: s x -> s x -> s x
