{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Foldable
( Foldable(..)
) where

import Higher.Function
import Higher.Monoid
import Prelude hiding (Foldable, Monoid)

class Foldable t where
  foldMap :: Monoid m => (a ~> m) -> (t a ~> m)
