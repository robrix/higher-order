{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Foldable
( Foldable(..)
, module X
) where

import Higher.Function as X
import Higher.Monoid as X
import Prelude hiding (Foldable, Monoid)

class Foldable t where
  foldMap :: Monoid m => (a ~> m) -> (t a ~> m)
