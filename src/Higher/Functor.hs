{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Functor
( Functor(..)
, module X
) where

import Higher.Function as X
import Prelude hiding (Functor)

class Functor f where
  fmap :: (a ~> b) -> (f a ~> f b)
