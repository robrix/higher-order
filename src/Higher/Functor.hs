{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Functor
( Functor(..)
) where

import Higher.Function
import Prelude hiding (Functor)

class Functor f where
  fmap :: (a ~> b) -> (f a ~> f b)
