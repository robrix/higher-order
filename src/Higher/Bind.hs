{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Bind
( Bind(..)
, (>>=)
) where

import Higher.Function as H
import Prelude hiding ((>>=))

class Bind m where
  join :: m (m a) ~> m a
  bind :: (a ~> m b) -> (m a ~> m b)

(>>=) :: Bind m => m a x -> (a ~> m b) -> m b x
m >>= f = bind f m
