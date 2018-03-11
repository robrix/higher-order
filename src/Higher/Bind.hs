{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Higher.Bind
( Bind(..)
, (>>=)
, (=<<)
, module H
) where

import Higher.Functor as H
import Prelude hiding ((>>=), (=<<))

class H.Functor m => Bind m where
  join :: m (m a) ~> m a
  bind :: (a ~> m b) -> (m a ~> m b)

(>>=) :: Bind m => m a x -> (a ~> m b) -> m b x
m >>= f = bind f m

(=<<) :: Bind m => (a ~> m b) -> (m a ~> m b)
(=<<) = bind
