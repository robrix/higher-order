{-# LANGUAGE GADTs, PolyKinds, RankNTypes, TypeOperators #-}
module Higher.Coyoneda
( Coyoneda(..)
, module H
) where

import Higher.Functor as H

data Coyoneda f a x where
  Coyoneda :: (b ~> a) -> f b x -> Coyoneda f a x

instance H.Functor (Coyoneda f) where
  fmap f (Coyoneda g a) = Coyoneda (f . g) a
