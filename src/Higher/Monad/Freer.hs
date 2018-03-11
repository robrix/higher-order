{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
module Higher.Monad.Freer
( Freer(..)
, FreerF
, module H
) where

import Higher.Bifunctor
import Higher.Bisum
import Higher.Const
import Higher.Corecursive
import Higher.Coyoneda
import Higher.Monad as H
import Higher.Recursive
import Higher.Sum

newtype Freer f a x = Freer { runFreer :: (a :+: Coyoneda f (Freer f a)) x }

type FreerF f a = Const a :++: Coyoneda f


instance Pointed (Freer f) where
  point = Freer . inl

instance H.Functor (Freer f) where
  fmap f = cata (embed . (first f <<+++>> id))

instance Bind (Freer f) where
  bind f = cata (f . getConst <<|||>> Freer . inr)
  join   = cata (    getConst <<|||>> Freer . inr)

instance Recursive (Freer f a) where
  type Base (Freer f a) = FreerF f a
  project = (biinl . Const <|||> biinr) . runFreer

instance Corecursive (Freer f a) where
  type Cobase (Freer f a) = FreerF f a
  embed = Freer . (inl . getConst <<|||>> inr)
