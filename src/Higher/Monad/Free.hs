{-# LANGUAGE PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module Higher.Monad.Free where

import Higher.Bifunctor
import Higher.Bisum
import Higher.Const
import Higher.Monad as H
import Higher.Recursive
import Higher.Sum

newtype Free f a x = Free { runFree :: (a :+: f (Free f a)) x }

type FreeF f a = Const a :++: f


instance Pointed (Free f) where
  point = Free . inl

instance H.Functor f => H.Functor (Free f) where
  fmap f = cata (Free . (inl . getConst <<|||>> inr) . (first f <<+++>> id))

instance H.Functor f => Bind (Free f) where
  bind f = cata (f . getConst <<|||>> Free . inr)
  join   = cata (    getConst <<|||>> Free . inr)


instance Recursive (Free f a) where
  type Base (Free f a) = FreeF f a
  project = (biinl . Const <|||> biinr) . runFree
