{-# LANGUAGE ConstrainedClassMethods, FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Higher.Corecursive
( Corecursive(..)
, ana
, apo
, futu
, module H
) where

import Higher.Bisum
import Higher.Const
import Higher.Functor as H
import Higher.Pointed
import Higher.Monad.Free
import Higher.Sum

class Corecursive (t :: k -> *) where
  type Cobase t :: (k -> *) -> (k -> *)

  embed :: Cobase t t ~> t

  mana :: forall a . (forall b . (a ~> b) -> (a ~> Cobase t b)) -> (a ~> t)
  mana coalgebra = go
    where go :: a ~> t
          go = embed . coalgebra go

  mapo :: forall a . (forall b . ((t :+: a) ~> b) -> (a ~> Cobase t b)) -> (a ~> t)
  mapo coalgebra = go
    where go :: a ~> t
          go = embed . coalgebra (id <|||> go)

  mfutu :: forall a . H.Functor (Cobase t) => (forall b . (Free (Cobase t) a ~> b) -> (a ~> Cobase t b)) -> (a ~> t)
  mfutu coalgebra = go . point
    where go :: Free (Cobase t) a ~> t
          go = embed . H.fmap go . (coalgebra id <|||> id) . runFree

ana :: (Corecursive t, H.Functor (Cobase t)) => (a ~> Cobase t a) -> (a ~> t)
ana coalgebra = mana (\ coyield -> H.fmap coyield . coalgebra)

apo :: (Corecursive t, H.Functor (Cobase t)) => (a ~> Cobase t (t :+: a)) -> (a ~> t)
apo coalgebra = mapo (\ coyield -> H.fmap coyield . coalgebra)

futu :: (Corecursive t, H.Functor (Cobase t)) => (a ~> Cobase t (Free (Cobase t) a)) -> (a ~> t)
futu coalgebra = mfutu (\ coyield -> H.fmap coyield . coalgebra)


instance Corecursive (Free f a) where
  type Cobase (Free f a) = FreeF f a
  embed = Free . (inl . getConst <<|||>> inr)
