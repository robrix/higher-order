{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Higher.Corecursive
( Corecursive(..)
, ana
, apo
, module H
) where

import Higher.Function as H
import Higher.Functor as H
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

ana :: (Corecursive t, H.Functor (Cobase t)) => (a ~> Cobase t a) -> (a ~> t)
ana coalgebra = mana (\ coyield -> H.fmap coyield . coalgebra)

apo :: (Corecursive t, H.Functor (Cobase t)) => (a ~> Cobase t (t :+: a)) -> (a ~> t)
apo coalgebra = mapo (\ coyield -> H.fmap coyield . coalgebra)
