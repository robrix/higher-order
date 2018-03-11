{-# LANGUAGE FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Higher.Recursive
( Recursive(..)
, cata
, para
, module H
) where

import Higher.Function as H
import Higher.Functor as H
import Higher.Product

class Recursive (t :: k -> *) where
  type Base t :: (k -> *) -> (k -> *)

  project :: t ~> Base t t

  mcata :: forall a . (forall b . (b ~> a) -> (Base t b ~> a)) -> (t ~> a)
  mcata algebra = go
    where go :: t ~> a
          go = algebra go . project

  mpara :: forall a . (forall b . (b ~> (t :*: a)) -> (Base t b ~> a)) -> (t ~> a)
  mpara algebra = go
    where go :: t ~> a
          go = algebra (id <&&&> go) . project

cata :: (Recursive t, H.Functor (Base t)) => (Base t a ~> a) -> (t ~> a)
cata algebra = mcata (\ yield -> algebra . H.fmap yield)

para :: (Recursive t, H.Functor (Base t)) => (Base t (t :*: a) ~> a) -> (t ~> a)
para algebra = mpara (\ yield -> algebra . H.fmap yield)
