{-# LANGUAGE ConstrainedClassMethods, FlexibleContexts, PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Higher.Internal
( Recursive(..)
, cata
, para
, histo
, Corecursive(..)
, ana
, apo
, futu
, Fix(..)
, hylo
, Free(..)
, FreeF
, Cofree(..)
, CofreeF
) where

import Higher.Bifunctor
import Higher.Biproduct
import Higher.Bisum
import Higher.Const
import Higher.Comonad as H
import Higher.Monad as H
import Higher.Product
import Higher.Sum

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

  mhisto :: forall a . H.Functor (Base t) => (forall b . (b ~> Cofree (Base t) a) -> (Base t b ~> a)) -> (t ~> a)
  mhisto algebra = copoint . go
    where go :: t ~> Cofree (Base t) a
          go = Cofree . (algebra id <&&&> id) . H.fmap go . project

cata :: (Recursive t, H.Functor (Base t)) => (Base t a ~> a) -> (t ~> a)
cata algebra = mcata (\ yield -> algebra . H.fmap yield)

para :: (Recursive t, H.Functor (Base t)) => (Base t (t :*: a) ~> a) -> (t ~> a)
para algebra = mpara (\ yield -> algebra . H.fmap yield)

histo :: (Recursive t, H.Functor (Base t)) => (Base t (Cofree (Base t) a) ~> a) -> (t ~> a)
histo algebra = mhisto (\ yield -> algebra . H.fmap yield)


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


newtype Fix f x = Fix { unFix :: f (Fix f) x }

instance Recursive (Fix f) where
  type Base (Fix f) = f
  project = unFix

instance Corecursive (Fix f) where
  type Cobase (Fix f) = f
  embed = Fix


hylo :: forall f a b . H.Functor f => (f b ~> b) -> (a ~> f a) -> (a ~> b)
hylo f g = h
  where h :: a ~> b
        h = f . H.fmap h . g


newtype Free f a x = Free { runFree :: (a :+: f (Free f a)) x }

type FreeF f a = Const a :++: f


instance Pointed (Free f) where
  point = Free . inl

instance H.Functor f => H.Functor (Free f) where
  fmap f = cata (embed . (first f <<+++>> id))

instance H.Functor f => Bind (Free f) where
  bind f = cata (f . getConst <<|||>> Free . inr)
  join   = cata (    getConst <<|||>> Free . inr)

instance Recursive (Free f a) where
  type Base (Free f a) = FreeF f a
  project = (biinl . Const <|||> biinr) . runFree

instance Corecursive (Free f a) where
  type Cobase (Free f a) = FreeF f a
  embed = Free . (inl . getConst <<|||>> inr)


newtype Cofree f a x = Cofree { runCofree :: (a :*: f (Cofree f a)) x }

type CofreeF f a = Const a :**: f


instance Copointed (Cofree f) where
  copoint = exl . runCofree

instance H.Functor f => H.Functor (Cofree f) where
  fmap f = cata (embed . (first f <<***>> id))

instance H.Functor f => Extend (Cofree f) where
  extend f  = ana (Const . f <<&&&>> exr . runCofree)
  duplicate = ana (Const     <<&&&>> exr . runCofree)

instance Recursive (Cofree f a) where
  type Base (Cofree f a) = CofreeF f a
  project = (Const . exl <<&&&>> exr) . runCofree

instance Corecursive (Cofree f a) where
  type Cobase (Cofree f a) = CofreeF f a
  embed = Cofree . (getConst . biexl <&&&> biexr)
