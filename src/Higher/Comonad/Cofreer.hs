{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
module Higher.Comonad.Cofreer
( Cofreer(..)
, CofreerF
, module H
) where

import Higher.Bifunctor
import Higher.Biproduct
import Higher.Comonad as H
import Higher.Const
import Higher.Corecursive
import Higher.Coyoneda
import Higher.Product
import Higher.Recursive

newtype Cofreer f a x = Cofreer { runCofreer :: (a :*: Coyoneda f (Cofreer f a)) x }

type CofreerF f a = Const a :**: Coyoneda f


instance Copointed (Cofreer f) where
  copoint = exl . runCofreer

instance H.Functor (Cofreer f) where
  fmap f = cata (embed . (first f <<***>> id))

instance Extend (Cofreer f) where
  extend f  = ana (Const . f <<&&&>> exr . runCofreer)
  duplicate = ana (Const     <<&&&>> exr . runCofreer)

instance Recursive (Cofreer f a) where
  type Base (Cofreer f a) = CofreerF f a
  project = (Const . exl <<&&&>> exr) . runCofreer

instance Corecursive (Cofreer f a) where
  type Cobase (Cofreer f a) = CofreerF f a
  embed = Cofreer . (getConst . biexl <&&&> biexr)
