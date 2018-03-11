{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, TypeOperators #-}
module Higher.Bisum
( Bisum(..)
, (:++:)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Higher.Function

class Bisum s where
  biinl :: l ~~> (l `s` r)
  biinr :: r ~~> (l `s` r)

  infixr 3 <<+++>>
  (<<+++>>) :: (l1 x y -> l2 z w) -> (r1 x y -> r2 z w) -> ((l1 `s` r1) x y -> (l2 `s` r2) z w)
  f1 <<+++>> f2 = biinl . f1 <<|||>> biinr . f2

  infixr 3 <<|||>>
  (<<|||>>) :: (l x y -> a) -> (r x y -> a) -> ((l `s` r) x y -> a)


data (l :++: r) a b = L2 (l a b) | R2 (r a b)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance (Bifoldable l, Bifoldable r) => Bifoldable (l :++: r) where
  bifoldMap f g (L2 l) = bifoldMap f g l
  bifoldMap f g (R2 r) = bifoldMap f g r

instance (Bifunctor l, Bifunctor r) => Bifunctor (l :++: r) where
  bimap f g (L2 l) = L2 (bimap f g l)
  bimap f g (R2 r) = R2 (bimap f g r)

instance (Bitraversable l, Bitraversable r) => Bitraversable (l :++: r) where
  bitraverse f g (L2 l) = L2 <$> bitraverse f g l
  bitraverse f g (R2 r) = R2 <$> bitraverse f g r

instance Bisum (:++:) where
  biinl = L2
  biinr = R2

  (f <<|||>> _) (L2 a) = f a
  (_ <<|||>> g) (R2 b) = g b
