{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, PolyKinds, TypeOperators #-}
module Higher.Biproduct
( Biproduct(..)
, (:**:)(..)
) where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Higher.Function

class Biproduct p where
  biexl :: (l `p` r) ~~> l
  biexr :: (l `p` r) ~~> r

  infixr 3 <<***>>
  (<<***>>) :: (l1 x y -> l2 z w) -> (r1 x y -> r2 z w) -> ((l1 `p` r1) x y -> (l2 `p` r2) z w)
  f1 <<***>> f2 = f1 . biexl <<&&&>> f2 . biexr

  infixr 3 <<&&&>>
  (<<&&&>>) :: (a -> l x y) -> (a -> r x y) -> (a -> (l `p` r) x y)


data (l :**: r) a b = l a b :**: r a b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance (Bifoldable l, Bifoldable r) => Bifoldable (l :**: r) where
  bifoldMap f g (l :**: r) = bifoldMap f g l `mappend` bifoldMap f g r

instance (Bifunctor l, Bifunctor r) => Bifunctor (l :**: r) where
  bimap f g (l :**: r) = bimap f g l :**: bimap f g r

instance (Bitraversable l, Bitraversable r) => Bitraversable (l :**: r) where
  bitraverse f g (l :**: r) = (:**:) <$> bitraverse f g l <*> bitraverse f g r

instance Biproduct (:**:) where
  biexl (l :**: _) = l
  biexr (_ :**: r) = r

  f1 <<&&&>> f2 = (:**:) <$> f1 <*> f2
