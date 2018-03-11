{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Higher.Generics where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce

data U2 a b = U2
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable U2 where
  bifoldMap _ _ _ = mempty

instance Bifunctor U2 where
  bimap _ _ _ = U2

instance Bitraversable U2 where
  bitraverse _ _ _ = pure U2


newtype K2 k a b = K2 { unK2 :: k }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable (K2 k) where
  bifoldMap _ _ _ = mempty

instance Bifunctor (K2 k) where
  bimap _ _ = coerce

instance Bitraversable (K2 k) where
  bitraverse _ _ = pure . coerce


newtype Par2L a b = Par2L { unPar2L :: a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Par2L where
  bifoldMap f _ (Par2L a) = f a

instance Bifunctor Par2L where
  bimap f _ (Par2L a) = Par2L (f a)

instance Bitraversable Par2L where
  bitraverse f _ (Par2L a) = Par2L <$> f a


newtype Par2R a b = Par2R { unPar2R :: b }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable Par2R where
  bifoldMap _ g (Par2R b) = g b

instance Bifunctor Par2R where
  bimap _ g (Par2R b) = Par2R (g b)

instance Bitraversable Par2R where
  bitraverse _ g (Par2R b) = Par2R <$> g b
