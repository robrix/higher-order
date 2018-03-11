{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Higher.Biidentity where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

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
