{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Higher.Generics where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

data U2 a b = U2
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable U2 where
  bifoldMap _ _ _ = mempty

instance Bifunctor U2 where
  bimap _ _ _ = U2

instance Bitraversable U2 where
  bitraverse _ _ _ = pure U2
