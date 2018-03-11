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
