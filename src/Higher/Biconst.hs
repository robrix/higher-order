{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Higher.Biconst where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce

newtype K2 k a b = K2 { unK2 :: k }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Bifoldable (K2 k) where
  bifoldMap _ _ _ = mempty

instance Bifunctor (K2 k) where
  bimap _ _ = coerce

instance Bitraversable (K2 k) where
  bitraverse _ _ = pure . coerce
