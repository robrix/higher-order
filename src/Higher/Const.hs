{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, PolyKinds #-}
module Higher.Const
( Const(..)
, module H
) where

import Higher.Bifunctor
import Higher.Functor as H

newtype Const a b x = Const { getConst :: a x }
  deriving (Eq, Foldable, Prelude.Functor, Ord, Show, Traversable)

instance H.Functor (Const a) where
  fmap _ (Const a) = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

  second _ (Const a) = Const a
