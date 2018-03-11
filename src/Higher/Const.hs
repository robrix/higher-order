{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, PolyKinds #-}
module Higher.Const
( Const(..)
) where

import Higher.Functor as H

newtype Const a b x = Const { getConst :: a x }
  deriving (Eq, Foldable, Prelude.Functor, Ord, Show, Traversable)

instance H.Functor (Const a) where
  fmap _ (Const a) = Const a
