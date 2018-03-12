{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Higher.Semigroup where

import Control.Applicative (Alternative(..))

class Semigroup a where
  (<>) :: a x -> a x -> a x


instance Semigroup Maybe where
  (<>) = (<|>)

instance Semigroup [] where
  (<>) = (<|>)


newtype Endo a x = Endo { appEndo :: a x -> a x }

instance Semigroup (Endo a) where
  Endo a <> Endo b = Endo (a . b)


newtype Dual a x = Dual { getDual :: a x }

instance Semigroup (Dual a) where
  (<>) = flip (<>)


newtype App f a = App { getApp :: f a }
  deriving (Alternative, Applicative, Eq, Foldable, Functor, Monad, Ord, Show, Traversable)

instance Applicative f => Semigroup (App f) where
  App a <> App b = App (a *> b)
