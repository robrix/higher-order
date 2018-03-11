{-# LANGUAGE PolyKinds, TypeOperators #-}
module Higher.Category
( Category(..)
, Cartesian(..)
, Choice(..)
) where

import Control.Category
import GHC.Generics ((:+:) (..), (:*:) (..))
import Prelude hiding (id, (.))

class Category cat => Cartesian cat where
  infixr 3 ***
  (***) :: (l1 x `cat` l2 y) -> (r1 x `cat` r2 y) -> ((l1 :*: r1) x `cat` (l2 :*: r2) y)

  infixr 3 &&&
  (&&&) :: (a `cat` l x) -> (a `cat` r x) -> (a `cat` (l :*: r) x)

instance Cartesian (->) where
  (f1 *** f2) (a1 :*: a2) = f1 a1 :*: f2 a2
  f1 &&& f2 = (:*:) <$> f1 <*> f2


class Category cat => Choice cat where
  infixr 2 +++
  (+++) :: (l1 x `cat` l2 y) -> (r1 x `cat` r2 y) -> ((l1 :+: r1) x `cat` (l2 :+: r2) y)

  infixr 2 |||
  (|||) :: (l x `cat` b) -> (r x `cat` b) -> ((l :+: r) x `cat` b)

instance Choice (->) where
  (f1 +++ _)  (L1 a1) = L1 (f1 a1)
  (_  +++ f2) (R1 a2) = R1 (f2 a2)
  (f1 ||| _)  (L1 a1) = f1 a1
  (_  ||| f2) (R1 a2) = f2 a2
