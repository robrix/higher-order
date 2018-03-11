{-# LANGUAGE TypeOperators #-}
module Higher.Sum
( Sum(..)
, (:+:)(..)
) where

import Higher.Function
import GHC.Generics

class Sum s where
  inl :: l ~> (l `s` r)
  inr :: r ~> (l `s` r)

  infixr 2 +++
  (+++) :: (l1 x -> l2 y) -> (r1 x -> r2 y) -> ((l1 `s` r1) x -> (l2 `s` r2) y)
  f1 +++ f2 = inl . f1 ||| inr . f2

  infixr 2 |||
  (|||) :: (l x -> b) -> (r x -> b) -> ((l `s` r) x -> b)

instance Sum (:+:) where
  inl = L1
  inr = R1

  (f1 ||| _)  (L1 a1) = f1 a1
  (_  ||| f2) (R1 a2) = f2 a2
