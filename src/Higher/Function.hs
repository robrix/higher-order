{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Higher.Function where

infixr 0 ~>
type a ~> b = forall x . a x -> b x

infixr 0 ~~>
type a ~~> b = forall x y . a x y -> b x y
