{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Higher.Function where

type a ~> b = forall x . a x -> b x
type a ~~> b = forall x y . a x y -> b x y
