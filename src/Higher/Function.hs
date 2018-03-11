{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Function where

type a ~> b = forall x . a x -> b x
