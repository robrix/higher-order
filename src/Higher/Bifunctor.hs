{-# LANGUAGE PolyKinds, RankNTypes, TypeOperators #-}
module Higher.Bifunctor
( Bifunctor(..)
, module H
) where

import Higher.Function as H

class Bifunctor p where
  bimap  :: (a1 ~> a2) -> (b1 ~> b2) -> (a1 `p` b1 ~> a2 `p` b2)

  first  :: (a1 ~> a2) ->               (a1 `p` b  ~> a2 `p` b)
  first f = bimap f id

  second ::               (b1 ~> b2) -> (a  `p` b1 ~> a  `p` b2)
  second = bimap id
