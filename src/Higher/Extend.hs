{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Extend
( Extend(..)
, module H
, (<<=)
, (=>>)
) where

import Higher.Function as H

class Extend w where
  duplicate :: w a ~> w (w a)
  extend :: (w a ~> b) -> (w a ~> w b)

(=>>) :: Extend w => w a x -> (w a ~> b) -> w b x
w =>> f = extend f w

(<<=) :: Extend w => (w a ~> b) -> (w a ~> w b)
(<<=) = extend
