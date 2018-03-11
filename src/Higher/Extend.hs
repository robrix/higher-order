{-# LANGUAGE RankNTypes, TypeOperators #-}
module Higher.Extend
( Extend(..)
, module H
) where

import Higher.Function as H

class Extend w where
  duplicate :: w a ~> w (w a)
  extend :: (w a ~> b) -> (w a ~> w b)
