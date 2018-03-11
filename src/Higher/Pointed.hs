{-# LANGUAGE PolyKinds, TypeOperators #-}
module Higher.Pointed
( Pointed(..)
, module H
) where

import Higher.Function as H

class Pointed p where
  point :: a ~> p a
