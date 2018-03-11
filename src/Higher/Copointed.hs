{-# LANGUAGE PolyKinds, TypeOperators #-}
module Higher.Copointed
( Copointed(..)
, module H
) where

import Higher.Function as H

class Copointed p where
  copoint :: p a ~> a
