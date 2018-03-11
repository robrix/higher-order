{-# LANGUAGE TypeOperators #-}
module Higher.Product
( Product(..)
, (:*:)(..)
) where

import GHC.Generics
import Higher.Function

class Product p where
  exl :: (l `p` r) ~> l
  exr :: (l `p` r) ~> r

instance Product (:*:) where
  exl (l :*: _) = l
  exr (_ :*: r) = r
