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

  infixr 3 ***
  (***) :: (l1 x -> l2 y) -> (r1 x -> r2 y) -> ((l1 `p` r1) x -> (l2 `p` r2) y)
  f1 *** f2 = f1 . exl &&& f2 . exr

  infixr 3 &&&
  (&&&) :: (a -> l x) -> (a -> r x) -> (a -> (l `p` r) x)

instance Product (:*:) where
  exl (l :*: _) = l
  exr (_ :*: r) = r

  f1 &&& f2 = (:*:) <$> f1 <*> f2
