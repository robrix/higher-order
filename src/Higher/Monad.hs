{-# LANGUAGE ConstraintKinds, TypeOperators #-}
module Higher.Monad
( module H
, Monad
, return
) where

import Higher.Functor.Bind as H
import Higher.Pointed as H
import Prelude hiding (Monad(..))

type Monad m = (Bind m, Pointed m)

return :: Monad m => a ~> m a
return = point
