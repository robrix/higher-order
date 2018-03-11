{-# LANGUAGE ConstraintKinds #-}
module Higher.Monad
( module H
, Monad
) where

import Higher.Bind as H
import Higher.Pointed as H
import Prelude hiding (Monad)

type Monad m = (Bind m, Pointed m)
