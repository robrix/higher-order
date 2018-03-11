{-# LANGUAGE ConstraintKinds #-}
module Higher.Comonad
( module H
, Comonad
) where

import Higher.Copointed as H
import Higher.Extend as H

type Comonad w = (Copointed w, Extend w)
