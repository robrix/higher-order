{-# LANGUAGE DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Higher.Generics
( Rollable1(..)
, module X
) where

import Data.Coerce
import Higher.Biconst as X
import Higher.Biidentity as X
import Higher.Biproduct as X
import Higher.Bisum as X
import Higher.Biunit as X
import Higher.Product
import Higher.Sum
import GHC.Generics

class Rollable1 t where
  type Unrolled1 t :: * -> * -> *
  type Unrolled1 t = GUnrolled1 t (Rep1 t)

  unroll1 ::           t a       -> Unrolled1 t a (t a)
  roll1   :: Unrolled1 t a (t a) ->           t a

  default unroll1 :: (Generic1 t, GRollable1 t (Rep1 t), Unrolled1 t ~ GUnrolled1 t (Rep1 t))
                  =>           t a       -> Unrolled1 t a (t a)
  default roll1   :: (Generic1 t, GRollable1 t (Rep1 t), Unrolled1 t ~ GUnrolled1 t (Rep1 t))
                  => Unrolled1 t a (t a) ->           t a

  unroll1 = gunroll1 . from1
  roll1   =            to1   . groll1

class  GRollable1 t (rep :: * -> *) where
  type GUnrolled1 t  rep :: * -> * -> *
  gunroll1 ::              rep a       -> GUnrolled1 t rep a (t a)
  groll1   :: GUnrolled1 t rep a (t a) ->              rep a

instance GRollable1 t f
      => GRollable1 t (M1 i c f) where
  type   GUnrolled1 t (M1 i c f) = GUnrolled1 t f
  gunroll1 = gunroll1 . unM1
  groll1   =              M1 . groll1

instance ( GRollable1 t l
         , GRollable1 t r )
      =>   GRollable1 t (l :*: r) where
  type     GUnrolled1 t (l :*: r)
    = (    GUnrolled1 t l
      :**: GUnrolled1 t r )
  gunroll1 = gunroll1 . exl <<&&&>> gunroll1 . exr
  groll1   = groll1 . biexl  <&&&>  groll1 . biexr

instance ( GRollable1 t l
         , GRollable1 t r )
      =>   GRollable1 t (l :+: r) where
  type     GUnrolled1 t (l :+: r)
    = (    GUnrolled1 t l
      :++: GUnrolled1 t r)
  gunroll1 = biinl . gunroll1 <|||> biinr . gunroll1
  groll1   =   inl . groll1  <<|||>>  inr . groll1

instance GRollable1 t (K1 i k) where
  type   GUnrolled1 t (K1 i k) = K2 k
  gunroll1 = coerce
  groll1   = coerce

instance GRollable1 t U1 where
  type   GUnrolled1 t U1 = U2
  gunroll1 _ = U2
  groll1   _ = U1

instance GRollable1 t Par1 where
  type   GUnrolled1 t Par1 = Par2L
  gunroll1 = coerce
  groll1   = coerce

instance GRollable1 t (Rec1 t) where
  type   GUnrolled1 t (Rec1 t) = Par2R
  gunroll1 = coerce
  groll1   = coerce
