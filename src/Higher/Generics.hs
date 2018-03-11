{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DefaultSignatures, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Higher.Generics
( Rollable(..)
, Rollable1(..)
, module X
) where

import Data.Coerce
import Data.Functor.Const
import Higher.Biconst as X
import Higher.Biidentity as X
import Higher.Biproduct as X
import Higher.Bisum as X
import Higher.Biunit as X
import Higher.Product
import Higher.Sum
import GHC.Generics

class Rollable t where
  type Unrolled t :: * -> *
  type Unrolled t = GUnrolled (EqK t (Rep t)) t (Rep t)

  unroll ::          t   -> Unrolled t t
  roll   :: Unrolled t t ->          t

  default unroll :: forall eq . (Generic t, eq ~ EqK t (Rep t), GRollable eq t (Rep t), Unrolled t ~ GUnrolled eq t (Rep t))
                 =>          t   -> Unrolled t t
  default roll   :: forall eq . (Generic t, eq ~ EqK t (Rep t), GRollable eq t (Rep t), Unrolled t ~ GUnrolled eq t (Rep t))
                 => Unrolled t t ->          t

  unroll = gunroll @eq @t @(Rep t) . from
  roll   =                           to   . groll @eq @t @(Rep t)


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


type family EqK a (b :: * -> *) :: Bool where
  EqK t (K1 i t) = 'True
  EqK t _        = 'False


class  GRollable (eq :: Bool) t (rep :: * -> *) where
  type GUnrolled  eq          t  rep :: * -> *
  gunroll ::                rep t -> GUnrolled eq t rep t
  groll   :: GUnrolled eq t rep t ->                rep t

instance ( eqF ~ EqK t f
         , GRollable eqF t f
         )
      => GRollable 'False t (M1 i c f) where
  type   GUnrolled 'False t (M1 i c f) = GUnrolled (EqK t f) t f
  gunroll = gunroll @eqF @t @f . unM1
  groll   =                        M1 . groll @eqF @t @f

instance ( eqL ~ EqK t l
         , eqR ~ EqK t r
         , GRollable eqL t l
         , GRollable eqR t r
         )
      => GRollable 'False t (l :*: r) where
  type   GUnrolled 'False t (l :*: r)
    = (   GUnrolled (EqK t l) t l
      :*: GUnrolled (EqK t r) t r )
  gunroll = gunroll @eqL @t @l <***> gunroll @eqR @t @r
  groll   = groll   @eqL @t @l <***> groll   @eqR @t @r

instance ( eqL ~ EqK t l
         , eqR ~ EqK t r
         , GRollable eqL t l
         , GRollable eqR t r
         )
      => GRollable 'False t (l :+: r) where
  type   GUnrolled 'False t (l :+: r)
    = (   GUnrolled (EqK t l) t l
      :+: GUnrolled (EqK t r) t r)
  gunroll = gunroll @eqL @t @l <+++> gunroll @eqR @t @r
  groll   = groll   @eqL @t @l <+++> groll   @eqR @t @r

instance GRollable 'True t (K1 i t) where
  type   GUnrolled 'True t (K1 i t) = Par1
  gunroll = coerce
  groll   = coerce

instance GRollable 'False t (K1 i k) where
  type   GUnrolled 'False t (K1 i k) = Const k
  gunroll = coerce
  groll   = coerce

instance GRollable 'False t U1 where
  type   GUnrolled 'False t U1 = U1
  gunroll = coerce
  groll   = coerce


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
