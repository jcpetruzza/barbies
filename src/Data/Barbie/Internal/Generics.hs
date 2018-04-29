-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie.Internal.Functor
--
-- GHC is at the moemt unable to derive @'Generic1' b@ for a Barbie-type
-- @b@. Instead, we use a trick by which we use the uninhabited type
-- 'Target' to identify the point where an 'f' occurs. That is, we coerce
-- a @b f@ into a @b 'Target'@, operate on the representation of this type,
-- and finally coerce back to the desired type.
----------------------------------------------------------------------------
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Data.Barbie.Internal.Generics
  ( Target
  , unsafeTargetBarbie
  , unsafeUntarget
  , unsafeTarget
  , unsafeUntargetBarbie

  , Repl

  , RecRep
  , RecUsage(..), NonRec(..)
  , AnnRec, DeannRec
  , toWithRecAnn
  , fromWithRecAnn
  )

where

import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

-- | We use 'Target' to identify the position in
--   in the generic representation where @f@ is used.
--   This is a hack to overcome the fact that 'Generic1'
--   does not currently work on a type @T f@ whenever
--   if 'f' is applied in 'T', which are all the interesting
--   cases!
data Target (f :: * -> *) a

unsafeTargetBarbie :: forall t b f . b f -> b (Target t)
unsafeTargetBarbie = unsafeCoerce

unsafeUntarget :: forall t f a . Target t a -> f a
unsafeUntarget = unsafeCoerce

unsafeTarget :: forall t f a . f a -> Target t a
unsafeTarget = unsafeCoerce

unsafeUntargetBarbie :: forall t b f . b (Target t) -> b f
unsafeUntargetBarbie = unsafeCoerce

type family Repl f g rep where
    Repl f g (M1 i c x)       = M1 i c (Repl f g x)
    Repl f g V1               = V1
    Repl f g U1               = U1
    Repl f g (K1 i (f a))     = K1 i (g a)
    Repl f g (K1 i (b f))     = K1 i (b g)
    Repl f g (K1 i (h (b f))) = K1 i (h (b g))
    Repl f g (K1 i c)         = K1 i c
    Repl f g (l :+: r)        = (Repl f g l) :+: (Repl f g r)
    Repl f g (l :*: r)        = (Repl f g l) :*: (Repl f g r)


-- | We use 'RecUsage' to identify the position in the
--   generic representation where the barbie type is used
--   recursively.
newtype RecUsage a
  = RecUsage a

newtype NonRec a
  = NonRec a

type family AnnRec a rep where
  AnnRec a (M1 i c x)  = M1  i c (AnnRec a x)
  AnnRec a V1          = V1
  AnnRec a U1          = U1
  AnnRec a (K1 i a)    = K1 i (RecUsage a)
  AnnRec a (K1 i a')   = K1 i (NonRec a')
  AnnRec a (l :*: r)   = AnnRec a l :*: AnnRec a r
  AnnRec a (l :+: r)   = AnnRec a l :+: AnnRec a r

type family DeannRec rep where
  DeannRec (M1 i c x)          = M1  i c (DeannRec x)
  DeannRec V1                  = V1
  DeannRec U1                  = U1
  DeannRec (K1 i (RecUsage a)) = K1 i a
  DeannRec (K1 i (NonRec a))   = K1 i a
  DeannRec (l :*: r)           = DeannRec l :*: DeannRec r
  DeannRec (l :+: r)           = DeannRec l :+: DeannRec r

fromWithRecAnn :: Generic a => a -> RecRep a x
fromWithRecAnn = unsafeCoerce . from

toWithRecAnn :: Generic a => RecRep a x -> a
toWithRecAnn = to . unsafeCoerce

type RecRep a = AnnRec a (Rep a)
