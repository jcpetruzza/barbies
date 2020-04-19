{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Barbies.Generics.Bare
  ( GBare(..)
  )

where

import Data.Functor.Identity (Identity(..))

import Data.Coerce (Coercible, coerce)
import Data.Generics.GenericN
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)


class GBare (n :: Nat) repbi repbb where
  gstrip :: Proxy n -> repbi x -> repbb x
  gcover :: Proxy n -> repbb x -> repbi x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GBare n repbi repbb => GBare n (M1 i k repbi) (M1 i k repbb) where
  gstrip pn = M1 . gstrip pn . unM1
  {-# INLINE gstrip #-}

  gcover pn = M1 . gcover pn . unM1
  {-# INLINE gcover #-}


instance GBare n V1 V1 where
  gstrip _ _ = undefined
  gcover _ _ = undefined

instance GBare n U1 U1 where
  gstrip _ = id
  {-# INLINE gstrip #-}

  gcover _ = id
  {-# INLINE gcover #-}


instance (GBare n l l', GBare n r r') => GBare n (l :*: r) (l' :*: r') where
  gstrip pn (l :*: r) = (gstrip pn l) :*: gstrip pn r
  {-# INLINE gstrip #-}

  gcover pn (l :*: r) = (gcover pn l) :*: gcover pn r
  {-# INLINE gcover #-}


instance (GBare n l l', GBare n r r') => GBare n (l :+: r) (l' :+: r') where
  gstrip pn = \case
    L1 l -> L1 (gstrip pn l)
    R1 r -> R1 (gstrip pn r)
  {-# INLINE gstrip #-}

  gcover pn = \case
    L1 l -> L1 (gcover pn l)
    R1 r -> R1 (gcover pn r)
  {-# INLINE gcover #-}

-- --------------------------------
-- The interesting cases
-- --------------------------------

type P = Param

instance Coercible a b => GBare n (Rec (P n Identity a) (Identity a)) (Rec b b) where
  gstrip _ = coerce
  {-# INLINE gstrip #-}

  gcover _ = coerce
  {-# INLINE gcover #-}

instance repbi ~ repbb => GBare n (Rec repbi repbi) (Rec repbb repbb) where
  gstrip _ = id
  {-# INLINE gstrip #-}

  gcover _ = id
  {-# INLINE gcover #-}
