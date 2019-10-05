{-# LANGUAGE TypeFamilies #-}
module Barbies.Internal.Bare
  ( Wear, Bare, Covered
  , BareB(..)
  , bstripFrom, bcoverWith

  , GBareB(..)
  , gbstripDefault
  , gbcoverDefault

  , CanDeriveBareB
  )

where

import Barbies.Internal.Functor (FunctorB(..))
import Barbies.Internal.Wear(Bare, Covered, Wear)
import Data.Functor.Identity (Identity(..))

import Data.Coerce (coerce)
import Data.Generics.GenericN
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat)


-- | Class of Barbie-types defined using 'Wear' and can therefore
--   have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB (b Covered) => BareB b where
    bstrip :: b Covered Identity -> b Bare Identity
    bcover :: b Bare Identity -> b Covered Identity

    default bstrip :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
    bstrip = gbstripDefault

    default bcover :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
    bcover = gbcoverDefault

-- | Generalization of 'bstrip' to arbitrary functors
bstripFrom :: BareB b => (forall a . f a -> a) -> b Covered f -> b Bare Identity
bstripFrom f
  = bstrip . bmap (Identity . f)

-- | Generalization of 'bcover' to arbitrary functors
bcoverWith :: BareB b => (forall a . a -> f a) -> b Bare Identity -> b Covered f
bcoverWith f
  = bmap (f . runIdentity) . bcover


-- | All types that admit a generic FunctorB' instance, and have all
--   their occurrences of 'f' under a 'Wear' admit a generic 'BareB'
--   instance.
type CanDeriveBareB b
  = ( GenericN (b Bare Identity)
    , GenericN (b Covered Identity)
    , GBareB 0 (RepN (b Covered Identity)) (RepN (b Bare Identity))
    )

-- | Default implementation of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
gbstripDefault
  = toN . gbstrip (Proxy @0) . fromN
{-# INLINE gbstripDefault #-}

-- | Default implementation of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
gbcoverDefault
  = toN . gbcover (Proxy @0) . fromN
{-# INLINE gbcoverDefault #-}


class GBareB (n :: Nat) repbi repbb where
  gbstrip :: Proxy n -> repbi x -> repbb x
  gbcover :: Proxy n -> repbb x -> repbi x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GBareB n repbi repbb => GBareB n (M1 i k repbi) (M1 i k repbb) where
  gbstrip pn = M1 . gbstrip pn . unM1
  {-# INLINE gbstrip #-}

  gbcover pn = M1 . gbcover pn . unM1
  {-# INLINE gbcover #-}


instance GBareB n V1 V1 where
  gbstrip _ _ = undefined
  gbcover _ _ = undefined

instance GBareB n U1 U1 where
  gbstrip _ = id
  {-# INLINE gbstrip #-}

  gbcover _ = id
  {-# INLINE gbcover #-}


instance (GBareB n l l', GBareB n r r') => GBareB n (l :*: r) (l' :*: r') where
  gbstrip pn (l :*: r) = (gbstrip pn l) :*: gbstrip pn r
  {-# INLINE gbstrip #-}

  gbcover pn (l :*: r) = (gbcover pn l) :*: gbcover pn r
  {-# INLINE gbcover #-}


instance (GBareB n l l', GBareB n r r') => GBareB n (l :+: r) (l' :+: r') where
  gbstrip pn = \case
    L1 l -> L1 (gbstrip pn l)
    R1 r -> R1 (gbstrip pn r)
  {-# INLINE gbstrip #-}

  gbcover pn = \case
    L1 l -> L1 (gbcover pn l)
    R1 r -> R1 (gbcover pn r)
  {-# INLINE gbcover #-}

-- -- --------------------------------
-- -- The interesting cases
-- -- --------------------------------

type P = Param

instance GBareB n (Rec (P n Identity a) (Identity a)) (Rec a a) where
  gbstrip _ = coerce
  {-# INLINE gbstrip #-}

  gbcover _ = coerce
  {-# INLINE gbcover #-}


instance BareB b => GBareB 0 (Rec (b Covered (P 0 Identity)) (b Covered Identity))
                             (Rec (b Bare    (P 0 Identity)) (b Bare    Identity)) where
  gbstrip _ = Rec . K1 . bstrip . unK1 . unRec
  {-# INLINE gbstrip #-}

  gbcover _ = Rec . K1 .  bcover . unK1 . unRec
  {-# INLINE gbcover #-}


instance (Functor h, BareB b)
    => GBareB 0 (Rec (h (b Covered (P 0 Identity))) (h (b Covered Identity)))
                (Rec (h (b Bare    (P 0 Identity))) (h (b Bare    Identity))) where
  gbstrip _ = Rec . K1 . fmap bstrip . unK1 . unRec
  {-# INLINE gbstrip #-}

  gbcover _ = Rec . K1 . fmap bcover . unK1 . unRec
  {-# INLINE gbcover #-}


instance repbi ~ repbb => GBareB n (Rec repbi repbi) (Rec repbb repbb) where
  gbstrip _ = id
  {-# INLINE gbstrip #-}

  gbcover _ = id
  {-# INLINE gbcover #-}
