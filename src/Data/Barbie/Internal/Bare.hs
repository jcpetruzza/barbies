{-# LANGUAGE TypeFamilies       #-}
module Data.Barbie.Internal.Bare
  ( Wear, Bare, Covered
  , BareB(..)
  , bstripFrom, bcoverWith

  , GBareB(..)
  , gbstripDefault
  , gbcoverDefault

  , CanDeriveBareB
  )

where

import Data.Barbie.Internal.Functor (FunctorB(..))
import Data.Barbie.Internal.Wear(Bare, Covered, Wear)
import Data.Functor.Identity (Identity(..))

import Data.Coerce (coerce)
import Data.Generics.GenericN (GenericN(..), toN, fromN, Rec(..), Param)
import GHC.Generics


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
    , GBareB (RepN (b Covered Identity)) (RepN (b Bare Identity))
    )

-- | Default implementation of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
gbstripDefault
  = toN . gbstrip . fromN
{-# INLINE gbstripDefault #-}

-- | Default implementation of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
gbcoverDefault
  = toN . gbcover . fromN
{-# INLINE gbcoverDefault #-}


class GBareB repbi repbb where
  gbstrip :: repbi x -> repbb x
  gbcover :: repbb x -> repbi x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GBareB repbi repbb => GBareB (M1 i k repbi) (M1 i k repbb) where
  gbstrip = M1 . gbstrip . unM1
  {-# INLINE gbstrip #-}

  gbcover = M1 . gbcover . unM1
  {-# INLINE gbcover #-}


instance GBareB V1 V1 where
  gbstrip _ = undefined
  gbcover _ = undefined

instance GBareB U1 U1 where
  gbstrip = id
  {-# INLINE gbstrip #-}

  gbcover = id
  {-# INLINE gbcover #-}


instance (GBareB l l', GBareB r r') => GBareB (l :*: r) (l' :*: r') where
  gbstrip (l :*: r) = (gbstrip l) :*: gbstrip r
  {-# INLINE gbstrip #-}

  gbcover (l :*: r) = (gbcover l) :*: gbcover r
  {-# INLINE gbcover #-}


instance (GBareB l l', GBareB r r') => GBareB (l :+: r) (l' :+: r') where
  gbstrip = \case
    L1 l -> L1 (gbstrip l)
    R1 r -> R1 (gbstrip r)
  {-# INLINE gbstrip #-}

  gbcover = \case
    L1 l -> L1 (gbcover l)
    R1 r -> R1 (gbcover r)
  {-# INLINE gbcover #-}

-- -- --------------------------------
-- -- The interesting cases
-- -- --------------------------------

type P = Param 0

instance GBareB (Rec (P Identity a) (Identity a)) (Rec a a) where
  gbstrip = coerce
  {-# INLINE gbstrip #-}

  gbcover = coerce
  {-# INLINE gbcover #-}


instance BareB b => GBareB (Rec (b Covered (P Identity)) (b Covered Identity))
                           (Rec (b Bare    (P Identity)) (b Bare    Identity)) where
  gbstrip = Rec . K1 . bstrip . unK1 . unRec
  {-# INLINE gbstrip #-}

  gbcover = Rec . K1 .  bcover . unK1 . unRec
  {-# INLINE gbcover #-}


instance (Functor h, BareB b)
    => GBareB (Rec (h (b Covered (P Identity))) (h (b Covered Identity)))
              (Rec (h (b Bare    (P Identity))) (h (b Bare    Identity))) where
  gbstrip = Rec . K1 . fmap bstrip . unK1 . unRec
  {-# INLINE gbstrip #-}

  gbcover = Rec . K1 . fmap bcover . unK1 . unRec
  {-# INLINE gbcover #-}


instance repbi ~ repbb => GBareB (Rec repbi repbi) (Rec repbb repbb) where
  gbstrip = id
  {-# INLINE gbstrip #-}

  gbcover = id
  {-# INLINE gbcover #-}
