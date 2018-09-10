{-# LANGUAGE TypeFamilies       #-}
module Data.Barbie.Internal.Bare
  ( Wear, Bare
  , BareB(..)
  , bstripFrom, bcoverWith

  , GBareB(..)
  , gbstripDefault
  , gbcoverDefault

  , CanDeriveGenericInstance
  )

where

import Data.Barbie.Internal.Functor (FunctorB(..))
import Data.Barbie.Internal.Tag (Tag(..), CoercibleTag(..))
import Data.Barbie.Internal.Wear
import Data.Functor.Identity (Identity(..))

import GHC.Generics
import Data.Coerce (coerce)


-- | Class of Barbie-types defined using 'Wear' and can therefore
--   have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB b => BareB b where
    bstrip :: b Identity -> b Bare
    bcover :: b Bare -> b Identity

    default bstrip :: CanDeriveGenericInstance b => b Identity -> b Bare
    bstrip = gbstripDefault

    default bcover :: CanDeriveGenericInstance b => b Bare -> b Identity
    bcover = gbcoverDefault

-- | Generalization of 'bstrip' to arbitrary functors
bstripFrom :: BareB b => (forall a . f a -> a) -> b f -> b Bare
bstripFrom f
  = bstrip . bmap (Identity . f)

-- | Generalization of 'bcover' to arbitrary functors
bcoverWith :: BareB b => (forall a . a -> f a) -> b Bare -> b f
bcoverWith f
  = bmap (f . runIdentity) . bcover


data I_

type I = Tag I_

-- | All types that admit a generic FunctorB' instance, and have all
--   their occurrences of 'f' under a 'Wear' admit a generic 'BareB'
--   instance.
type CanDeriveGenericInstance b
  = ( Generic (b (I Identity))
    , Generic (b Bare)
    , CoercibleTag I b Identity
    , GBareB (Rep (b (I Identity))) (Rep (b Bare))
    )

-- | Default implementatio of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveGenericInstance b => b Identity -> b Bare
gbstripDefault
  = to . gbstrip . from . coerceTag @I

-- | Default implementatio of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveGenericInstance b => b Bare -> b Identity
gbcoverDefault
  = coerceUntag @I . to . gbcover . from


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

instance GBareB (Rec0 (I Identity a)) (Rec0 a) where
  gbstrip = coerce
  {-# INLINE gbstrip #-}

  gbcover = coerce
  {-# INLINE gbcover #-}


instance (BareB b, CoercibleTag I b Identity) => GBareB (Rec0 (b (I Identity))) (Rec0 (b Bare)) where
  gbstrip = K1 . bstrip . coerceUntag @I . unK1
  {-# INLINE gbstrip #-}

  gbcover = K1 . coerceTag @I . bcover . unK1
  {-# INLINE gbcover #-}


instance
  (Functor h, BareB b, CoercibleTag I b Identity)
    => GBareB (Rec0 (h (b (I Identity)))) (Rec0 (h (b Bare))) where
  gbstrip = K1 . fmap (bstrip . coerceUntag @I) . unK1
  {-# INLINE gbstrip #-}

  gbcover = K1 . fmap (coerceTag @I . bcover) . unK1
  {-# INLINE gbcover #-}


instance {-# OVERLAPPABLE #-} repbi ~ repbb => GBareB (Rec0 repbi) (Rec0 repbb) where
  gbstrip = id
  {-# INLINE gbstrip #-}

  gbcover = id
  {-# INLINE gbcover #-}
