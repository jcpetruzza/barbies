{-# LANGUAGE TypeFamilies        #-}
module Data.Barbie.Internal.Functor
  ( FunctorB(..)

  , GFunctorB
  , gbmapDefault
  , CanDeriveGenericInstance
  )

where

import Data.Barbie.Internal.Tag (Tag(..), CoercibleTag(..))

import GHC.Generics

-- | Barbie-types that can be mapped over. Instances of 'FunctorB' should
--   satisfy the following laws:
--
-- @
--   'bmap' 'id' = 'id'
--   'bmap' f . 'bmap' g = 'bmap' (f . g)
-- @
--
-- There is a default 'bmap' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorB b where
  bmap :: (forall a . f a -> g a) -> b f -> b g

  default bmap
    :: forall f g
    .  CanDeriveGenericInstance b f g
    => (forall a . f a -> g a) -> b f -> b g
  bmap = gbmapDefault

data F_
data G_

type F = Tag F_
type G = Tag G_

-- | Intuivively, the requirements to have @'FunctorB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'FunctorB' instance.
--
--     * Recursive usages of @B f@ are allowed to appear as argument to a
--       'Functor' (e.g. @'Maybe' (B f)')
type CanDeriveGenericInstance b f g
  = ( Generic (b (F f))
    , Generic (b (G g))
    , CoercibleTag F b f
    , CoercibleTag G b g
    , GFunctorB f g (Rep (b (F f))) (Rep (b (G g)))
    )

-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: CanDeriveGenericInstance b f g
  => (forall a . f a -> g a) -> b f -> b g
gbmapDefault f
  = coerceUntag @G
      . to
      . gbmap f
      . from
      . coerceTag @F


class GFunctorB f g repbf repbg where
  gbmap :: (forall a . f a -> g a) -> repbf x -> repbg x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GFunctorB f g bf bg => GFunctorB f g (M1 i c bf) (M1 i c bg) where
  gbmap h = M1 . gbmap h . unM1
  {-# INLINE gbmap #-}

instance GFunctorB f g V1 V1 where
  gbmap _ _ = undefined

instance GFunctorB f g U1 U1 where
  gbmap _ = id
  {-# INLINE gbmap #-}

instance(GFunctorB f g l l', GFunctorB f g r r') => GFunctorB f g (l :*: r) (l' :*: r') where
  gbmap h (l :*: r) = (gbmap h l) :*: gbmap h r
  {-# INLINE gbmap #-}

instance(GFunctorB f g l l', GFunctorB f g r r') => GFunctorB f g (l :+: r) (l' :+: r') where
  gbmap h = \case
    L1 l -> L1 (gbmap h l)
    R1 r -> R1 (gbmap h r)
  {-# INLINE gbmap #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance GFunctorB f g (Rec0 (F f a)) (Rec0 (G g a)) where
  gbmap h (K1 (Tag fa)) = K1 (Tag (h fa))
  {-# INLINE gbmap #-}

instance FunctorB b => GFunctorB f g (Rec0 (b (F f))) (Rec0 (b (G g))) where
  gbmap h (K1 bf) = K1 (bmap (Tag . h . unTag) bf)
  {-# INLINE gbmap #-}

instance (Functor h, FunctorB b) => GFunctorB f g (Rec0 (h (b (F f)))) (Rec0 (h (b (G g)))) where
  gbmap h (K1 hbf) = K1 (fmap (bmap (Tag . h . unTag)) hbf)
  {-# INLINE gbmap #-}

instance {-# OVERLAPPABLE #-} bf ~ bg => GFunctorB f g (Rec0 bf) (Rec0 bg) where
  gbmap _ = id
  {-# INLINE gbmap #-}
