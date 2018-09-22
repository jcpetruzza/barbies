{-# LANGUAGE TypeFamilies        #-}
module Data.Barbie.Internal.Functor
  ( FunctorB(..)

  , GFunctorB
  , gbmapDefault
  , CanDeriveFunctorB
  )

where

import Data.Generics.GenericN (GenericN(..), toN, fromN, Rec(..), Param)

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
    .  CanDeriveFunctorB b f g
    => (forall a . f a -> g a) -> b f -> b g
  bmap = gbmapDefault

-- | Intuivively, the requirements to have @'FunctorB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'FunctorB' instance.
--
--     * Recursive usages of @B f@ are allowed to appear as argument to a
--       'Functor' (e.g. @'Maybe' (B f)')
type CanDeriveFunctorB b f g
  = ( GenericN (b f)
    , GenericN (b g)
    , GFunctorB f g (RepN (b f)) (RepN (b g))
    )

-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: CanDeriveFunctorB b f g
  => (forall a . f a -> g a) -> b f -> b g
gbmapDefault f
  = toN . gbmap f . fromN
{-# INLINE gbmapDefault #-}


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

type P = Param 0

instance GFunctorB f g (Rec (P f a) (f a)) (Rec (P g a) (g a)) where
  gbmap h (Rec (K1 fa)) = Rec (K1 (h fa))
  {-# INLINE gbmap #-}

instance FunctorB b => GFunctorB f g (Rec (b (P f)) (b f)) (Rec (b (P g)) (b g)) where
  gbmap h (Rec (K1 bf)) = Rec (K1 (bmap h bf))
  {-# INLINE gbmap #-}

instance (Functor h, FunctorB b) => GFunctorB f g (Rec (h (b (P f))) (h (b f))) (Rec (h (b (P g))) (h (b g))) where
  gbmap h (Rec (K1 hbf)) = Rec (K1 (fmap (bmap h) hbf))
  {-# INLINE gbmap #-}

instance bf ~ bg => GFunctorB f g (Rec bf bf) (Rec bg bg) where
  gbmap _ = id
  {-# INLINE gbmap #-}
