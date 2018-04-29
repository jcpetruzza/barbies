{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Barbie.Internal.Functor
  ( FunctorB(..)

  , GFunctorB
  , gbmapDefault
  , CanDeriveGenericInstance
  )

where

import Data.Barbie.Internal.Generics
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
    :: CanDeriveGenericInstance b
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
type CanDeriveGenericInstance b
  = ( Generic (b (Target F))
    , Generic (b (Target G))
    , GFunctorB (Rep (b (Target F)))
    , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
    )


-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: CanDeriveGenericInstance b
  => (forall a . f a -> g a) -> b f -> b g
gbmapDefault f b
  = unsafeUntargetBarbie @G $ to $ gbmap f $ from (unsafeTargetBarbie @F b)


class GFunctorB b where
  gbmap :: (forall a . f a -> g a) -> b x -> Repl (Target F) (Target G) b x


data F a
data G a

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GFunctorB x => GFunctorB (M1 i c x) where
  {-# INLINE gbmap #-}
  gbmap f (M1 x) = M1 (gbmap f x)

instance GFunctorB V1 where
  gbmap _ _ = undefined

instance GFunctorB U1 where
  {-# INLINE gbmap #-}
  gbmap _ u1 = u1

instance (GFunctorB l, GFunctorB r) => GFunctorB (l :*: r) where
  {-# INLINE gbmap #-}
  gbmap f (l :*: r)
    = (gbmap f l) :*: gbmap f r

instance (GFunctorB l, GFunctorB r) => GFunctorB (l :+: r) where
  {-# INLINE gbmap #-}
  gbmap f = \case
    L1 l -> L1 (gbmap f l)
    R1 r -> R1 (gbmap f r)


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GFunctorB (K1 R (Target F a)) where
  {-# INLINE gbmap #-}
  gbmap f (K1 fa)
    = K1 $ unsafeTarget @G (f $ unsafeUntarget @F fa)

instance {-# OVERLAPPING #-} FunctorB b => GFunctorB (K1 R (b (Target F))) where
  {-# INLINE gbmap #-}
  gbmap f (K1 bf)
    = K1 $ bmap (unsafeTarget @G . f . unsafeUntarget @F) bf

instance {-# OVERLAPPING #-}
  ( Functor h
  , FunctorB b
  , Repl (Target F) (Target G) (K1 R (h (b (Target F)))) -- shouldn't be
      ~ (K1 R (h (b (Target G))))  -- necessary but ghc chokes otherwise
  )
  => GFunctorB (K1 R (h (b (Target F)))) where
  {-# INLINE gbmap #-}
  gbmap f (K1 hbf)
    = K1 (fmap (unsafeTargetBarbie @G . bmap f . unsafeUntargetBarbie @F) hbf)

instance (K1 i c) ~ Repl (Target F) (Target G) (K1 i c) => GFunctorB (K1 i c) where
  {-# INLINE gbmap #-}
  gbmap _ k1 = k1
