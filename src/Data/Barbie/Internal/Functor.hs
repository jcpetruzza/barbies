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
  bmap = bmapDefault


  bmapDefault :: (forall a . f a -> g a) -> b f -> b g

  default bmapDefault
    :: ( Generic (b (Target F))
       , Generic (b (Target G))
       , GFunctorB (Rep (b (Target F)))
       , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
       )
    => (forall a . f a -> g a)
    -> b f -> b g
  bmapDefault = gbmapDefault

-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: ( Generic (b (Target F))
     , Generic (b (Target G))
     , GFunctorB (Rep (b (Target F)))
     , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
     )
  => (forall a . f a -> g a)
  -> b f -> b g
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

instance (K1 i c) ~ Repl (Target F) (Target G) (K1 i c) => GFunctorB (K1 i c) where
  {-# INLINE gbmap #-}
  gbmap _ k1 = k1
