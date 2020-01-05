{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.FunctorB
  ( FunctorB(..)
  , gbmapDefault
  , CanDeriveFunctorB
  )

where

import Barbies.Generics.Functor (GFunctor(..))

import Data.Functor.Compose   (Compose (..))
import Data.Functor.Const     (Const (..))
import Data.Functor.Constant  (Constant (..))
import Data.Functor.Product   (Product (..))
import Data.Functor.Sum       (Sum (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Kind              (Type)

-- | Barbie-types that can be mapped over. Instances of 'FunctorB' should
--   satisfy the following laws:
--
-- @
-- 'bmap' 'id' = 'id'
-- 'bmap' f . 'bmap' g = 'bmap' (f . g)
-- @
--
-- There is a default 'bmap' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorB (b :: (k -> Type) -> Type) where
  bmap :: (forall a . f a -> g a) -> b f -> b g

  default bmap
    :: forall f g
    .  CanDeriveFunctorB b f g
    => (forall a . f a -> g a) -> b f -> b g
  bmap = gbmapDefault

-- | @'CanDeriveFunctorB' B f g@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B f@ can contain fields of type @b f@ as long as there exists a
--       @'FunctorB' b@ instance. In particular, recursive usages of @B f@
--       are allowed.
--
--     * @B f@ can also contain usages of @b f@ under a @'Functor' h@.
--       For example, one could use @'Maybe' (B f)@ when defining @B f@.
type CanDeriveFunctorB b f g
  = ( GenericP 0 (b f)
    , GenericP 0 (b g)
    , GFunctor 0 f g (RepP 0 (b f)) (RepP 0 (b g))
    )

-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: CanDeriveFunctorB b f g
  => (forall a . f a -> g a) -> b f -> b g
gbmapDefault f
  = toP (Proxy @0) . gmap (Proxy @0) f . fromP (Proxy @0)
{-# INLINE gbmapDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorB
-- -----------------------------------------------------------

type P = Param

-- b' is b, maybe with 'Param' annotations
instance
  ( FunctorB b
  ) => GFunctor 0 f g (Rec (b' (P 0 f)) (b f))
                      (Rec (b' (P 0 g)) (b g))
  where
  gmap _ h (Rec (K1 bf)) = Rec (K1 (bmap h bf))
  {-# INLINE gmap #-}

-- h' and b' are essentially  h and b, but maybe
-- with 'Param' annotations
instance
  ( Functor h
  , FunctorB b
  ) => GFunctor 0 f g (Rec (h' (b' (P 0 f))) (h (b f)))
                      (Rec (h' (b' (P 0 g))) (h (b g)))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (bmap h) hbf))
  {-# INLINE gmap #-}

-- This is the same as the previous instance, but for nested (normal-flavoured)
-- functors.
instance
  ( Functor h
  , Functor m
  , FunctorB b
  ) => GFunctor 0 f g (Rec (m' (h' (b' (P 0 f)))) (m (h (b f))))
                      (Rec (m' (h' (b' (P 0 g)))) (m (h (b g))))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (fmap (bmap h)) hbf))
  {-# INLINE gmap #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance FunctorB Proxy where
  bmap _ _ = Proxy
  {-# INLINE bmap #-}

instance (FunctorB a, FunctorB b) => FunctorB (Product a b) where
  bmap f (Pair x y) = Pair (bmap f x) (bmap f y)
  {-# INLINE bmap #-}

instance (FunctorB a, FunctorB b) => FunctorB (Sum a b) where
  bmap f (InL x) = InL (bmap f x)
  bmap f (InR x) = InR (bmap f x)
  {-# INLINE bmap #-}

instance FunctorB (Const x) where
  bmap _ (Const x) = Const x
  {-# INLINE bmap #-}

instance (Functor f, FunctorB b) => FunctorB (f `Compose` b) where
  bmap h (Compose x) = Compose (bmap h <$> x)
  {-# INLINE bmap #-}


-- --------------------------------
-- Instances for transformer types
-- --------------------------------

instance FunctorB (Constant x) where
  bmap _ (Constant x) = Constant x
  {-# INLINE bmap #-}
