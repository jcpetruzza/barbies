{-# LANGUAGE TypeFamilies #-}
module Data.Barbie.Internal.Functor
  ( FunctorB(..)

  , GFunctorB(..)
  , gbmapDefault
  , CanDeriveFunctorB
  )

where

import Data.Functor.Const     (Const (..))
import Data.Functor.Product   (Product (..))
import Data.Functor.Sum       (Sum (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))

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

type P0 = Param 0

instance GFunctorB f g (Rec (P0 f a) (f a))
                       (Rec (P0 g a) (g a)) where
  gbmap h (Rec (K1 fa)) = Rec (K1 (h fa))
  {-# INLINE gbmap #-}

instance
  ( SameOrParam b b'
  , FunctorB b'
  ) => GFunctorB f g (Rec (b (P0 f)) (b' f))
                     (Rec (b (P0 g)) (b' g)) where
  gbmap h (Rec (K1 bf)) = Rec (K1 (bmap h bf))
  {-# INLINE gbmap #-}

instance
  ( SameOrParam h h'
  , SameOrParam b b'
  , Functor h'
  , FunctorB b'
  ) => GFunctorB f g (Rec (h (b (P0 f))) (h' (b' f)))
                     (Rec (h (b (P0 g))) (h' (b' g))) where
  gbmap h (Rec (K1 hbf)) = Rec (K1 (fmap (bmap h) hbf))
  {-# INLINE gbmap #-}

instance GFunctorB f g (Rec x x) (Rec x x) where
  gbmap _ = id
  {-# INLINE gbmap #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance FunctorB Proxy where
  bmap _ _ = Proxy

instance (FunctorB a, FunctorB b) => FunctorB (Product a b) where
  bmap f (Pair x y) = Pair (bmap f x) (bmap f y)

instance (FunctorB a, FunctorB b) => FunctorB (Sum a b) where
  bmap f (InL x) = InL (bmap f x)
  bmap f (InR x) = InR (bmap f x)

instance FunctorB (Const x) where
  bmap _ (Const x) = Const x
