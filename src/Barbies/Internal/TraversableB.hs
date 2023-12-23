{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.TraversableB
  ( TraversableB(..)
  , bfor
  , btraverse_
  , bfor_
  , bsequence
  , bsequence'
  , bfoldMap

  , CanDeriveTraversableB
  , gbtraverseDefault
  )

where

import Barbies.Generics.Traversable(GTraversable(..))
import Barbies.Internal.FunctorB(FunctorB (..))
import Barbies.Internal.Writer(execWr, tell)

import Data.Functor           (void)
import Data.Functor.Compose   (Compose (..))
import Data.Functor.Const     (Const (..))
import Data.Functor.Constant  (Constant (..))
import Data.Functor.Identity  (Identity (..))
import Data.Functor.Product   (Product (..))
import Data.Functor.Sum       (Sum (..))
import Data.Kind              (Type)
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))

-- | Barbie-types that can be traversed from left to right. Instances should
--   satisfy the following laws:
--
-- @
--  t . 'btraverse' f   = 'btraverse' (t . f)  -- naturality
-- 'btraverse' 'Data.Functor.Identity' = 'Data.Functor.Identity'           -- identity
-- 'btraverse' ('Compose' . 'fmap' g . f) = 'Compose' . 'fmap' ('btraverse' g) . 'btraverse' f -- composition
-- @
--
-- There is a default 'btraverse' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorB b => TraversableB (b :: (k -> Type) -> Type) where
  btraverse :: Applicative e => (forall a . f a -> e (g a)) -> b f -> e (b g)

  default btraverse
    :: ( Applicative e, CanDeriveTraversableB b f g)
    => (forall a . f a -> e (g a))
    -> b f
    -> e (b g)
  btraverse = gbtraverseDefault

-- | 'btraverse' with the arguments flipped. Useful when the traversing function is a large lambda:
--
-- @
-- bfor someBarbie $ \fa -> ...
-- @
--
-- @since 2.1.1.0
bfor
  :: (TraversableB b, Applicative e)
  => b f
  -> (forall a . f a -> e (g a))
  -> e (b g)
bfor b f = btraverse f b


-- | Map each element to an action, evaluate these actions from left to right,
--   and ignore the results.
btraverse_
  :: (TraversableB b, Applicative e)
  => (forall a. f a -> e c)
  -> b f
  -> e ()
btraverse_ f
  = void . btraverse (fmap (const $ Const ()) . f)

-- | 'btraverse_' with the arguments flipped.
--
-- @since 2.1.1.0
bfor_
  :: (TraversableB b, Applicative e)
  => b f
  -> (forall a. f a -> e c)
  -> e ()
bfor_ b f = btraverse_ f b


-- | Evaluate each action in the structure from left to right,
--   and collect the results.
bsequence :: (Applicative e, TraversableB b) => b (Compose e f) -> e (b f)
bsequence
  = btraverse getCompose

-- | A version of 'bsequence' with @f@ specialized to 'Identity'.
bsequence' :: (Applicative e, TraversableB b) => b e -> e (b Identity)
bsequence'
  = btraverse (fmap Identity)


-- | Map each element to a monoid, and combine the results.
bfoldMap :: (TraversableB b, Monoid m) => (forall a. f a -> m) -> b f -> m
bfoldMap f
  = execWr . btraverse_ (tell . f)


-- | @'CanDeriveTraversableB' B f g@ is in practice a predicate about @B@ only.
--   It is analogous to 'Barbies.Internal.FunctorB.CanDeriveFunctorB', so it
--   essentially requires the following to hold, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B f@ can contain fields of type @b f@ as long as there exists a
--       @'TraversableB' b@ instance. In particular, recursive usages of @B f@
--       are allowed.
--
--     * @B f@ can also contain usages of @b f@ under a @'Traversable' h@.
--       For example, one could use @'Maybe' (B f)@ when defining @B f@.
type CanDeriveTraversableB b f g
  = ( GenericP 0 (b f)
    , GenericP 0 (b g)
    , GTraversable 0 f g (RepP 0 (b f)) (RepP 0 (b g))
    )

-- | Default implementation of 'btraverse' based on 'Generic'.
gbtraverseDefault
  :: forall b f g e
  .  (Applicative e, CanDeriveTraversableB b f g)
  => (forall a . f a -> e (g a))
  -> b f -> e (b g)
gbtraverseDefault h
  = fmap (toP (Proxy @0)) . gtraverse (Proxy @0) h . fromP (Proxy @0)
{-# INLINE gbtraverseDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for TraversableB
-- -----------------------------------------------------------

type P = Param

instance
  ( TraversableB b
  ) => GTraversable 0 f g (Rec (b (P 0 f)) (b f))
                          (Rec (b (P 0 g)) (b g))
  where
  gtraverse _ h
    = fmap (Rec . K1) . btraverse h . unK1 . unRec
  {-# INLINE gtraverse #-}

instance
   ( Traversable h
   , TraversableB b
   ) => GTraversable 0 f g (Rec (h (b (P 0 f))) (h (b f)))
                           (Rec (h (b (P 0 g))) (h (b g)))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (btraverse h) . unK1 . unRec
  {-# INLINE gtraverse #-}

-- This instance is the same as the previous instance but for nested
-- Traversables.
instance
   ( Traversable h
   , Traversable m
   , TraversableB b
   ) => GTraversable 0 f g (Rec (m (h (b (P 0 f)))) (m (h (b f))))
                           (Rec (m (h (b (P 0 g)))) (m (h (b g))))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (traverse (btraverse h)) . unK1 . unRec
  {-# INLINE gtraverse #-}


-- -----------------------------------------------------------
-- Instances for base types
-- -----------------------------------------------------------

instance TraversableB Proxy where
  btraverse _ _ = pure Proxy
  {-# INLINE btraverse #-}

instance (TraversableB a, TraversableB b) => TraversableB (Product a b) where
  btraverse f (Pair x y) = Pair <$> btraverse f x <*> btraverse f y
  {-# INLINE btraverse #-}

instance (TraversableB a, TraversableB b) => TraversableB (Sum a b) where
  btraverse f (InL x) = InL <$> btraverse f x
  btraverse f (InR x) = InR <$> btraverse f x
  {-# INLINE btraverse #-}

instance TraversableB (Const a) where
  btraverse _ (Const x) = pure (Const x)
  {-# INLINE btraverse #-}

instance (Traversable f, TraversableB b) => TraversableB (f `Compose` b) where
  btraverse h (Compose x)
    = Compose <$> traverse (btraverse h) x
  {-# INLINE btraverse #-}

-- -----------------------------------------------------------
-- Instances for transformer types
-- -----------------------------------------------------------

instance TraversableB (Constant a) where
  btraverse _ (Constant x) = pure (Constant x)
  {-# INLINE btraverse #-}
