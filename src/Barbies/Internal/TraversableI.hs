{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.TraversableI
  ( TraversableI(..)
  , itraverse_
  , isequence
  , isequence'
  , ifoldMap

  , CanDeriveTraversableI
  , itraverseDefault
  )

where

import Barbies.Generics.Traversable(GTraversable(..))
import Barbies.Internal.FunctorI(FunctorI (..))
import Barbies.Internal.Writer(execWr, tell)

import Control.Applicative.Backwards(Backwards (..))
import Control.Applicative.Lift(Lift(..))
import Control.Monad.Trans.Except(ExceptT(..))
import Control.Monad.Trans.Identity(IdentityT(..))
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))

import Data.Functor           (void)
import Data.Functor.Compose   (Compose (..))
import Data.Functor.Const     (Const (..))
import Data.Functor.Identity  (Identity (..))
import Data.Functor.Product   (Product (..))
import Data.Functor.Reverse   (Reverse (..))
import Data.Functor.Sum       (Sum (..))
import Data.Kind              (Type)
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))

-- | Indexed-functors that can be traversed from left to right. Instances should
--   satisfy the following laws:
--
-- @
--  t . itraverse' f   = itraverse' (t . f)  -- naturality
-- itraverse' 'Data.Functor.Identity' = 'Data.Functor.Identity'           -- identity
-- itraverse' ('Compose' . 'fmap' g . f) = 'Compose' . 'fmap' (itraverse' g) . itraverse' f -- composition
-- @
--
-- There is a default itraverse' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorI b => TraversableI (b :: (k -> Type) -> k' -> Type) where
  itraverse
    :: Applicative t
    => (forall a . f a -> t (g a))
    -> (forall x . b f x -> t (b g x))

  default itraverse
    :: ( Applicative t, CanDeriveTraversableI b f g x)
    => (forall a . f a -> t (g a)) -> b f x -> t (b g x)
  itraverse = itraverseDefault



-- | Map each element to an action, evaluate these actions from left to right,
--   and ignore the results.
itraverse_
  :: (TraversableI b, Applicative t)
  => (forall a. f a -> t c)
  -> b f x -> t ()
itraverse_ f
  = void . itraverse (fmap (const $ Const ()) . f)


-- | Evaluate each action in the structure from left to right,
--   and collect the results.
isequence
  :: (Applicative f, TraversableI b)
  => b (Compose f g) x
  -> f (b g x)
isequence
  = itraverse getCompose

-- | A version of 'isequence' with @g@ specialized to 'Identity'.
isequence'
  :: (Applicative f, TraversableI b)
  => b f x
  -> f (b Identity x)
isequence'
  = itraverse (fmap Identity)


-- | Map each element to a monoid, and combine the results.
ifoldMap :: (TraversableI b, Monoid m) => (forall a. f a -> m) -> b f x -> m
ifoldMap f
  = execWr . itraverse_ (tell . f)


-- | @'CanDeriveTraversableI' B f g x@ is in practice a predicate about @B@ only.
--   It is analogous to 'Barbies.Internal.FunctorI.CanDeriveFunctorI', so it
--   essentially requires the following to hold, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f x)@.
--
--     * @B f x@ can contain fields of type @b f x@ as long as there exists a
--       @'TraversableI' b@ instance. In particular, recursive usages of @B f x@
--       are allowed.
--
--     * @B f x@ can also contain usages of @b f x@ under a @'Traversable' h@.
--       For example, one could use @'Maybe' (B f x)@ when defining @B f x@.
type CanDeriveTraversableI b f g x
  = ( GenericN (b f x)
    , GenericN (b g x)
    , GTraversable 1 f g (RepN (b f x)) (RepN (b g x))
    )

-- | Default implementation of itraverse' based on 'Generic'.
itraverseDefault
  :: forall b f g t x
  .  (Applicative t, CanDeriveTraversableI b f g x)
  => (forall a . f a -> t (g a))
  -> b f x -> t (b g x)
itraverseDefault h
  = fmap toN . gtraverse (Proxy @1) h . fromN
{-# INLINE itraverseDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for TraversableI
-- -----------------------------------------------------------

type P = Param

-- b' is b, maybe with 'Param' annotations
instance
  ( TraversableI b
  ) => GTraversable 1 f g (Rec (b' (P 1 f) (P 0 x)) (b f x))
                          (Rec (b' (P 1 g) (P 0 x)) (b g x))
  where
  gtraverse _ h
    = fmap (Rec . K1) . itraverse h . unK1 . unRec
  {-# INLINE gtraverse #-}

-- b' and h' are b and h, maybe with 'Param' annotations
instance
   ( Traversable h
   , TraversableI b
   ) => GTraversable 1 f g (Rec (h' (b' (P 1 f) (P 0 x))) (h (b f x)))
                           (Rec (h' (b' (P 1 g) (P 0 x))) (h (b g x)))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (itraverse h) . unK1 . unRec
  {-# INLINE gtraverse #-}


-- This instance is the same as the previous instance but for nested
-- Traversables.
instance
   ( Traversable h
   , Traversable m
   , TraversableI b
   ) => GTraversable 1 f g (Rec (m' (h' (b' (P 1 f) (P 0 x)))) (m (h (b f x))))
                           (Rec (m' (h' (b' (P 1 g) (P 0 x)))) (m (h (b g x))))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (traverse (itraverse h)) . unK1 . unRec
  {-# INLINE gtraverse #-}


-- -----------------------------------------------------------
-- Instances for base types
-- -----------------------------------------------------------

instance TraversableI (Product f) where
  itraverse h (Pair fa ga) = Pair fa <$> h ga
  {-# INLINE itraverse #-}

instance TraversableI (Sum f) where
  itraverse h = \case
    InL fa -> pure $ InL fa
    InR ga -> InR <$> h ga
  {-# INLINE itraverse #-}

-- -----------------------------------------------------------
-- Instances for transformers types
-- -----------------------------------------------------------

instance TraversableI Backwards where
  itraverse h (Backwards fa)
    = Backwards <$> h fa
  {-# INLINE itraverse #-}

instance TraversableI Lift where
  itraverse h = \case
    Pure  a  -> pure $ Pure a
    Other fa -> Other <$> h fa
  {-# INLINE itraverse #-}

instance TraversableI Reverse where
  itraverse h (Reverse fa) = Reverse <$> h fa
  {-# INLINE itraverse #-}

instance TraversableI (ExceptT e) where
  itraverse h (ExceptT mea)
    = ExceptT <$> h mea
  {-# INLINE itraverse #-}

instance TraversableI IdentityT where
  itraverse h (IdentityT ma)
    = IdentityT <$> h ma
  {-# INLINE itraverse #-}

instance TraversableI MaybeT where
  itraverse h (MaybeT mma)
    = MaybeT <$> h mma
  {-# INLINE itraverse #-}

instance TraversableI (Lazy.WriterT w) where
  itraverse h (Lazy.WriterT maw)
    = Lazy.WriterT <$> h maw
  {-# INLINE itraverse #-}

instance TraversableI (Strict.WriterT w) where
  itraverse h (Strict.WriterT maw)
    = Strict.WriterT <$> h maw
  {-# INLINE itraverse #-}
