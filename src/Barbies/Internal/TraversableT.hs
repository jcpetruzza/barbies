{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.TraversableT
  ( TraversableT(..)
  , tfor
  , ttraverse_
  , tfor_
  , tsequence
  , tsequence'
  , tfoldMap

  , CanDeriveTraversableT
  , ttraverseDefault
  )

where

import Barbies.Generics.Traversable(GTraversable(..))
import Barbies.Internal.FunctorT(FunctorT (..))
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
--  t . 'ttraverse' f   = 'ttraverse' (t . f)  -- naturality
-- 'ttraverse' 'Data.Functor.Identity' = 'Data.Functor.Identity'           -- identity
-- 'ttraverse' ('Compose' . 'fmap' g . f) = 'Compose' . 'fmap' ('ttraverse' g) . 'ttraverse' f -- composition
-- @
--
-- There is a default 'ttraverse' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorT t => TraversableT (t :: (k -> Type) -> k' -> Type) where
  ttraverse
    :: Applicative e
    => (forall a . f a -> e (g a))
    -> t f x -> e (t g x)

  default ttraverse
    :: ( Applicative e, CanDeriveTraversableT t f g x)
    => (forall a . f a -> e (g a)) -> t f x -> e (t g x)
  ttraverse = ttraverseDefault

-- | 'ttraverse' with the arguments flipped. Useful when the traversing function is a large lambda:
--
-- @
-- tfor someTransformer $ \fa -> ...
-- @
--
-- @since 2.1.0.0
tfor
  :: (TraversableT t, Applicative e)
  => t f x
  -> (forall a . f a -> e (g a))
  -> e (t g x)
tfor t f = ttraverse f t


-- | Map each element to an action, evaluate these actions from left to right,
--   and ignore the results.
ttraverse_
  :: (TraversableT t, Applicative e)
  => (forall a. f a -> e c)
  -> t f x -> e ()
ttraverse_ f
  = void . ttraverse (fmap (const $ Const ()) . f)

-- | 'ttraverse_' with the arguments flipped.
--
-- @since 2.1.0.0
tfor_
  :: (TraversableT t, Applicative e)
  => t f x
  -> (forall a . f a -> e c)
  -> e ()
tfor_ t f = ttraverse_ f t


-- | Evaluate each action in the structure from left to right,
--   and collect the results.
tsequence
  :: (Applicative e, TraversableT t)
  => t (Compose e f) x
  -> e (t f x)
tsequence
  = ttraverse getCompose

-- | A version of 'tsequence' with @f@ specialized to 'Identity'.
tsequence'
  :: (Applicative e, TraversableT t)
  => t e x
  -> e (t Identity x)
tsequence'
  = ttraverse (fmap Identity)


-- | Map each element to a monoid, and combine the results.
tfoldMap
  :: ( TraversableT t, Monoid m)
  => (forall a. f a -> m)
  -> t f x
  -> m
tfoldMap f
  = execWr . ttraverse_ (tell . f)


-- | @'CanDeriveTraversableT' T f g x@ is in practice a predicate about @T@ only.
--   It is analogous to 'Barbies.Internal.FunctorT.CanDeriveFunctorT', so it
--   essentially requires the following to hold, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (T f x)@.
--
--     * @T f x@ can contain fields of type @t f x@ as long as there exists a
--       @'TraversableT' t@ instance. In particular, recursive usages of @T f x@
--       are allowed.
--
--     * @T f x@ can also contain usages of @t f x@ under a @'Traversable' h@.
--       For example, one could use @'Maybe' (T f x)@ when defining @T f x@.
type CanDeriveTraversableT t f g x
  = ( GenericP 1 (t f x)
    , GenericP 1 (t g x)
    , GTraversable 1 f g (RepP 1 (t f x)) (RepP 1 (t g x))
    )

-- | Default implementation of 'ttraverse' based on 'Generic'.
ttraverseDefault
  :: forall t f g e x
  .  (Applicative e, CanDeriveTraversableT t f g x)
  => (forall a . f a -> e (g a))
  -> t f x -> e (t g x)
ttraverseDefault h
  = fmap (toP (Proxy @1)) . gtraverse (Proxy @1) h . fromP (Proxy @1)
{-# INLINE ttraverseDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for TraversableT
-- -----------------------------------------------------------

type P = Param

instance
  ( TraversableT t
  ) => GTraversable 1 f g (Rec (t (P 1 f) x) (t f x))
                          (Rec (t (P 1 g) x) (t g x))
  where
  gtraverse _ h
    = fmap (Rec . K1) . ttraverse h . unK1 . unRec
  {-# INLINE gtraverse #-}

instance
   ( Traversable h
   , TraversableT t
   ) => GTraversable 1 f g (Rec (h (t (P 1 f) x)) (h (t f x)))
                           (Rec (h (t (P 1 g) x)) (h (t g x)))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (ttraverse h) . unK1 . unRec
  {-# INLINE gtraverse #-}


-- This instance is the same as the previous instance but for nested
-- Traversables.
instance
   ( Traversable h
   , Traversable m
   , TraversableT t
   ) => GTraversable 1 f g (Rec (m (h (t (P 1 f) x))) (m (h (t f x))))
                           (Rec (m (h (t (P 1 g) x))) (m (h (t g x))))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse (traverse (ttraverse h)) . unK1 . unRec
  {-# INLINE gtraverse #-}


-- -----------------------------------------------------------
-- Instances for base types
-- -----------------------------------------------------------

instance Traversable f => TraversableT (Compose f) where
  ttraverse h (Compose fga)
    = Compose <$> traverse h fga
  {-# INLINE ttraverse #-}

instance TraversableT (Product f) where
  ttraverse h (Pair fa ga) = Pair fa <$> h ga
  {-# INLINE ttraverse #-}

instance TraversableT (Sum f) where
  ttraverse h = \case
    InL fa -> pure $ InL fa
    InR ga -> InR <$> h ga
  {-# INLINE ttraverse #-}

-- -----------------------------------------------------------
-- Instances for transformers types
-- -----------------------------------------------------------

instance TraversableT Backwards where
  ttraverse h (Backwards fa)
    = Backwards <$> h fa
  {-# INLINE ttraverse #-}

instance TraversableT Lift where
  ttraverse h = \case
    Pure  a  -> pure $ Pure a
    Other fa -> Other <$> h fa
  {-# INLINE ttraverse #-}

instance TraversableT Reverse where
  ttraverse h (Reverse fa) = Reverse <$> h fa
  {-# INLINE ttraverse #-}

instance TraversableT (ExceptT e) where
  ttraverse h (ExceptT mea)
    = ExceptT <$> h mea
  {-# INLINE ttraverse #-}

instance TraversableT IdentityT where
  ttraverse h (IdentityT ma)
    = IdentityT <$> h ma
  {-# INLINE ttraverse #-}

instance TraversableT MaybeT where
  ttraverse h (MaybeT mma)
    = MaybeT <$> h mma
  {-# INLINE ttraverse #-}

instance TraversableT (Lazy.WriterT w) where
  ttraverse h (Lazy.WriterT maw)
    = Lazy.WriterT <$> h maw
  {-# INLINE ttraverse #-}

instance TraversableT (Strict.WriterT w) where
  ttraverse h (Strict.WriterT maw)
    = Strict.WriterT <$> h maw
  {-# INLINE ttraverse #-}
