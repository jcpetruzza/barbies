{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.FunctorI
  ( FunctorI(..)
  , gimapDefault
  , CanDeriveFunctorI
  )

where

import Barbies.Generics.Functor (GFunctor(..))

import Control.Applicative.Backwards(Backwards (..))
import Control.Applicative.Lift(Lift, mapLift )

import Control.Monad.Trans.Except(ExceptT, mapExceptT)
import Control.Monad.Trans.Identity(IdentityT, mapIdentityT)
import Control.Monad.Trans.Maybe(MaybeT, mapMaybeT)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST, mapRWST)
import Control.Monad.Trans.RWS.Strict as Strict (RWST, mapRWST)
import Control.Monad.Trans.Reader(ReaderT, mapReaderT)
import Control.Monad.Trans.State.Lazy as Lazy (StateT, mapStateT)
import Control.Monad.Trans.State.Strict as Strict (StateT, mapStateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT, mapWriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT, mapWriterT)

import Data.Functor.Product   (Product (..))
import Data.Functor.Reverse   (Reverse (..))
import Data.Functor.Sum       (Sum (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Kind              (Type)

-- | Functor from indexed-types to indexed-types. Instances of 'FunctorI' should
--   satisfy the following laws:
--
-- @
--   'imap' 'id' = 'id'
--   'imap' f . 'imap' g = 'imap' (f . g)
-- @
--
-- There is a default 'imap' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorI (b :: (k -> Type) -> k' -> Type) where
  imap :: (forall a . f a -> g a) -> (forall x. b f x -> b g x)

  default imap
    :: forall f g x
    .  CanDeriveFunctorI b f g x
    => (forall a . f a -> g a)
    -> b f x -> b g x
  imap = gimapDefault

-- | @'CanDeriveFunctorI' B f g x@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B f x@ can contain fields of type @b f y@ as long as there exists a
--       @'FunctorI' b@ instance. In particular, recursive usages of @B f y@
--       are allowed.
--
--     * @B f x@ can also contain usages of @b f y@ under a @'Functor' h@.
--       For example, one could use @'Maybe' (B f y)@ when defining @B f y@.
type CanDeriveFunctorI b f g x
  = ( GenericN (b f x)
    , GenericN (b g x)
    , GFunctor 1 f g (RepN (b f x)) (RepN (b g x))
    )

-- | Default implementation of 'imap' based on 'Generic'.
gimapDefault
  :: CanDeriveFunctorI b f g x
  => (forall a . f a -> g a)
  -> b f x -> b g x
gimapDefault f
  = toN . gmap (Proxy @1) f . fromN
{-# INLINE gimapDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorI
-- -----------------------------------------------------------

type P = Param

-- b' is b, maybe with 'Param' annotations
instance
  ( FunctorI b
  ) => GFunctor 1 f g (Rec (b' (P 1 f) (P 0 x)) (b f x))
                      (Rec (b' (P 1 g) (P 0 x)) (b g x))
  where
  gmap _ h (Rec (K1 bf)) = Rec (K1 (imap h bf))
  {-# INLINE gmap #-}

-- b' and h' are b and h, maybe with 'Param' annotations
instance
  ( Functor h
  , FunctorI b
  ) => GFunctor 1 f g (Rec (h' (b' (P 1 f) (P 0 x))) (h (b f x)))
                      (Rec (h' (b' (P 1 g) (P 0 x))) (h (b g x)))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (imap h) hbf))
  {-# INLINE gmap #-}


-- This is the same as the previous instance, but for nested (normal-flavoured)
-- functors.
instance
  ( Functor h
  , Functor m
  , FunctorI b
  ) => GFunctor 1 f g (Rec (m' (h' (b' (P 1 f) (P 0 x)))) (m (h (b f x))))
                      (Rec (m' (h' (b' (P 1 g) (P 0 x)))) (m (h (b g x))))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (fmap (imap h)) hbf))
  {-# INLINE gmap #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance FunctorI (Product f) where
  imap h (Pair fa ga) = Pair fa (h ga)
  {-# INLINE imap #-}

instance FunctorI (Sum f) where
  imap h = \case
    InL fa -> InL fa
    InR ga -> InR (h ga)
  {-# INLINE imap #-}

-- --------------------------------
-- Instances for transformers types
-- --------------------------------

instance FunctorI Backwards where
  imap h (Backwards fa)
    = Backwards (h fa)
  {-# INLINE imap #-}

instance FunctorI Reverse where
  imap h (Reverse fa) = Reverse (h fa)
  {-# INLINE imap #-}

instance FunctorI Lift where
  imap h = mapLift h
  {-# INLINE imap #-}

instance FunctorI (ExceptT e) where
  imap h = mapExceptT h
  {-# INLINE imap #-}

instance FunctorI IdentityT where
  imap h = mapIdentityT h
  {-# INLINE imap #-}

instance FunctorI MaybeT where
  imap h = mapMaybeT h
  {-# INLINE imap #-}

instance FunctorI (Lazy.RWST r w s) where
  imap h = Lazy.mapRWST h
  {-# INLINE imap #-}

instance FunctorI (Strict.RWST r w s) where
  imap h = Strict.mapRWST h
  {-# INLINE imap #-}

instance FunctorI (ReaderT r) where
  imap h = mapReaderT h
  {-# INLINE imap #-}

instance FunctorI (Lazy.StateT s) where
  imap h = Lazy.mapStateT h
  {-# INLINE imap #-}

instance FunctorI (Strict.StateT s) where
  imap h = Strict.mapStateT h
  {-# INLINE imap #-}

instance FunctorI (Lazy.WriterT w) where
  imap h = Lazy.mapWriterT h
  {-# INLINE imap #-}

instance FunctorI (Strict.WriterT w) where
  imap h = Strict.mapWriterT h
  {-# INLINE imap #-}
