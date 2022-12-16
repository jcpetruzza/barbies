{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.FunctorT
  ( FunctorT(..)
  , gtmapDefault
  , CanDeriveFunctorT
  )

where

import Barbies.Generics.Functor (GFunctor(..))

import Control.Applicative.Backwards(Backwards (..))
import Control.Applicative.Lift(Lift, mapLift )

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum(AccumT, mapAccumT)
#endif
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

import Data.Functor.Compose   (Compose (..))
import Data.Functor.Product   (Product (..))
import Data.Functor.Reverse   (Reverse (..))
import Data.Functor.Sum       (Sum (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Kind              (Type)

-- | Functor from indexed-types to indexed-types. Instances of 'FunctorT' should
--   satisfy the following laws:
--
-- @
-- 'tmap' 'id' = 'id'
-- 'tmap' f . 'tmap' g = 'tmap' (f . g)
-- @
--
-- There is a default 'tmap' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorT (t :: (k -> Type) -> k' -> Type) where
  tmap :: (forall a . f a -> g a) -> t f x -> t g x

  default tmap
    :: forall f g x
    .  CanDeriveFunctorT t f g x
    => (forall a . f a -> g a)
    -> t f x
    -> t g x
  tmap = gtmapDefault

-- | @'CanDeriveFunctorT' T f g x@ is in practice a predicate about @T@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (T f)@.
--
--     * @T f x@ can contain fields of type @t f y@ as long as there exists a
--       @'FunctorT' t@ instance. In particular, recursive usages of @T f y@
--       are allowed.
--
--     * @T f x@ can also contain usages of @t f y@ under a @'Functor' h@.
--       For example, one could use @'Maybe' (T f y)@ when defining @T f y@.
type CanDeriveFunctorT t f g x
  = ( GenericP 1 (t f x)
    , GenericP 1 (t g x)
    , GFunctor 1 f g (RepP 1 (t f x)) (RepP 1 (t g x))
    )

-- | Default implementation of 'tmap' based on 'Generic'.
gtmapDefault
  :: CanDeriveFunctorT t f g x
  => (forall a . f a -> g a)
  -> t f x
  -> t g x
gtmapDefault f
  = toP (Proxy @1) . gmap (Proxy @1) f . fromP (Proxy @1)
{-# INLINE gtmapDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorT
-- -----------------------------------------------------------

type P = Param

instance
  ( FunctorT t
  ) => GFunctor 1 f g (Rec (t (P 1 f) x) (t f x))
                      (Rec (t (P 1 g) x) (t g x))
  where
  gmap _ h (Rec (K1 tf)) = Rec (K1 (tmap h tf))
  {-# INLINE gmap #-}


instance
  ( Functor h
  , FunctorT t
  ) => GFunctor 1 f g (Rec (h (t (P 1 f) x)) (h (t f x)))
                      (Rec (h (t (P 1 g) x)) (h (t g x)))
  where
  gmap _ h (Rec (K1 htf)) = Rec (K1 (fmap (tmap h) htf))
  {-# INLINE gmap #-}


-- This is the same as the previous instance, but for nested (normal-flavoured)
-- functors.
instance
  ( Functor h
  , Functor m
  , FunctorT t
  ) => GFunctor 1 f g (Rec (m (h (t (P 1 f) x))) (m (h (t f x))))
                      (Rec (m (h (t (P 1 g) x))) (m (h (t g x))))
  where
  gmap _ h (Rec (K1 mhtf)) = Rec (K1 (fmap (fmap (tmap h)) mhtf))
  {-# INLINE gmap #-}

-- --------------------------------
-- Instances for base types
-- --------------------------------

instance Functor f => FunctorT (Compose f) where
  tmap h (Compose fga)
    = Compose (fmap h fga)
  {-# INLINE tmap #-}

instance FunctorT (Product f) where
  tmap h (Pair fa ga) = Pair fa (h ga)
  {-# INLINE tmap #-}

instance FunctorT (Sum f) where
  tmap h = \case
    InL fa -> InL fa
    InR ga -> InR (h ga)
  {-# INLINE tmap #-}

-- --------------------------------
-- Instances for transformers types
-- --------------------------------

#if MIN_VERSION_transformers(0,5,3)
instance FunctorT (AccumT w) where
  tmap h = mapAccumT h
  {-# INLINE tmap #-}
#endif

instance FunctorT Backwards where
  tmap h (Backwards fa)
    = Backwards (h fa)
  {-# INLINE tmap #-}

instance FunctorT Reverse where
  tmap h (Reverse fa) = Reverse (h fa)
  {-# INLINE tmap #-}

instance FunctorT Lift where
  tmap h = mapLift h
  {-# INLINE tmap #-}

instance FunctorT (ExceptT e) where
  tmap h = mapExceptT h
  {-# INLINE tmap #-}

instance FunctorT IdentityT where
  tmap h = mapIdentityT h
  {-# INLINE tmap #-}

instance FunctorT MaybeT where
  tmap h = mapMaybeT h
  {-# INLINE tmap #-}

instance FunctorT (Lazy.RWST r w s) where
  tmap h = Lazy.mapRWST h
  {-# INLINE tmap #-}

instance FunctorT (Strict.RWST r w s) where
  tmap h = Strict.mapRWST h
  {-# INLINE tmap #-}

instance FunctorT (ReaderT r) where
  tmap h = mapReaderT h
  {-# INLINE tmap #-}

instance FunctorT (Lazy.StateT s) where
  tmap h = Lazy.mapStateT h
  {-# INLINE tmap #-}

instance FunctorT (Strict.StateT s) where
  tmap h = Strict.mapStateT h
  {-# INLINE tmap #-}

instance FunctorT (Lazy.WriterT w) where
  tmap h = Lazy.mapWriterT h
  {-# INLINE tmap #-}

instance FunctorT (Strict.WriterT w) where
  tmap h = Strict.mapWriterT h
  {-# INLINE tmap #-}
