{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveT
  ( DistributiveT(..)
  , tdistribute'
  , tcotraverse
  , tdecompose
  , trecompose
  , gtdistributeDefault
  , CanDeriveDistributiveT
  )

where

import Barbies.Generics.Distributive (GDistributive(..))
import Barbies.Internal.FunctorT (FunctorT (..))

import Control.Applicative.Backwards(Backwards (..))

#if MIN_VERSION_transformers(0,5,3)
import Control.Monad.Trans.Accum(AccumT(..), runAccumT)
#endif
import Control.Monad.Trans.Except(ExceptT(..), runExceptT)
import Control.Monad.Trans.Identity(IdentityT(..))
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..))
import Control.Monad.Trans.RWS.Strict as Strict (RWST(..))
import Control.Monad.Trans.Reader(ReaderT(..))
import Control.Monad.Trans.State.Lazy as Lazy (StateT(..))
import Control.Monad.Trans.State.Strict as Strict (StateT(..))
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..))
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..))

import Data.Functor.Compose   (Compose (..))
import Data.Functor.Identity  (Identity (..))
import Data.Functor.Reverse   (Reverse (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Distributive
import Data.Kind              (Type)

-- | A 'FunctorT' where the effects can be distributed to the fields:
--  `tdistribute` turns an effectful way of building a transformer-type
--  into a pure transformer-type with effectful ways of computing the
--  values of its fields.
--
--  This class is the categorical dual of `Barbies.Internal.TraversableT.TraversableT`,
--  with `tdistribute` the dual of `Barbies.Internal.TraversableT.tsequence`
--  and `tcotraverse` the dual of `Barbies.Internal.TraversableT.ttraverse`. As such,
--  instances need to satisfy these laws:
--
-- @
-- 'tdistribute' . h = 'tmap' ('Compose' . h . 'getCompose') . 'tdistribute'    -- naturality
-- 'tdistribute' . 'Data.Functor.Identity' = 'tmap' ('Compose' . 'Data.Functor.Identity')                 -- identity
-- 'tdistribute' . 'Compose' = 'fmap' ('Compose' . 'Compose' . 'fmap' 'getCompose' . 'getCompose') . 'tdistribute' . 'fmap' 'distribute' -- composition
-- @
--
-- By specializing @f@ to @((->) a)@ and @g@ to 'Identity', we can define a function that
-- decomposes a function on distributive transformers into a collection of simpler functions:
--
-- @
-- 'tdecompose' :: 'DistributiveT' b => (a -> b 'Identity') -> b ((->) a)
-- 'tdecompose' = 'tmap' ('fmap' 'runIdentity' . 'getCompose') . 'tdistribute'
-- @
--
-- Lawful instances of the class can then be characterized as those that satisfy:
--
-- @
-- 'trecompose' . 'tdecompose' = 'id'
-- 'tdecompose' . 'trecompose' = 'id'
-- @
--
-- This means intuitively that instances need to have a fixed shape (i.e. no sum-types can be involved).
-- Typically, this means record types, as long as they don't contain fields where the functor argument is not applied.
--
--
-- There is a default implementation of 'tdistribute' based on
-- 'Generic'.  Intuitively, it works on product types where the shape
-- of a pure value is uniquely defined and every field is covered by
-- the argument @f@.
class FunctorT t => DistributiveT (t :: (Type -> Type) -> i -> Type) where
  tdistribute :: Functor f => f (t g x) -> t (Compose f g) x

  default tdistribute
    :: forall f g x
    .  CanDeriveDistributiveT t f g x
    => f (t g x)
    -> t (Compose f g) x
  tdistribute = gtdistributeDefault

-- | A version of `tdistribute` with @g@ specialized to `Identity`.
tdistribute' :: (DistributiveT t, Functor f) => f (t Identity x) -> t f x
tdistribute' = tmap (fmap runIdentity . getCompose) . tdistribute

-- | Dual of `Barbies.Internal.TraversableT.ttraverse`
tcotraverse :: (DistributiveT t, Functor f) => (forall a . f (g a) -> f a) -> f (t g x) -> t f x
tcotraverse h = tmap (h . getCompose) . tdistribute

-- | Decompose a function returning a distributive transformer, into
--   a collection of simpler functions.
tdecompose :: DistributiveT t => (a -> t Identity x) -> t ((->) a) x
tdecompose = tdistribute'

-- | Recompose a decomposed function.
trecompose :: FunctorT t => t ((->) a) x -> a -> t Identity x
trecompose bfs = \a -> tmap (Identity . ($ a)) bfs

-- | @'CanDeriveDistributiveT' T f g x@ is in practice a predicate about @T@ only.
--   Intuitively, it says the the following holds  for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f x)@.
--
--     * @(B f x)@ has only one constructor, and doesn't contain "naked" fields
--       (that is, not covered by `f`). In particular, @x@ needs to occur under @f@.
--
--     * @B f x@ can contain fields of type @b f y@ as long as there exists a
--       @'DistributiveT' b@ instance. In particular, recursive usages of @B f x@
--       are allowed.
--
--     * @B f x@ can also contain usages of @b f y@ under a @'Distributive' h@.
--       For example, one could use @a -> (B f x)@ as a field of @B f x@.
type CanDeriveDistributiveT (t :: (Type -> Type) -> i -> Type) f g x
  = ( GenericP 1 (t g x)
    , GenericP 1 (t (Compose f g) x)
    , GDistributive 1 f (RepP 1 (t g x)) (RepP 1 (t (Compose f g) x))
    )

-- | Default implementation of 'tdistribute' based on 'Generic'.
gtdistributeDefault
  :: CanDeriveDistributiveT t f g x
  => f (t g x)
  -> t (Compose f g) x
gtdistributeDefault = toP (Proxy @1) . gdistribute (Proxy @1) . fmap (fromP (Proxy @1))
{-# INLINE gtdistributeDefault #-}

------------------------------------------------------------
-- Generic derivation: Special cases for FunctorT
-- -----------------------------------------------------------

type P = Param

instance
  ( Functor f
  , DistributiveT t
  ) => GDistributive 1 f (Rec (t (P 1 g) x) (t g x)) (Rec (t (P 1 (Compose f g)) x) (t (Compose f g) x))
  where
  gdistribute _ = Rec . K1 . tdistribute . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}


instance
  ( Functor f
  , Distributive h
  , DistributiveT t
  ) =>
  GDistributive 1 f (Rec (h (t (P 1 g) x)) (h (t g x))) (Rec (h (t (P 1 (Compose f g)) x)) (h (t (Compose f g) x)))
  where
  gdistribute _ = Rec . K1 . fmap tdistribute . distribute . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}

-- --------------------------------
-- Instances for base types
-- --------------------------------

instance Distributive f => DistributiveT (Compose f) where
  tdistribute = Compose . fmap Compose . distribute . fmap getCompose
  {-# INLINE tdistribute #-}

-- -- --------------------------------
-- -- Instances for transformers types
-- -- --------------------------------

#if MIN_VERSION_transformers(0,5,3)
instance DistributiveT (AccumT w) where
  tdistribute fh = AccumT $ \w -> Compose $ fmap (\h -> runAccumT h w) fh
  {-# INLINE tdistribute #-}
#endif

instance DistributiveT Backwards where
  tdistribute = Backwards . Compose . fmap forwards
  {-# INLINE tdistribute #-}

instance DistributiveT Reverse where
  tdistribute = Reverse . Compose . fmap getReverse
  {-# INLINE tdistribute #-}

instance DistributiveT (ExceptT e) where
  tdistribute = ExceptT . Compose . fmap runExceptT
  {-# INLINE tdistribute #-}

instance DistributiveT IdentityT where
  tdistribute = IdentityT . Compose . fmap runIdentityT
  {-# INLINE tdistribute #-}

instance DistributiveT MaybeT where
  tdistribute = MaybeT . Compose . fmap runMaybeT
  {-# INLINE tdistribute #-}

instance DistributiveT (Lazy.RWST r w s) where
  tdistribute fh = Lazy.RWST $ \r s -> Compose $ fmap (\h -> Lazy.runRWST h r s) fh
  {-# INLINE tdistribute #-}

instance DistributiveT (Strict.RWST r w s) where
  tdistribute fh = Strict.RWST $ \r s -> Compose $ fmap (\h -> Strict.runRWST h r s) fh
  {-# INLINE tdistribute #-}

instance DistributiveT (ReaderT r) where
  tdistribute fh = ReaderT $ \r -> Compose $ fmap (\h -> runReaderT h r) fh
  {-# INLINE tdistribute #-}

instance DistributiveT (Lazy.StateT s) where
  tdistribute fh = Lazy.StateT $ \s -> Compose $ fmap (\h -> Lazy.runStateT h s) fh
  {-# INLINE tdistribute #-}

instance DistributiveT (Strict.StateT s) where
  tdistribute fh = Strict.StateT $ \s -> Compose $ fmap (\h -> Strict.runStateT h s) fh
  {-# INLINE tdistribute #-}

instance DistributiveT (Lazy.WriterT w) where
  tdistribute = Lazy.WriterT . Compose . fmap Lazy.runWriterT
  {-# INLINE tdistribute #-}

instance DistributiveT (Strict.WriterT w) where
  tdistribute = Strict.WriterT . Compose . fmap Strict.runWriterT
  {-# INLINE tdistribute #-}
