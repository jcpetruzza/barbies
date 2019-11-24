{-# LANGUAGE PolyKinds #-}
module Barbies.Internal.MonadT
  ( MonadT(..)
  )
where

import Barbies.Internal.FunctorT(FunctorT(..))

import Control.Applicative (Alternative(..))
import Control.Applicative.Lift as Lift (Lift(..))
import Control.Applicative.Backwards as Backwards (Backwards(..))
import Control.Monad (join)
import Control.Monad.Trans.Identity(IdentityT(..))
import Control.Monad.Trans.Reader(ReaderT(..))

import Data.Coerce (coerce)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Reverse (Reverse(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))

-- | Some endo-functors on indexed-types are monads. Common examples would be
--   "functor-transformers", like 'Compose' or 'ReaderT'. In that sense, 'MonadT'
--   is similar to 'Control.Monad.Trans.Class.MonadTrans' but with additional
--   structure (see also <https://hackage.haskell.org.package/mmorph mmorph>'s
--   @MMonad@ class).
--
--   Notice though that while 'Control.Monad.Trans.Class.lift' assumes
--   a 'Monad' instance of the value to be lifted, 'tlift' has no such constraint.
--   This means we cannot have instances for most "monad transformers", since
--   lifting typically involves an 'fmap'.
--
--   'MonadT' also corresponds to the indexed-monad of
--   <https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf Kleisli arrows of outrageous fortune>.
--
--   Instances of this class should to satisfy the monad laws. They laws can stated
--   either in terms of @('tlift', 'tjoin')@ or @('tlift', 'tembed')@. In the former:
--
-- @
-- 'tmap' h . 'tlift' = 'tlift' . h
-- 'tmap' h . 'tjoin' = 'tjoin' . 'tmap' ('tmap' h)
-- 'tjoin' . 'tlift'  = 'id'
-- 'tjoin' . 'tmap tlift' = 'id'
-- 'tjoin' . 'tjoin' = 'tjoin' . 'tmap' 'tjoin'
-- @
--
--   In the latter:
--
-- @
-- 'tembed' f . 'tlift' = f
-- 'tembed' 'tlift' = 'id'
-- 'tembed' f . 'tembed' g = 'tembed' ('tembed' f . g)
-- @
--
class FunctorT t => MonadT t where
  -- | Lift a functor to a transformed functor.
  tlift :: f a -> t f a

  -- | The conventional monad join operator. It is used to remove
  --   one level of monadic structure, projecting its bound argument
  --   into the outer level.
  tjoin :: t (t f) a -> t f a
  tjoin
    = tembed id

  -- | Analogous to @('Control.Monad.=<<')@.
  tembed :: MonadT t => (forall x. f x -> t g x) -> t f a -> t g a
  tembed h
    = tjoin . tmap h

  {-# MINIMAL tlift, tjoin | tlift, tembed #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance Monad f => MonadT (Compose f) where
  tlift = Compose . pure
  {-# INLINE tlift #-}

  tjoin (Compose ffga)
    = Compose (join $ coerce <$> ffga)
  {-# INLINE tjoin #-}


instance Alternative f => MonadT (Product f) where
  tlift = Pair empty
  {-# INLINE tlift #-}

  tjoin (Pair fa (Pair fa' ga))
    = Pair (fa <|> fa') ga
  {-# INLINE tjoin #-}


instance MonadT (Sum f) where
  tlift = InR
  {-# INLINE tlift #-}

  tjoin = \case
    InL fa -> InL fa
    InR (InL fa) -> InL fa
    InR (InR ga) -> InR ga


-- --------------------------------
-- Instances for transformers types
-- --------------------------------

instance MonadT Backwards where
  tlift = Backwards
  {-# INLINE tlift #-}

  tjoin = coerce
  {-# INLINE tjoin #-}


instance MonadT Lift where
  tlift = Lift.Other
  {-# INLINE tlift #-}

  tjoin = \case
    Lift.Pure a
      -> Lift.Pure a

    Lift.Other (Lift.Pure a)
      -> Lift.Pure a

    Lift.Other (Lift.Other fa)
      -> Lift.Other fa
  {-# INLINE tjoin #-}


instance MonadT IdentityT where
  tlift = coerce
  {-# INLINE tlift #-}

  tjoin = coerce
  {-# INLINE tjoin #-}


instance MonadT (ReaderT r) where
  tlift = ReaderT . const
  {-# INLINE tlift #-}

  tjoin rra
    = ReaderT $ \e -> coerce rra e e
  {-# INLINE tjoin #-}


instance MonadT Reverse where
  tlift = coerce
  {-# INLINE tlift #-}

  tjoin = coerce
  {-# INLINE tjoin #-}
