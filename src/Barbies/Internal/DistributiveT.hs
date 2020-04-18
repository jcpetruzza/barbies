{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveT
  ( DistributiveT(..)
  , tshape
  , tdistribute'
  , tcotraverse
  , gtdistributeDefault
  , CanDeriveDistributiveT
  )

where

import Barbies.Generics.Distributive (GDistributive(..))
import Barbies.Internal.FunctorT (FunctorT (..))

import Control.Applicative.Backwards(Backwards (..))

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

class FunctorT t => DistributiveT (t :: (Type -> Type) -> i -> Type) where
  tdistribute :: Functor f => f (t g x) -> t (Compose f g) x

  default tdistribute
    :: forall f g x
    .  CanDeriveDistributiveT t f g x
    => f (t g x)
    -> t (Compose f g) x
  tdistribute = gtdistributeDefault

tshape :: DistributiveT b => b ((->) (b Identity x)) x
tshape = tdistribute' id

-- | A version of `tdistribute` with @g@ specialized to `Identity`.
tdistribute' :: (DistributiveT b, Functor f) => f (b Identity x) -> b f x
tdistribute' = tmap (fmap runIdentity . getCompose) . tdistribute

-- | Dual of `Barbies.Internal.TraversableT.ttraverse`
tcotraverse :: (DistributiveT b, Functor f) => (forall a . f (g a) -> f a) -> f (b g x) -> b f x
tcotraverse h = tmap (h . getCompose) . tdistribute

-- | @'CanDeriveDistributiveT' T f g x@ is in practice a predicate about @T@ only.
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
