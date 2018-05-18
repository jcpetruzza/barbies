{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Barbie.Internal.Instances ( Barbie(..) )

where

import Data.Barbie.Internal.Bare
import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Functor
import Data.Barbie.Internal.Traversable
import Data.Barbie.Internal.Product
import Data.Barbie.Internal.ProofB

-- | A wrapper for Barbie-types, providing useful instances.
newtype Barbie b (f :: * -> *)
  = Barbie { getBarbie :: b f }
  deriving (FunctorB, ProductB, BareB, ConstraintsB, ProofB)

instance TraversableB b => TraversableB (Barbie b) where
  btraverse f = fmap Barbie . btraverse f . getBarbie


instance (ProofB b, ConstraintsOf Monoid f b) => Monoid (Barbie b f) where
  mempty = bmap mk bproof
    where
      mk :: DictOf Monoid f a -> f a
      mk = requiringDict mempty

  mappend = bzipWith3 mk bproof
    where
      mk :: DictOf Monoid f a -> f a -> f a -> f a
      mk = requiringDict mappend
