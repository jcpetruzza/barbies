{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Barbie.Internal.Instances ( Barbie(..) )

where

import Data.Barbie.Internal.Bare
import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Dicts
import Data.Barbie.Internal.Functor
import Data.Barbie.Internal.Traversable
import Data.Barbie.Internal.Product
import Data.Barbie.Internal.ProofB

-- | A wrapper for Barbie-types, providing useful instances.
newtype Barbie b (f :: * -> *)
  = Barbie { getBarbie :: b f }
  deriving (FunctorB, ProductB, BareB, ProofB)

-- Need to derive it manually to make GHC 8.0.2 happy
instance ConstraintsB b => ConstraintsB (Barbie b) where
  type ConstraintsOf c f (Barbie b) = ConstraintsOf c f b
  adjProof = Barbie . adjProof . getBarbie

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
