{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.Barbie.Internal.Instances ( Barbie(..) )

where

import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Dicts
import Data.Barbie.Internal.Functor
import Data.Barbie.Internal.Traversable
import Data.Barbie.Internal.Product
import Data.Barbie.Internal.ProofB

import Data.Semigroup (Semigroup, (<>))

-- | A wrapper for Barbie-types, providing useful instances.
newtype Barbie b (f :: * -> *)
  = Barbie { getBarbie :: b f }
  deriving (FunctorB, ProductB, ProofB)

-- Need to derive it manually to make GHC 8.0.2 happy
instance ConstraintsB b => ConstraintsB (Barbie b) where
  type AllB c (Barbie b) = AllB c b
  adjProof = Barbie . adjProof . getBarbie

instance TraversableB b => TraversableB (Barbie b) where
  btraverse f = fmap Barbie . btraverse f . getBarbie


instance (ProofB b, AllBF Semigroup f b) => Semigroup (Barbie b f) where
  (<>) = bzipWith3 mk bproof
    where
      mk :: Dict (ClassF Semigroup f) a -> f a -> f a -> f a
      mk = requiringDict (<>)

instance (ProofB b, AllBF Semigroup f b, AllBF Monoid f b) => Monoid (Barbie b f) where
  mempty  = bmempty
  mappend = (<>)
