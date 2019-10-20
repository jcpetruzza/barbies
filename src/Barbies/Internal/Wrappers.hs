{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Barbies.Internal.Wrappers
  ( Barbie(..)
  ) where

import Barbies.Internal.ApplicativeB
import Barbies.Internal.ConstraintsB
import Barbies.Internal.Dicts
import Barbies.Internal.FunctorB
import Barbies.Internal.TraversableB

import Data.Kind (Type)


-- | A wrapper for Barbie-types, providing useful instances.
newtype Barbie (b :: (k -> Type) -> Type) f
  = Barbie { getBarbie :: b f }
  deriving (FunctorB, ApplicativeB)

-- Need to derive it manually to make GHC 8.0.2 happy
instance ConstraintsB b => ConstraintsB (Barbie b) where
  type AllB c (Barbie b) = AllB c b
  baddDicts = Barbie . baddDicts . getBarbie

instance TraversableB b => TraversableB (Barbie b) where
  btraverse f = fmap Barbie . btraverse f . getBarbie


instance (ConstraintsB b, ApplicativeB b, AllBF Semigroup f b) => Semigroup (Barbie b f) where
  (<>) = bzipWith3 mk bdicts
    where
      mk :: Dict (ClassF Semigroup f) a -> f a -> f a -> f a
      mk = requiringDict (<>)

instance (ConstraintsB b, ApplicativeB b, AllBF Semigroup f b, AllBF Monoid f b) => Monoid (Barbie b f) where
  mempty  = bmempty
  mappend = (<>)
