{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Barbie.Internal.Dicts
  ( DictOf(..)
  , DictOfWear(..)
  , NoDict(..)
  , ClassInstanceDictF(..)
  )

where

import Data.Barbie.Internal.Wear(Wear)

import Data.Functor.Classes(Show1(..))
import Data.Kind(Constraint)

-- | @'DictOf' c f a@ is evidence that there exists an instance
--   of @c (f a)@.
data DictOf c f a where
  PackedDict :: c (f a) => DictOf c f a


instance Eq (DictOf c f a) where
  _ == _ = True

instance Show (DictOf c f a) where
  showsPrec _ PackedDict = showString "PackedDict"

instance Show1 (DictOf c f) where
  liftShowsPrec _ _ = showsPrec

-- | @'DictOfWear' c f a@  is evidence that there exists an
--   instance of @c (Wear f a)@.
data DictOfWear c f a where
  PackedDictWear :: c (Wear f a) => DictOfWear c f a

instance Eq (DictOfWear c f a) where
  _ == _ = True

instance Show (DictOfWear c f a) where
  showsPrec _ PackedDictWear = showString "PackedDictWear"

instance Show1 (DictOfWear c f) where
  liftShowsPrec _ _ = showsPrec


data NoDict (c :: * -> Constraint) (f :: * -> *) a
  = NoDict
  deriving(Eq, Show)

instance Show1 (NoDict c f) where
  liftShowsPrec _ _ = showsPrec


class ClassInstanceDictF d where
  type ConstraintOfDict d (c :: * -> Constraint) (f :: * -> *) a :: Constraint

  -- | Pack the dictionary associated with an instance.
  packDict :: ConstraintOfDict d c f a => d c f a


  -- | Turn a constrained-function into an unconstrained one
  --   that uses the packed instance dictionary instead.
  requiringDict :: (ConstraintOfDict d c f a => r) -> (d c f a -> r)


instance ClassInstanceDictF DictOf where
  type ConstraintOfDict DictOf c f a = c (f a)
  packDict = PackedDict
  requiringDict r = \PackedDict -> r

instance ClassInstanceDictF DictOfWear where
  type ConstraintOfDict DictOfWear c f a = c (Wear f a)
  packDict = PackedDictWear
  requiringDict r = \PackedDictWear -> r

instance ClassInstanceDictF NoDict where
  type ConstraintOfDict NoDict c f a = ()
  packDict = NoDict
  requiringDict r = \NoDict -> r

