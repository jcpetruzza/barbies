{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Barbie.Internal.Dicts
  ( DictOf(..)
  , packDict
  , requiringDict
  )

where

import Data.Functor.Classes(Show1(..))

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

-- | Pack the dictionary associated with an instance.
packDict :: c (f a) => DictOf c f a
packDict = PackedDict

-- | Turn a constrained-function into an unconstrained one
--   that uses the packed instance dictionary instead.
requiringDict :: (c (f a) => r) -> (DictOf c f a -> r)
requiringDict r = \PackedDict -> r
