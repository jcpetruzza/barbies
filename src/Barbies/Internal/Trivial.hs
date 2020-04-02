{-# LANGUAGE PolyKinds #-}
module Barbies.Internal.Trivial
  ( Void
  , Unit (..)
  )

where

import Barbies.Internal.ApplicativeB(ApplicativeB(..))
import Barbies.Internal.ConstraintsB(ConstraintsB(..))
import Barbies.Internal.FunctorB(FunctorB(..))
import Barbies.Internal.DistributiveB(DistributiveB(..))
import Barbies.Internal.TraversableB(TraversableB(..))

import Data.Data (Data(..))
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

---------------------------------------------------
-- Trivial Barbies
---------------------------------------------------

-- | Uninhabited barbie type.
data Void (f :: k -> Type)
  deriving (Generic, Typeable)

instance Eq   (Void f) where
  (==) v = case v of

instance Ord  (Void f) where
  compare v = case v of

instance Show (Void f) where
  showsPrec _ v = case v of

instance Semigroup (Void f) where
  (<>) v = case v of


instance FunctorB Void
instance TraversableB Void
instance ConstraintsB Void


-- | A barbie type without structure.
data Unit (f :: k -> Type)
  = Unit
  deriving
    ( Data, Generic, Typeable
    , Eq, Ord, Read, Show
    )

instance Semigroup (Unit f) where
  Unit <> Unit = Unit

instance Monoid (Unit f) where
  mempty  = Unit
  mappend = (<>)

instance FunctorB Unit
instance DistributiveB Unit
instance TraversableB Unit
instance ApplicativeB Unit
instance ConstraintsB Unit
