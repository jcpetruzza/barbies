module Barbies.Internal
  ( -- * Functor
    Internal.gbmapDefault
  , Internal.GFunctorB(..)
  , Internal.CanDeriveFunctorB

    -- * Traversable
  , Internal.gbtraverseDefault
  , Internal.GTraversableB(..)
  , Internal.CanDeriveTraversableB

    -- * Product
  , Internal.gbuniqDefault
  , Internal.gbprodDefault
  , Internal.GProductB(..)
  , Internal.CanDeriveProductB

    -- * Constraints
  , Internal.gbaddDictsDefault
  , Internal.GConstraintsB(..)
  , Internal.CanDeriveConstraintsB
  , Internal.GAllBC(..)
  , Internal.GAllBRep
  , Internal.X
  , Internal.TagSelf, Internal.Self, Internal.Other

    -- * Proof
  , Internal.gbdictsDefault
  , Internal.GProductBC(..)
  , Internal.CanDeriveProductBC

    -- * Bare values
  , Internal.gbcoverDefault
  , Internal.gbstripDefault
  , Internal.GBareB(..)
  , Internal.CanDeriveBareB

    -- * Generic derivation support
  , GenericN, Rec(..), RepN
  )

where

import qualified Barbies.Internal.Bare as Internal
import qualified Barbies.Internal.Constraints as Internal
import qualified Barbies.Internal.Functor as Internal
import qualified Barbies.Internal.Product as Internal
import qualified Barbies.Internal.ProductC as Internal
import qualified Barbies.Internal.Traversable as Internal

import Data.Generics.GenericN (GenericN, Rec(..), RepN)
