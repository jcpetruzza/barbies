module Data.Barbie.Internal
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
  , Internal.gadjProofDefault
  , Internal.GConstraintsB(..)
  , Internal.CanDeriveConstraintsB
  , Internal.GAllBC(..)
  , Internal.GAllBRep
  , Internal.X
  , Internal.TagSelf, Internal.Self, Internal.Other

    -- * Proof
  , Internal.gbproofDefault
  , Internal.GProofB(..)
  , Internal.CanDeriveProofB

    -- * Bare values
  , Internal.gbcoverDefault
  , Internal.gbstripDefault
  , Internal.GBareB(..)
  , Internal.CanDeriveBareB

    -- * Generic derivation support
  , GenericN, Rec(..), RepN
  )

where

import qualified Data.Barbie.Internal.Bare as Internal
import qualified Data.Barbie.Internal.Constraints as Internal
import qualified Data.Barbie.Internal.Functor as Internal
import qualified Data.Barbie.Internal.Product as Internal
import qualified Data.Barbie.Internal.ProofB as Internal
import qualified Data.Barbie.Internal.Traversable as Internal

import Data.Generics.GenericN (GenericN, Rec(..), RepN)
