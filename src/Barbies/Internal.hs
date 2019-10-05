module Barbies.Internal
  ( -- * Functor
    Internal.gbmapDefault
  , Internal.GFunctorB(..)
  , Internal.CanDeriveFunctorB

    -- * Traversable
  , Internal.gbtraverseDefault
  , Internal.GTraversableB(..)
  , Internal.CanDeriveTraversableB

    -- * Applicative
  , Internal.gbpureDefault
  , Internal.gbprodDefault
  , Internal.GApplicativeB(..)
  , Internal.CanDeriveApplicativeB

    -- * Constraints
  , Internal.gbaddDictsDefault
  , Internal.GConstraintsB(..)
  , Internal.CanDeriveConstraintsB
  , Internal.GAllB
  , Internal.GAllBRep
  , Internal.X
  , Internal.TagSelf, Internal.Self, Internal.Other

    -- * Bare values
  , Internal.gbcoverDefault
  , Internal.gbstripDefault
  , Internal.GBareB(..)
  , Internal.CanDeriveBareB

    -- * Generic derivation support
  , GenericN, Rec(..), RepN
  )

where

import qualified Barbies.Internal.Applicative as Internal
import qualified Barbies.Internal.Bare as Internal
import qualified Barbies.Internal.Constraints as Internal
import qualified Barbies.Internal.Functor as Internal
import qualified Barbies.Internal.Traversable as Internal

import Data.Generics.GenericN (GenericN, Rec(..), RepN)
