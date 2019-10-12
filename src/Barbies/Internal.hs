module Barbies.Internal
  ( -- * Functor
    Internal.gbmapDefault
  , Generics.GFunctor(..)
  , Internal.CanDeriveFunctorB

    -- * Traversable
  , Internal.gbtraverseDefault
  , Generics.GTraversable(..)
  , Internal.CanDeriveTraversableB

    -- * Applicative
  , Internal.gbpureDefault
  , Internal.gbprodDefault
  , Generics.GApplicative(..)
  , Internal.CanDeriveApplicativeB

    -- * Constraints
  , Internal.gbaddDictsDefault
  , Generics.GConstraints(..)
  , Internal.CanDeriveConstraintsB
  , Generics.GAll
  , Internal.GAllRepB
  , Generics.X, Generics.Y
  , Generics.TagSelf, Generics.Self, Generics.Other

    -- * Bare values
  , Internal.gbcoverDefault
  , Internal.gbstripDefault
  , Generics.GBare(..)
  , Internal.CanDeriveBareB

    -- * Generic derivation support
  , GenericN, Rec(..), RepN
  )

where

import qualified Barbies.Generics.Applicative as Generics
import qualified Barbies.Generics.Bare as Generics
import qualified Barbies.Generics.Constraints as Generics
import qualified Barbies.Generics.Functor as Generics
import qualified Barbies.Generics.Traversable as Generics

import qualified Barbies.Internal.ApplicativeB as Internal
import qualified Barbies.Internal.BareB as Internal
import qualified Barbies.Internal.ConstraintsB as Internal
import qualified Barbies.Internal.FunctorB as Internal
import qualified Barbies.Internal.TraversableB as Internal

import Data.Generics.GenericN (GenericN, Rec(..), RepN)
