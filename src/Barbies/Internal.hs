module Barbies.Internal
  ( -- * Functor
    Internal.gbmapDefault
  , Generics.GFunctor(..)
  , Internal.CanDeriveFunctorB
  , Internal.CanDeriveFunctorT

    -- * Traversable
  , Internal.gbtraverseDefault
  , Generics.GTraversable(..)
  , Internal.CanDeriveTraversableB
  , Internal.CanDeriveTraversableT

    -- * Distributive
  , Internal.gbdistributeDefault
  , Generics.GDistributive(..)
  , Internal.CanDeriveDistributiveB
  , Internal.CanDeriveDistributiveT

    -- * Applicative
  , Internal.gbpureDefault
  , Internal.gbprodDefault
  , Generics.GApplicative(..)
  , Internal.CanDeriveApplicativeB
  , Internal.CanDeriveApplicativeT


    -- * Constraints
  , Internal.gbaddDictsDefault
  , Generics.GConstraints(..)
  , Internal.CanDeriveConstraintsB
  , Internal.CanDeriveConstraintsT


  , Generics.GAll
  , Internal.GAllRepB
  , Internal.GAllRepT
  , Generics.X, Generics.Y
  , Generics.TagSelf, Generics.TagSelf', Generics.Self, Generics.Other

    -- * Bare values
  , Internal.gbcoverDefault
  , Internal.gbstripDefault
  , Generics.GBare(..)
  , Internal.CanDeriveBareB



    -- * Generic derivation support
  , module Data.Generics.GenericN
  )

where

import qualified Barbies.Generics.Applicative as Generics
import qualified Barbies.Generics.Bare as Generics
import qualified Barbies.Generics.Constraints as Generics
import qualified Barbies.Generics.Distributive as Generics
import qualified Barbies.Generics.Functor as Generics
import qualified Barbies.Generics.Traversable as Generics

import qualified Barbies.Internal.ApplicativeB as Internal
import qualified Barbies.Internal.ApplicativeT as Internal
import qualified Barbies.Internal.BareB as Internal
import qualified Barbies.Internal.ConstraintsB as Internal
import qualified Barbies.Internal.ConstraintsT as Internal
import qualified Barbies.Internal.DistributiveB as Internal
import qualified Barbies.Internal.DistributiveT as Internal
import qualified Barbies.Internal.FunctorB as Internal
import qualified Barbies.Internal.FunctorT as Internal
import qualified Barbies.Internal.TraversableB as Internal
import qualified Barbies.Internal.TraversableT as Internal

import Data.Generics.GenericN
