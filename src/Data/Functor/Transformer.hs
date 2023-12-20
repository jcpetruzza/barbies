-----------------------------------------------------------------------------
-- |
-- Module:  Data.Functor.Transformer
--
-- Functors on indexed-types.
----------------------------------------------------------------------------
module Data.Functor.Transformer
  (
    -- * Functor
    Func.FunctorT(tmap)

    -- * Traversable
  , Trav.TraversableT(ttraverse)
    -- ** Utility functions
  , Trav.tfor
  , Trav.ttraverse_
  , Trav.tfor_
  , Trav.tfoldMap
  , Trav.tsequence
  , Trav.tsequence'

    -- * Distributive
  , Dist.DistributiveT(tdistribute)
  , Dist.tdistribute'
  , Dist.tcotraverse
  , Dist.tdecompose
  , Dist.trecompose

    -- * Applicative
  , Appl.ApplicativeT(tpure, tprod)
    -- ** Utility functions
  , Appl.tzip
  , Appl.tunzip
  , Appl.tzipWith
  , Appl.tzipWith3
  , Appl.tzipWith4

    -- * Monad
  , Mon.MonadT(..)

    -- * Constraints and instance dictionaries
  , Cons.ConstraintsT(..)
  , Cons.AllTF

    -- ** Utility functions
  , Cons.tmapC
  , Cons.ttraverseC
  , Cons.tforC

    -- * Support for generic derivations
  , GenericsN.Rec(..)
  )

where

import qualified Barbies.Internal.ApplicativeT as Appl
import qualified Barbies.Internal.ConstraintsT as Cons
import qualified Barbies.Internal.DistributiveT as Dist
import qualified Barbies.Internal.FunctorT as Func
import qualified Barbies.Internal.MonadT as Mon
import qualified Barbies.Internal.TraversableT as Trav

import qualified Data.Generics.GenericN as GenericsN
