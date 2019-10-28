-----------------------------------------------------------------------------
-- |
-- Module:  Data.Functor.Transformer
--
-- Endofunctors on indexed-types.
----------------------------------------------------------------------------
module Data.Functor.Transformer
  (
    -- * Functor
    Func.FunctorT(tmap)

    -- * Traversable
  , Trav.TraversableT(ttraverse)
    -- ** Utility functions
  , Trav.ttraverse_
  , Trav.tfoldMap
  , Trav.tsequence
  , Trav.tsequence'

    -- * Applicative
  , Appl.ApplicativeT(tpure, tprod)
    -- ** Utility functions
  , Appl.tzip
  , Appl.tunzip
  , Appl.tzipWith
  , Appl.tzipWith3
  , Appl.tzipWith4

    -- * Constraints and instance dictionaries
  , Cons.ConstraintsT(..)
  , Cons.AllTF

    -- ** Utility functions
  , Cons.tmapC
  , Cons.ttraverseC

    -- * Support for generic derivations
  , GenericsN.Rec(..)
  )

where

import qualified Barbies.Internal.ApplicativeT as Appl
import qualified Barbies.Internal.ConstraintsT as Cons
import qualified Barbies.Internal.FunctorT as Func
import qualified Barbies.Internal.TraversableT as Trav

import qualified Data.Generics.GenericN as GenericsN
