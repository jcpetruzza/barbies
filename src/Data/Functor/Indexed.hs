-----------------------------------------------------------------------------
-- |
-- Module:  Data.Functor.Indexed
--
-- Endofunctors on indexed-types.
----------------------------------------------------------------------------
module Data.Functor.Indexed
  (
    -- * Functor
    Func.FunctorI(imap)

    -- * Traversable
  , Trav.TraversableI(itraverse)
    -- ** Utility functions
  , Trav.itraverse_
  , Trav.ifoldMap
  , Trav.isequence
  , Trav.isequence'

    -- * Applicative
  , Appl.ApplicativeI(ipure, iprod)
    -- ** Utility functions
  , Appl.izip
  , Appl.iunzip
  , Appl.izipWith
  , Appl.izipWith3
  , Appl.izipWith4

    -- * Constraints and instance dictionaries
  , Cons.ConstraintsI(..)
  , Cons.AllIF

    -- ** Utility functions
  , Cons.imapC
  , Cons.itraverseC

    -- * Support for generic derivations
  , GenericsN.Rec(..)
  )

where

import qualified Barbies.Internal.ApplicativeI as Appl
import qualified Barbies.Internal.ConstraintsI as Cons
import qualified Barbies.Internal.FunctorI as Func
import qualified Barbies.Internal.TraversableI as Trav

import qualified Data.Generics.GenericN as GenericsN
