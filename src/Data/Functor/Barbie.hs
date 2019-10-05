-----------------------------------------------------------------------------
-- |
-- Module:  Data.Functor.Barbie
--
-- Functors from indexed-types to types.
----------------------------------------------------------------------------
module Data.Functor.Barbie
  ( -- * Functor
    Func.FunctorB(bmap)

    -- * Traversable
  , Trav.TraversableB(btraverse)
    -- ** Utility functions
  , Trav.btraverse_
  , Trav.bfoldMap
  , Trav.bsequence
  , Trav.bsequence'

    -- * Applicative
  , Appl.ApplicativeB(bpure, bprod)
    -- ** Utility functions
  , Appl.bzip
  , Appl.bunzip
  , Appl.bzipWith
  , Appl.bzipWith3
  , Appl.bzipWith4

    -- * Constraints and instance dictionaries
    -- | Consider the following function:
    --
    -- @
    -- showIt :: 'Show' a => 'Maybe' a -> 'Data.Functor.Const' 'String' a
    -- showIt = 'Data.Functor.Const' . 'show'
    -- @
    --
    -- We would then like to be able to do:
    --
    -- @
    -- 'Data.Functor.Barbie.bmap' 'showIt' :: 'Data.Functor.Barbie.FunctorB' b => b 'Maybe' -> b ('Data.Functor.Const' 'String')
    -- @
    --
    -- This however doesn't work because of the @('Show' a)@ constraint in the
    -- the type of @showIt@.
    --
    -- The 'Cons.ConstraintsB' class let us overcome this problem.

  , Cons.ConstraintsB(..)
  , Cons.AllBF

    -- ** Utility functions
  , Cons.bdicts
  , Cons.bmapC
  , Cons.btraverseC
  , Cons.bpureC
  , Cons.bmempty

    -- * Support for generic derivations
  , GenericN.Rec(..)
  )

where

import qualified Barbies.Internal.Constraints as Cons
import qualified Barbies.Internal.Functor as Func
import qualified Barbies.Internal.Applicative as Appl
import qualified Barbies.Internal.Traversable as Trav

import qualified Data.Generics.GenericN as GenericN
