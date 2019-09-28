-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Barbie
--
-- TBP
----------------------------------------------------------------------------
module Data.Functor.Barbie
  (
    -- * Functor
    FunctorB(bmap)

    -- * Traversable
  , TraversableB(btraverse)
    -- ** Utility functions
  , btraverse_
  , bfoldMap
  , bsequence, bsequence'

    -- * Product
  , ProductB(buniq, bprod)
    -- ** Utility functions
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4

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
    -- 'Data.Barbie.bmap' 'showIt' :: 'Data.Functor.Barbie.FunctorB' b => b 'Maybe' -> b ('Data.Functor.Const' 'String')
    -- @
    --
    -- This however doesn't work because of the @('Show' a)@ constraint in the
    -- the type of @showIt@.
    --
    -- The 'ConstraintsB' class let us overcome this problem.

  , ConstraintsB(..)
  , AllBF

    -- ** Products and constaints
  , ProductBC(bdicts)

    -- ** Utility functions
  , bmapC
  , btraverseC
  , buniqC
  , bmempty

    -- * Wrapper
  , Barbie(..)

    -- * Trivial Barbies
  , Trivial.Void
  , Trivial.Unit (..)

    -- * Generic derivations
  , Rec(..)
  )

where

import Barbies.Internal.Constraints(AllBF, ConstraintsB (..), bmapC, btraverseC)

import Barbies.Internal.Functor(FunctorB(..))
import Barbies.Internal.Instances(Barbie(..))
import Barbies.Internal.Product
  ( ProductB(..)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4
  )
import Barbies.Internal.ProductC(ProductBC(..), buniqC, bmempty)
import Barbies.Internal.Traversable
  ( TraversableB(..)
  , bsequence, bsequence'
  , bfoldMap, btraverse_
  )
import qualified Barbies.Internal.Trivial as Trivial

import Data.Generics.GenericN(Rec(..))
