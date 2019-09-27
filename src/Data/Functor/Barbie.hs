-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Barbie
--
-- A common Haskell idiom is to parameterise a datatype by a type @k -> *@,
-- typically a functor or a GADT. These are like outfits of a Barbie,
-- that turn her into a different doll. E.g.
--
-- @
-- data Barbie f
--   = Barbie
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
--
-- b1 :: Barbie 'Data.Monoid.Last'       -- Barbie with a monoid structure
-- b2 :: Barbie ('Data.Functor.Const.Const' a)  -- 'Data.Functor.Barbie.Container' Barbie
-- b3 :: Barbie 'Data.Functor.Identity.Identity'   -- Barbie's new clothes
-- @
--
-- This module define the classes to work with these types and easily
-- transform them. They all come with default instances based on
-- `GHC.Generics.Generic`, so using them is as easy as:
--
-- @
-- data Barbie f
--   = Barbie
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
--   deriving
--     ( 'GHC.Generics.Generic'
--     , 'FunctorB', 'TraversableB', 'ProductB', 'ConstraintsB', 'ProductBC'
--     )
--
-- deriving instance 'AllBF' 'Show' f Barbie => 'Show' (Barbie f)
-- deriving instance 'AllBF' 'Eq'   f Barbie => 'Eq'   (Barbie f)
-- @
--
-- Sometimes one wants to use @Barbie 'Data.Functor.Identity.Identity'@
-- and it may feel like a second-class record type, where one needs to
-- unpack values in each field. "Barbies.Bare" offers a way to have
-- bare versions of a barbie-type.
--
-- Notice that all classes in this package are poly-kinded. Intuitively,
-- a barbie is a type parameterised by a functor, and because a barbies is
-- a type of functor, a type parameterised by a barbie is a (higher-kinded)
-- barbie too:
--
-- @
-- data Catalog b
--   = Catalog (b 'Identity') (b 'Maybe')
--   deriving
--     ('GHC.Generics.Generic'
--     , 'FunctorB', 'TraversableB', 'ProductB', 'ConstraintsB', 'ProductBC'
--     )
-- @
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
  , ConstraintsB(AllB, baddDicts)
  , AllBF
    -- ** Utility functions
  , bmapC
  , btraverseC

    -- * Products and constaints
  , ProductBC(bdicts)
    -- ** Utility functions
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

import Data.Barbie.Internal.Constraints (AllBF, ConstraintsB (..), bmapC, btraverseC)

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Instances(Barbie(..))
import Data.Barbie.Internal.Product
  ( ProductB(..)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4
  )
import Data.Barbie.Internal.ProductC(ProductBC(..), buniqC, bmempty)
import Data.Barbie.Internal.Traversable
  ( TraversableB(..)
  , bsequence, bsequence'
  , bfoldMap, btraverse_
  )
import qualified Data.Barbie.Trivial as Trivial

import Data.Generics.GenericN (Rec(..))
