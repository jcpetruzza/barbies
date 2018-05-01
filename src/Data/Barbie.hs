-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie
--
-- A common Haskell idiom is to parameterise a datatype by a type @* -> *@,
-- typically a functor or a GADT. These behave like the clothes of a Barbie,
-- that make them a different doll. E.g.
--
-- @
-- data Barbie f
--   = Barbie
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
--
-- b1 :: Barbie 'Data.Monoid.Last'       -- Barbie with a monoid structure
-- b2 :: Barbie ('Data.Functor.Const.Const' a)  -- container Barbie
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
--     , 'FunctorB', 'TraversableB', 'ProductB', 'ConstraintsB', 'ProofB'
--     )
--
-- deriving instance 'ConstraintsOf' 'Show' f Barbie => 'Show' Barbie
-- deriving instance 'ConstraintsOf' 'Eq'   f Barbie => 'Eq'   Barbie
--
-- @
----------------------------------------------------------------------------
module Data.Barbie
  (
    -- * Functor
    FunctorB(bmap)

    -- * Traversable
  , TraversableB(btraverse)
  , bsequence

    -- * Product
  , ProductB(buniq, bprod)
  , (/*/), (/*)

    -- * Bare values
  , Wear
  , Bare
  , BareB(bstrip, bcover)
  , bstripFrom
  , bcoverWith

    -- * Constraints and proofs of instance
  , ConstraintsB(ConstraintsOf, adjProof)
  , ProofB(bproof)
  )

where

import Data.Barbie.Internal.Bare(Bare, BareB(..), bstripFrom, bcoverWith, Wear)
import Data.Barbie.Internal.Constraints(ConstraintsB(..))
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Product(ProductB(..), (/*/), (/*))
import Data.Barbie.Internal.Traversable(TraversableB(..), bsequence)
import Data.Barbie.Internal.ProofB(ProofB(..))
