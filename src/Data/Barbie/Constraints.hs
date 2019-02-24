-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie
--
-- Support for operating on Barbie-types with constrained functions.
--
-- Consider the following function:
--
-- @
-- showIt :: 'Show' a => 'Maybe' a -> 'Data.Functor.Const' 'String' a
-- showIt = 'Data.Functor.Const' . 'show'
-- @
--
-- We would then like to be able to do:
--
-- @
-- 'Data.Barbie.bmap' 'showIt' :: 'Data.Barbie.FunctorB' b => b 'Maybe' -> b ('Data.Functor.Const' 'String')
-- @
--
-- This however doesn't work because of the @('Show' a)@ constraint in the
-- the type of @showIt@.
--
-- This module adds support to overcome this problem.
----------------------------------------------------------------------------
module Data.Barbie.Constraints
  ( -- * Instance dictionaries
    Dict(..)
  , requiringDict

    -- * Retrieving dictionaries
  , ConstraintsB(..)
  , ProductBC(..)
  , bmapC

  , AllBF
  , ClassF
  , ClassFG

    -- * Deprecated
  , ConstraintsOf
  , adjProof
  , ProofB
  )

where

import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Dicts
import Data.Barbie.Internal.ProductC
