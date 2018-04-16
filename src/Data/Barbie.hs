-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie
--
-- A common Haskell idiom is to parameterise a datatype by a type @* -> *@,
-- typically a functor or a GADT. These behave like the clothings of a Barbie,
-- that transform them in a different doll. E.g.
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
-- transform them.
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
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Product(ProductB(..), (/*/), (/*))
import Data.Barbie.Internal.Traversable(TraversableB(..), bsequence)
