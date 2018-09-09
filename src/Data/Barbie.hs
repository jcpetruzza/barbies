-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie
--
-- A common Haskell idiom is to parameterise a datatype by a type @* -> *@,
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
-- b2 :: Barbie ('Data.Functor.Const.Const' a)  -- 'Data.Barbie.Container.Container' Barbie
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
-- deriving instance 'ConstraintsOf' 'Show' f Barbie => 'Show' (Barbie f)
-- deriving instance 'ConstraintsOf' 'Eq'   f Barbie => 'Eq'   (Barbie f)
-- @
--
-- Sometimes one wants to use @Barbie 'Data.Functor.Identity.Identity'@
-- and it may feels lik a second-class record type, where one needs to
-- unpack values in each field. For those cases, we can leverage on
-- closed type-families ang get the best of both worlds:
--
-- @
-- data 'Bare'
--
-- type family 'Wear' f a where
--   'Wear' 'Bare' a = a
--   'Wear' f      a = f a
--
-- data SignUpForm f
--   = SignUpForm'
--       { username  :: 'Wear' f 'String',
--       , password  :: 'Wear' f 'String'
--       , mailingOk :: 'Wear' f 'Bool'
--       }
--   deriving ( ..., 'BareB')
--
-- type SignUpRaw  = SignUpForm 'Maybe'
-- type SignUpData = SignUpForm 'Bare'
--
-- formData = SignUpForm "jbond" "shaken007" False :: SignUpData
-- @


----------------------------------------------------------------------------
module Data.Barbie
  (
    -- * Functor
    FunctorB(bmap)

    -- * Traversable
  , TraversableB(btraverse)
  , btraverse_
  , bsequence

    -- * Product
  , ProductB(buniq, bprod)
  , (/*/), (/*)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4

    -- * Bare values
  , Wear
  , Bare
  , BareB(bstrip, bcover)
  , bstripFrom
  , bcoverWith

    -- * Constraints and proofs of instance
  , ConstraintsB(AllB, adjProof)
  , ConstraintsOf
  , ClassF
  , ClassFG
  , NotBare
  , ProofB(bproof)

    -- * Wrapper
  , Barbie(..)

    -- * Trivial Barbies
  , Void
  , Unit (..)
  )

where

import Data.Barbie.Internal.Bare(Bare, BareB(..), bstripFrom, bcoverWith, Wear)
import Data.Barbie.Internal.Constraints(ConstraintsB(..), ConstraintsOf)
import Data.Barbie.Internal.Dicts(ClassF, ClassFG)
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Instances(Barbie(..))
import Data.Barbie.Internal.ProofB(ProofB(..))
import Data.Barbie.Internal.Product
  ( ProductB(..)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4
  , (/*/), (/*)
  )
import Data.Barbie.Internal.Traversable(TraversableB(..), bsequence, btraverse_)
import Data.Barbie.Internal.Wear(NotBare)

import Data.Barbie.Trivial(Void, Unit(..))
