-----------------------------------------------------------------------------
-- |
-- Module:  Barbies
--
-- A common Haskell idiom is to parameterise a datatype by a type @k -> *@,
-- typically a functor or a GADT. These are like outfits of a Barbie,
-- that turn her into a different doll. E.g.
--
-- @
-- data Person f
--   = Person
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
--
-- b1 :: Person 'Data.Monoid.Last'       -- Barbie with a monoid structure
-- b2 :: Person ('Data.Functor.Const.Const' a)  -- 'Barbies.Container' Barbie
-- b3 :: Person 'Data.Functor.Identity.Identity'   -- Barbie's new clothes
-- @
--
-- This package define the classes to work with these types and easily
-- transform them. They all come with default instances based on
-- `GHC.Generics.Generic`, so using them is as easy as:
--
-- @
-- data Person f
--   = Person
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
--   deriving
--     ( 'GHC.Generics.Generic'
--     , 'FunctorB', 'TraversableB', 'ApplicativeB', 'ConstraintsB'
--     )
--
-- deriving instance 'AllBF' 'Show' f Person => 'Show' (Person f)
-- deriving instance 'AllBF' 'Eq'   f Person => 'Eq'   (Person f)
-- @
--

-----------------------------------------------------------------------------
module Barbies
  (  -- * Functor interface

     -- | Barbie-types are functors. That means that if one is familiar
     --   with standard classes like 'Functor', 'Applicative' or 'Traversable',
     --   one already knows how to work with barbie-types too. For instance, just
     --   like one would use:
     --
     -- @
     --   'fmap' f (xs :: [a])@
     -- @
     --
     --   to apply @f@ uniformly on every @a@ occurring
     --   in @xs@, one could use the following to turn a 'Either'-outfit
     --   into 'Maybe'-outfit:
     --
     -- @
     --   'bmap' ('either' (\\_ -> 'Nothing') 'Just') (p :: Person ('Either' e))@
     -- @
     --
     --   In this case, the argument of 'bmap' will have to be applied on all
     --   fields of @p@:
     --
     -- @
     --   name p :: 'Either' e 'String'
     --   age p  :: 'Either' e 'Int'
     -- @
     --
     --   So 'bmap' here demands a polymorphic function of type:
     --
     -- @
     --   forall a . 'Either' e a -> 'Maybe' a@
     -- @
     --
     --   Polymorphic functions with 'Applicative' effects can be applied
     --   using 'btraverse' and the effects will be accumulated. Finally,
     --   'bzipWith' gives us an analogous of 'Control.Applicative.liftA2':
     --
     -- @
     --   addDefaults :: Person 'Maybe' -> Person 'Data.Functor.Identity' -> Person 'Data.Functor.Identity'
     --   addDefaults = 'bzipWith' (\\m d -> 'maybe' d 'pure' m)
     -- @
     --
     --   Notice that here 'bzipWith' has lifted a function of type:
     --
     -- @
     --   forall a. 'Maybe' a -> 'Data.Functor.Identity' a -> 'Data.Functor.Identity' a
     -- @
     --
     --   The 'Functor' class captures endo-functors on 'Data.Kind.Type'. 'FunctorB' is
     --   for functors from indexed-types to 'Data.Kind.Type'. In particular, barbie-types
     --   are indexed-types too, so a type parameterized by a barbie is a
     --   (higher-kinded) barbie as well.
     --
     -- @
     --   data Catalog b
     --     = Catalog (b 'Data.Functor.Identity') (b 'Maybe')
     --     deriving
     --       ('GHC.Generics.Generic'
     --       , 'FunctorB', 'TraversableB', 'ApplicativeB', 'ConstraintsB'
     --       )
     -- @
     module Data.Functor.Barbie

     -- * Bi-functor and nested barbies
   , module Data.Functor.Indexed

   , Bi.Flip(..)
     -- * Container-barbies

     -- | Some clothes make barbies look like containers, and we can make those
     --   behave like normal 'Functor's.

   , Containers.Container(..)
   , Containers.ErrorContainer(..)

    -- * Wrappers

    -- | This can be use with deriving via to automate derivation of instances
    --   for Barbie-types.
   , Wrappers.Barbie(..)

    -- * Trivial Barbies
  , Trivial.Void
  , Trivial.Unit (..)
  ) where

import Barbies.Internal.Containers as Containers

import Data.Functor.Barbie
import Data.Functor.Indexed
import qualified Barbies.Internal.Bi as Bi
import qualified Barbies.Internal.Trivial as Trivial
import qualified Barbies.Internal.Wrappers as Wrappers
