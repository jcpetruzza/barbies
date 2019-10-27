-----------------------------------------------------------------------------
-- |
-- Module:  Barbies
--
-- A common Haskell idiom is to parameterise a datatype by a functor or GADT
-- (or any type with kind @k -> 'Data.Kind.Type'@, a pattern sometimes
-- called <https://reasonablypolymorphic.com/blog/higher-kinded-data/ HKD>).
-- This parameter acts like the outfit of a Barbie, turning it into a different
--  doll. The canonical example would be:
--
-- @
-- data Person f
--   = Person
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
-- @
--
-- Let's say that we are writing an application where 'Person' data
-- will be read from a web form, validated, and stored in a database. Some
-- possibles outfits that we could use along the way are:
--
-- @
-- Person ('Data.Functor.Const.Const' 'String')  -- for the raw input from the web-form,
-- Person ('Either' 'String') -- for the result of parsing and validating,
-- Person 'Data.Functor.Identity.Identity'        -- for the actual data,
-- Person DbColumn        -- To describe how to read / write a 'Person' to the db
--
-- data DbColumn a
--   = DbColumn
--       { colName :: 'String'
--       , fromDb  :: DbDataParser a
--       , toDb    :: a -> DbData
--       }
-- @
--
-- In such application it is likely that one will have lots of types like
-- @Person@ so we will like to handle these transformations uniformly,
-- without boilerplate or repetitions.  This package provides classes to
-- manipulate these types, using notions that are familiar to haskellers like
-- 'Functor', 'Applicative' or 'Traversable'. For example, instead writing
-- an ad-hoc function that checks that all fields have a correct value, like
--
-- @
-- checkPerson :: Person ('Either' 'String') -> 'Either' ['String'] (Person 'Data.Functor.Identity.Identity')
-- @
--
-- we can write only one such function:
--
-- @
-- check :: 'TraversableB' b => b ('Either' 'String') -> 'Either' ['String'] (b 'Data.Functor.Identity.Identity')
-- check be
--   = case 'btraverse' ('either' ('const' 'Nothing') ('Just' . 'Daa.Functor.Identity.Identity')) be of
--       'Just' bi -> 'Right' bi
--       'Nothing' -> 'Left' ('bfoldMap' ('either' (:[]) ('const' [])) be)
-- @
--
--  Moreover, these classes come with default instances based on
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
  (  -- * Functor-based interface

     -- | Barbie-types are functors. That means that if one is familiar
     --   with standard classes like 'Functor', 'Applicative' or 'Traversable',
     --   one already knows how to work with barbie-types too. For instance, just
     --   like one would use:
     --
     -- @
     -- 'fmap' f (as :: [a])
     -- @
     --
     --   to apply @f@ uniformly on every @a@ occurring
     --   in @as@, one could use the following to turn a 'Either'-outfit
     --   into 'Maybe'-outfit:
     --
     -- @
     -- 'bmap' ('either' ('const' 'Nothing') 'Just') (p :: Person ('Either' e))
     -- @
     --
     --   In this case, the argument of 'bmap' will have to be applied on all
     --   fields of @p@:
     --
     -- @
     -- name p :: 'Either' e 'String'
     -- age  p :: 'Either' e 'Int'
     -- @
     --
     --   So 'bmap' here demands a polymorphic function of type:
     --
     -- @
     -- forall a . 'Either' e a -> 'Maybe' a
     -- @
     --
     --   That is why `bmap` has a rank-2 type:
     --
     -- @
     -- 'bmap' :: 'FunctorB' b => (forall a. f a -> g a) -> b f -> b g
     -- @
     --
     --   Polymorphic functions with 'Applicative' effects can be applied
     --   using 'btraverse' and the effects will be accumulated:
     --
     -- @
     -- 'btraverse' :: ('TraversableB' b, 'Applicative' t) => (forall a. f a -> t (g a)) -> b f -> t (b g)
     -- @
     --
     --   Finally, some barbie-types (typically records like @Person@) have an
     --   'Applicative' structure, and allow us to lift pure n-ary functions
     --   to functions on barbie-types. For example, 'bzipWith' gives us an analogous
     --   of 'Control.Applicative.liftA2':
     --
     -- @
     -- 'bzipWith' :: 'ApplicativeB' b => (forall a. f a -> g a -> h a) -> b f -> b g -> b h
     -- @
     --
     -- We can use this to combine barbies:
     --
     -- @
     -- addDefaults :: Person 'Maybe' -> Person 'Data.Functor.Identity' -> Person 'Data.Functor.Identity'
     -- addDefaults = 'bzipWith' (\\m d -> 'maybe' d 'pure' m)
     -- @
     --
     --   Why is there not a @MonadB@ class as well? As everyone knows,
     --   <https://james-iry.blogspot.com/2009/05/brief-incomplete-and-mostly-wrong.html a monad is just a monoid in the category of endofunctors>,
     --   which in this case is a problem, since barbie-types are not endofunctors:
     --   they map indexed-types to types, unlike the 'Functor' class, that
     --   captures endo-functors on 'Data.Kind.Type'.
     --
     --   All these classes are defined in this module:
     module Data.Functor.Barbie

     -- * Bi-functor and nested barbies
   , module Data.Functor.Indexed
   , module Barbies.Bi


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
import Barbies.Bi
import qualified Barbies.Internal.Trivial as Trivial
import qualified Barbies.Internal.Wrappers as Wrappers
