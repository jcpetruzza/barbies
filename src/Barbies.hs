-----------------------------------------------------------------------------
-- |
-- Module:  Barbies
--
-- A common Haskell idiom is to parameterise a datatype by a functor or GADT
-- (or any "indexed type" @k -> 'Data.Kind.Type'@), a pattern
-- sometimes called <https://reasonablypolymorphic.com/blog/higher-kinded-data/ HKD>).
-- This parameter acts like the outfit of a Barbie, turning it into a different
-- doll. The canonical example would be:
--
-- @
-- data Person f
--   = Person
--       { name :: f 'String'
--       , age  :: f 'Int'
--       }
-- @
--
-- Let's say that we are writing an application where @Person@ data
-- will be read from a web form, validated, and stored in a database. Some
-- possibles outfits that we could use along the way are:
--
-- @
-- Person ('Data.Functor.Const.Const' 'String')  -- for the raw input from the web-form,
-- Person ('Either' 'String') -- for the result of parsing and validating,
-- Person 'Data.Functor.Identity.Identity'        -- for the actual data,
-- Person DbColumn        -- To describe how to read / write a @Person@ to the db
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
-- 'Functor', 'Applicative' or 'Traversable'. For example, instead of writing
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
  (  -- * Barbies are functors

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
     --  All these classes, and other convenient functions are found in:
     module Data.Functor.Barbie

     -- * Transformers are functors

     -- | Haskellers may be more used to playing with another family of dolls:
     --   <https://hackage.haskell.org/package/transformers transformers>.
     --   Consider for example the following functor-transformers:
     --
     -- @
     -- 'Data.Functor.Compose.Compose' g f a
     -- 'Control.Monad.Trans.Reader.ReaderT' r f a
     -- 'Control.Monad.Maybe.MaybeT' f a
     -- @
     --
     --  Like with barbies, we can think that different choices of @f@ will
     --  give us a different doll. And if we start thinking about how
     --  to change the outfit of a transformer, we notice that, just like
     --  barbie-types, transformer-types are functors too.
     --
     -- @
     -- 'tmap' :: 'FunctorT' t => (forall a. f a -> g a) -> t f x -> b g x
     -- @
     --
     --  Where 'FunctorB' captures functors from indexed-types to types,
     --  'FunctorT' captures those between indexed-types. And again, we can
     --  identitfy familiar classes of functors: 'ApplicativeT' and 'TraversableT'.
     --
     -- Now, transformers like the ones above, are actually endofunctors, e.g.
     -- they map @'Data.Kind.Type' -> 'Data.Kind.Type'@ to itself. So it makes
     -- sense to classify those that are actually monads: the 'MonadT' class
     -- gives us a notion similar to that of `Control.Monad.Trans.Class.MonadTrans',
     -- in that it lets us lift a value to its transformed version:
     --
     -- @
     -- 'tlift' :: 'MonadT' t => f a -> t f a
     --
     --  -- E.g., using the instance for Compose:
     -- 'tlift' [1, 2, 3] = 'Data.Functor.Compose.Compose' ('Just' [1, 2, 3]) :: 'Data.Functor.Compose' 'Maybe' [] 'Int'
     -- @
     --
     -- Unlike all other classes in this package, 'MonadT' instances need to be written
     -- by hand.
     --
     -- For further details, see:

   , module Data.Functor.Transformer

     -- * Bi-functors and nesting
     --
     -- | A barbie-type that is parametric on an additional functor can be made an
     -- instance of both 'FunctorB' and 'FunctorT'. For example:
     --
     -- @
     -- data B f g = B (f Int) (g Bool)
     --   deriving (Generic)
     --
     -- instance FunctorB (B f)
     -- instance FunctorT B
     -- @
     --
     -- This gives us a a bifunctor on indexed-types, as we can map
     -- simultaneously over both arguments using 'btmap':
     --
     -- @
     -- 'btmap' :: ('FunctorB' (b f), 'FunctorT' b) => (forall a . f a -> f' a) -> (forall a . g a -> g' a) -> b f g -> b f' g'
     -- @
     --
     -- When @f ~ g@, we can use a specialized version of 'btmap':
     --
     -- @
     -- 'btmap1' :: ('FunctorB' (b f), 'FunctorT' b) => (forall a . f a -> f' a) -> b f f -> b f' f'
     -- @
     --
     -- Functions like 'btmap1' can be useful to handle cases where we would like
     -- a barbie-type to occur under the functor-argument. Let's consider an example
     -- of this. Continuing the web form example above, one may want to find out
     -- about a person's dependants and model it as follows:
     --
     -- @
     -- newtype Dependants f
     --   = Dependants { getDependants :: f [Person f] }
     -- @
     --
     -- This has the appeal of letting us distinguish two states:
     --
     -- @
     -- Dependants { getDependants = Just [] }  -- the user declared 0 dependants
     -- Dependants { getDependants = Nothing }  -- the user didn't specify dependants yet
     -- @
     --
     -- Unfortunately, it is not possible to write a 'FunctorB' instance for such
     -- a type (before going on, try to write one yourself!). Intuitively, we would
     -- need to have @'Functor' f@, which we can't assume. However, such a type
     -- can be rewritten as follows:
     --
     -- @
     -- newtype Dependants f' f
     --   = Dependants { getDependants :: f' [Person f] }
     --   deriving (Generic)
     --
     -- instance Functor f' => FunctorB (Dependants f')
     -- instance FunctorT Dependants
     --
     -- type Dependants f = Dependants f f
     -- @
     --
     -- We can thus use 'btmap1' as a poor man's version of 'bmap' for 'Dependants'.
     --
     -- For more details, see:
   , module Barbies.Bi


     -- * Container-barbies

     -- | Some clothes make barbies look like containers, and we can make those
     --   types behave like normal 'Functor's.

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
import Data.Functor.Transformer
import Barbies.Bi
import qualified Barbies.Internal.Trivial as Trivial
import qualified Barbies.Internal.Wrappers as Wrappers
