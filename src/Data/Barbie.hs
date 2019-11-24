{-# OPTIONS_GHC -Wno-deprecations #-}
module Data.Barbie
  {-# DEPRECATED "Use Data.Functor.Barbie or Barbies instead" #-}
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
  , CanDeriveProductB

    -- ** Utility functions
  , App.bzip
  , App.bunzip
  , App.bzipWith
  , App.bzipWith3
  , App.bzipWith4

    -- * Constraints and instance dictionaries
  , ConstraintsB(AllB, baddDicts)
  , AllBF
    -- ** Utility functions
  , bmapC
  , btraverseC

    -- * Products and constaints
  , ProductBC(bdicts)
  , CanDeriveProductBC
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
  , GProductB(..)
  , GProductBC(..)

    -- * Deprecations
  , (/*/), (/*)
  )

where

import Barbies.Internal.ConstraintsB (AllBF, ConstraintsB (..), bmapC, btraverseC, bmempty)

import Barbies.Internal.FunctorB(FunctorB(..))
import Barbies.Internal.Wrappers(Barbie(..))
import qualified Barbies.Internal.ApplicativeB as App

import Data.Barbie.Internal.Product(ProductB(..), CanDeriveProductB, GProductB(..))
import Data.Barbie.Internal.ProductC(ProductBC(..), CanDeriveProductBC,  GProductBC(..), buniqC)

import Barbies.Internal.TraversableB
  ( TraversableB(..)
  , bsequence, bsequence'
  , bfoldMap, btraverse_
  )
import qualified Barbies.Internal.Trivial as Trivial

import Data.Functor.Product (Product(Pair))
import Data.Functor.Prod (Prod(..), oneTuple, prod)
import Data.Generics.GenericN (Rec(..))


{-# DEPRECATED (/*/), (/*) "Use bzipWith2, bzipWith3, etc" #-}

-- | Like 'bprod', but returns a binary 'Prod', instead of 'Product', which
--   composes better.
--
--   See '/*/' for usage.
(/*/)
  :: ProductB b => b f -> b g -> b (Prod '[f, g])
l /*/ r
  = bmap (\(Pair f g) -> Cons f (Cons g Unit)) (l `bprod` r)
infixr 4 /*/

-- | Similar to '/*/' but one of the sides is already a @'Prod' fs@.
--
--   Note that '/*', '/*/' and 'Data.Functor.Prod.uncurryn' are meant to be used together:
--   '/*' and '/*/' combine @b f1, b f2...b fn@ into a single product that
--   can then be consumed by using `Data.Functor.Prod.uncurryn` on an n-ary function. E.g.
--
-- @
-- f :: f a -> g a -> h a -> i a
--
-- 'bmap' ('Data.Functor.Prod.uncurryn' f) (bf '/*' bg '/*/' bh)
-- @
(/*) :: ProductB b => b f -> b (Prod fs) -> b (Prod (f ': fs))
l /* r =
  bmap (\(Pair f fs) -> oneTuple f `prod` fs) (l `bprod` r)
infixr 4 /*
