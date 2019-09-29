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

    -- * Deprecations
  , (/*/), (/*)
  )

where

import Barbies.Internal.Constraints (AllBF, ConstraintsB (..), bmapC, btraverseC)

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
--   Note that '/*', '/*/' and 'uncurryn' are meant to be used together:
--   '/*' and '/*/' combine @b f1, b f2...b fn@ into a single product that
--   can then be consumed by using `uncurryn` on an n-ary function. E.g.
--
-- @
-- f :: f a -> g a -> h a -> i a
--
-- 'bmap' ('uncurryn' f) (bf '/*' bg '/*/' bh)
-- @
(/*) :: ProductB b => b f -> b (Prod fs) -> b (Prod (f ': fs))
l /* r =
  bmap (\(Pair f fs) -> oneTuple f `prod` fs) (l `bprod` r)
infixr 4 /*
