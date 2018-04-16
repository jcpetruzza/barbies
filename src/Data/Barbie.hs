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
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}
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

import Data.Functor.Product (Product(..))
import Data.Functor.Prod

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Traversable(TraversableB(..), bsequence)


-- | Barbie-types that can form products, subject to the laws:
--
-- @
-- 'bmap' 'Data.Bifunctor.first'  . 'uncurry' . 'bprod' = 'Data.Bifunctor.first'
-- 'bmap' 'Data.Bifunctor.second' . 'uncurry' . 'bprod' = 'Data.Bifunctor.second'
-- @
--
-- Notice that because of the laws, having an internal product structure is not
-- enough to have a lawful instance. E.g.
--
-- @
-- data Ok  f = Ok {o1 :: f 'String', o2 :: f 'Int'}        -- has an instance
-- data Bad f = Bad{b1 :: f 'String', hiddenFromArg: 'Int'} -- no lawful instance
-- @
class FunctorB b => ProductB b where
  bprod :: b f -> b g -> b (Product f g)

  -- | Intuitively, the laws for this class require that `b` hides no structure
  --   from its argument @f@. Because of this, any @x :: forall a . f a@
  --   determines a unique value of @b f@. Formally:
  --
  -- @
  -- 'const' ('buniq' x) = 'bmap' ('const' x)
  -- @
  --
  buniq :: (forall a . f a) -> b f


-- | Like 'bprod', but returns a binary 'Prod', instead of 'Product', which
--   composes better.
--
--   See '/*/' for usage.
(/*/)
  :: ProductB b => b f -> b g -> b (Prod '[f, g])
l /*/ r
  = bmap (\(Pair f g) -> Cons f (Cons g Unit)) (l `bprod` r)
infixr 4 /*/

-- | Similar to '/*/' but one of the sides is already a 'Prod fs'.
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
