-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Prod
--
-- Generalize the standard two-functor 'Product' to the product of
-- @n@-functors. Intuitively, this means:
--
-- @
-- 'Product' f g a ~~ (f a, g a)
--
-- 'Prod' '[]        a ~~  Const () a
-- 'Prod' '[f]       a ~~ (f a)
-- 'Prod' '[f, g]    a ~~ (f a, g a)
-- 'Prod' '[f, g, h] a ~~ (f a, g a, h a)
--     ⋮
-- @
----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Functor.Prod
  ( -- * n-tuples of functors.
    Prod(Unit, Cons)
  , zeroTuple
  , oneTuple
  , fromProduct
  , toProduct

    -- * Flat product of functor products
  , prod

    -- * Lifting functions
  , uncurryn

    -- * Type-level helpers
  , type (++)
  , Curried
  )

where

import Control.Applicative(Alternative(..))
import Data.Functor.Product(Product(..))
import Data.Functor.Classes(Eq1(..), Ord1(..), Show1(..))

import qualified Data.Functor.Classes as FC

-- | Product of n functors.
data Prod :: [* -> *] -> * -> * where
  Unit :: Prod '[] a
  Cons :: (f a) -> Prod fs a -> Prod (f ': fs) a

-- | The unit of the product.
zeroTuple :: Prod '[] a
zeroTuple
  = Unit

-- | Lift a functor to a 1-tuple.
oneTuple :: f a -> Prod '[f] a
oneTuple fa
  = Cons fa Unit

-- | Conversion from a standard 'Product'
fromProduct :: Product f g a -> Prod '[f, g] a
fromProduct (Pair fa ga)
  = Cons fa $ Cons ga Unit

-- | Conversion to a standard 'Product'
toProduct :: Prod '[f, g] a -> Product f g a
toProduct (Cons fa (Cons ga Unit))
  = Pair fa ga


-- | Flat product of products.
prod :: Prod ls a -> Prod rs a -> Prod (ls ++ rs) a
l `prod` r =
  case l of
    Unit -> r
    Cons la l' -> Cons la (l' `prod` r)

-- | Type-level, poly-kinded, list-concatenation.
type family (++) l r :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

-- --------------------------------------------------------------
-- Uncurrying of functions
-- --------------------------------------------------------------

-- | @'Prod' '[f, g, h] a -> r@ is the type of the uncurried form
--   of a function @f a -> g a -> h a -> r@. 'Curried' moves from
--   the former to the later. E.g.
--
-- @
-- 'Curried' ('Prod' '[]  a    -> r) = r a
-- 'Curried' ('Prod' '[f] a    -> r) = f a -> r a
-- 'Curried' ('Prod' '[f, g] a -> r) = f a -> g a -> r a
-- @
type family Curried t  where
  Curried (Prod '[] a -> r a) = r a
  Curried (Prod (f ': fs) a -> r a) = f a -> Curried (Prod fs a -> r a)

-- | Like 'uncurry' but using 'Prod' instead of pairs. Can
--   be thought of as a family of functions:
--
-- @
-- 'uncurryn' :: r a -> 'Prod' '[] a
-- 'uncurryn' :: (f a -> r a) -> 'Prod' '[f] a
-- 'uncurryn' :: (f a -> g a -> r a) -> 'Prod' '[f, g] a
-- 'uncurryn' :: (f a -> g a -> h a -> r a) -> 'Prod' '[f, g, h] a
--         ⋮
-- @
uncurryn :: Curried (Prod fs a -> r a) -> Prod fs a -> r a
uncurryn fun = \case
  Unit -> fun
  Cons fa fs' ->
    let fun' = fun fa
    in uncurryn fun' fs'

-- --------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------

-- | Inductively defined instance: @'Functor' ('Prod' '[])@.
instance Functor (Prod '[]) where
  fmap _ Unit = Unit

-- | Inductively defined instance: @'Functor' ('Prod' (f ': fs))@.
instance (Functor f, Functor (Prod fs)) => Functor (Prod (f ': fs))  where
  fmap f (Cons fa fas)
    =  Cons (fmap f fa) (fmap f fas)

-- | Inductively defined instance: @'Applicative' ('Prod' '[])@.
instance Applicative (Prod '[]) where
  pure _
    = Unit

  Unit <*> Unit
    = Unit

-- | Inductively defined instance: @'Applicative' ('Prod' (f ': fs))@.
instance (Applicative f, Applicative (Prod fs)) => Applicative (Prod (f ': fs)) where
  pure a
    = Cons (pure a) (pure a)

  Cons f fs <*> Cons a as
    = Cons (f <*> a) (fs <*> as)

-- | Inductively defined instance: @'Alternative' ('Prod' '[])@.
instance Alternative (Prod '[]) where
  empty
    = Unit

  Unit <|> Unit
    = Unit

-- | Inductively defined instance: @'Alternative' ('Prod' (f ': fs))@.
instance (Alternative f, Alternative (Prod fs)) => Alternative (Prod (f ': fs)) where
  empty
    = Cons empty empty

  Cons f fs <|> Cons g gs
    = Cons (f <|> g) (fs <|> gs)


-- NB. There are Monad instances for `Data.Functor.Product`, but I'm not convinced they
-- make much sense. In particular, we seem to get a O(n^2) bind.

-- | Inductively defined instance: @'Foldable' ('Prod' '[])@.
instance Foldable (Prod '[]) where
  foldMap _ = mempty

-- | Inductively defined instance: @'Foldable' ('Prod' (f ': fs))@.
instance (Foldable f, Foldable (Prod fs)) => Foldable (Prod (f ': fs)) where
  foldMap f (Cons fa fas)
    = foldMap f fa `mappend` foldMap f fas

-- | Inductively defined instance: @'Traversable' ('Prod' '[])@.
instance Traversable (Prod '[]) where
  traverse _ Unit = pure Unit

-- | Inductively defined instance: @'Traversable' ('Prod' (f ': fs))@.
instance (Traversable f, Traversable (Prod fs)) => Traversable (Prod (f ': fs)) where
  traverse f (Cons fa fas)
    = Cons <$> (traverse f fa) <*> (traverse f fas)

-- | Inductively defined instance: @'Eq1' ('Prod' '[])@.
instance Eq1 (Prod '[]) where
  liftEq _ Unit Unit = True

-- | Inductively defined instance: @'Eq1' ('Prod' (f ': fs))@.
instance (Eq1 f, Eq1 (Prod fs)) => Eq1 (Prod (f ': fs)) where
  liftEq eq (Cons l ls) (Cons r rs)
    = liftEq eq l r && liftEq eq ls rs

-- | Inductively defined instance: @'Eq' ('Prod' '[])@.
instance Eq a => Eq (Prod '[] a) where
  (==) = FC.eq1

-- | Inductively defined instance: @'Eq' ('Prod' (f ': fs))@.
instance (Eq1 f, Eq a, Eq1 (Prod fs)) => Eq (Prod (f ': fs) a) where
  (==) = FC.eq1

-- | Inductively defined instance: @'Ord1' ('Prod' '[])@.
instance Ord1 (Prod '[]) where
  liftCompare _ Unit Unit = EQ

-- | Inductively defined instance: @'Ord1' ('Prod' (f ': fs))@.
instance (Ord1 f, Ord1 (Prod fs)) => Ord1 (Prod (f ': fs)) where
  liftCompare cmp (Cons l ls) (Cons r rs)
    = liftCompare cmp l r `mappend` liftCompare cmp ls rs

-- | Inductively defined instance: @'Ord' ('Prod' '[])@.
instance Ord a => Ord (Prod '[] a) where
  compare = FC.compare1

-- | Inductively defined instance: @'Ord' ('Prod' (f ': fs))@.
instance (Ord1 f, Ord a, Ord1 (Prod fs)) => Ord (Prod (f ': fs) a) where
  compare = FC.compare1

-- | Inductively defined instance: @'Show1' ('Prod' '[])@.
instance Show1 (Prod '[]) where
  liftShowsPrec _ _ _ Unit = showString "zeroTuple"

-- | Inductively defined instance: @'Show1' ('Prod' (f ': fs))@.
instance (Show1 f, Show1 (Prod fs)) => Show1 (Prod (f ': fs)) where
  liftShowsPrec sp sl d = \case
    (Cons fa Unit) ->
      showParen (d > 10) $
        showString "oneTuple " . liftShowsPrec sp sl 11 fa
    (Cons fa fas)  ->
      showParen (d > 10) $
        showString "oneTuple " . liftShowsPrec sp sl 11 fa
          . showString " `prod` "
          . liftShowsPrec sp sl 0 fas

-- | Inductively defined instance: @'Show' ('Prod' '[])@.
instance Show a => Show (Prod '[] a) where
  showsPrec = FC.showsPrec1

-- | Inductively defined instance: @'Show' ('Prod' (f ': fs))@.
instance (Show1 f, Show a, Show1 (Prod fs)) => Show (Prod (f ': fs) a) where
  showsPrec = FC.showsPrec1

