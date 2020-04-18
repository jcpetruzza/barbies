{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveB
  ( DistributiveB(..)
  , bdistribute'
  , bcotraverse
  , bdecompose
  , brecompose
  , gbdistributeDefault
  , CanDeriveDistributiveB
  )

where

import Barbies.Internal.FunctorB (FunctorB(..))
import Barbies.Generics.Distributive (GDistributive(..))

import Data.Functor.Compose   (Compose (..))
import Data.Functor.Identity  (Identity (..))
import Data.Functor.Product   (Product (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Distributive
import Data.Kind              (Type)

-- | A 'FunctorB' where the effects can be distributed to the fields:
--  `bdistribute` turns an effectful way of building a Barbie-type
--  into a pure Barbie-type with effectful ways of computing the
--  values of its fields.
--
--  This class is the categorical dual of `Barbies.Internal.TraversableB.TraversableB`,
--  with `bdistribute` the dual of `Barbies.Internal.TraversableB.bsequence`
--  and `bcotraverse` the dual of `Barbies.Internal.TraversableB.btraverse`. As such,
--  instances need to satisfy these laws:
--
-- @
-- 'bdistribute' . h = 'bmap' ('Compose' . h . 'getCompose') . 'bdistribute'    -- naturality
-- 'bdistribute' . 'Data.Functor.Identity' = 'bmap' ('Compose' . 'Data.Functor.Identity')                 -- identity
-- 'bdistribute' . 'Compose' = 'bmap' ('Compose' . 'Compose' . 'fmap' 'getCompose' . 'getCompose') . 'bdistribute' . 'fmap' 'bdistribute' -- composition
-- @
--
-- By specializing @f@ to @((->) a)@ and @g@ to 'Identity', we can define a function that
-- decomposes a function on distributive barbies into a collection of simpler functions:
--
-- @
-- 'bdecompose' :: 'DistributiveB' b => (a -> b 'Identity') -> b ((->) a)
-- 'bdecompose' = 'bmap' ('fmap' 'runIdentity' . 'getCompose') . 'bdistribute'
-- @
--
-- Lawful instances of the class can then be characterized as those that satisfy:
--
-- @
-- 'brecompose' . 'bdecompose' = 'id'
-- 'bdecompose' . 'brecompose' = 'id'
-- @
--
-- This means intuitively that instances need to have a fixed shape (i.e. no sum-types can be involved).
-- Typically, this means record types, as long as they don't contain fields where the functor argument is not applied.
--
--
-- There is a default implementation of 'bdistribute' based on
-- 'Generic'.  Intuitively, it works on product types where the shape
-- of a pure value is uniquely defined and every field is covered by
-- the argument @f@.
class (FunctorB b) => DistributiveB (b :: (k -> Type) -> Type) where
  bdistribute :: Functor f => f (b g) -> b (Compose f g)

  default bdistribute
    :: forall f g
    .  CanDeriveDistributiveB b f g
    => Functor f => f (b g) -> b (Compose f g)
  bdistribute = gbdistributeDefault


-- | A version of `bdistribute` with @g@ specialized to `Identity`.
bdistribute' :: (DistributiveB b, Functor f) => f (b Identity) -> b f
bdistribute' = bmap (fmap runIdentity . getCompose) . bdistribute

-- | Dual of `Barbies.Internal.TraversableB.btraverse`
bcotraverse :: (DistributiveB b, Functor f) => (forall a . f (g a) -> f a) -> f (b g) -> b f
bcotraverse h = bmap (h . getCompose) . bdistribute

-- | Decompose a function returning a distributive barbie, into
--   a collection of simpler functions.
bdecompose :: DistributiveB b => (a -> b Identity) -> b ((->) a)
bdecompose = bdistribute'

-- | Recompose a decomposed function.
brecompose :: FunctorB b => b ((->) a) -> a -> b Identity
brecompose bfs = \a -> bmap (Identity . ($ a)) bfs

-- | @'CanDeriveDistributiveB' B f g@ is in practice a predicate about @B@ only.
--   Intuitively, it says the the following holds  for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @(B f)@ has only one constructor, and doesn't contain "naked" fields
--       (that is, not covered by `f`).
--
--     * @B f@ can contain fields of type @b f@ as long as there exists a
--       @'DistributiveB' b@ instance. In particular, recursive usages of @B f@
--       are allowed.
--
--     * @B f@ can also contain usages of @b f@ under a @'Distributive' h@.
--       For example, one could use @a -> (B f)@ as a field of @B f@.
type CanDeriveDistributiveB b f g
  = ( GenericP 0 (b g)
    , GenericP 0 (b (Compose f g))
    , GDistributive 0 f (RepP 0 (b g)) (RepP 0 (b (Compose f g)))
    )

-- | Default implementation of 'bdistribute' based on 'Generic'.
gbdistributeDefault
  :: CanDeriveDistributiveB b f g
  => Functor f => f (b g) -> b (Compose f g)
gbdistributeDefault
  = toP (Proxy @0) . gdistribute (Proxy @0) . fmap (fromP (Proxy @0))
{-# INLINE gbdistributeDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for DistributiveB
-- -----------------------------------------------------------

type P = Param

instance
  ( Functor f
  , DistributiveB b
  ) => GDistributive 0 f (Rec (b' (P 0 g)) (b g)) (Rec (b' (P 0 (Compose f g))) (b (Compose f g)))
  where
  gdistribute _ = Rec . K1 . bdistribute . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}


instance
  ( Functor f
  , Distributive h
  , DistributiveB b
  ) =>
  GDistributive n f (Rec (h (b (P n g))) (h (b g))) (Rec (h (b (P n (Compose f g)))) (h (b (Compose f g))))
  where
  gdistribute _ = Rec . K1 . fmap bdistribute . distribute . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}

-- --------------------------------
-- Instances for base types
-- --------------------------------

instance DistributiveB Proxy where
  bdistribute _ = Proxy
  {-# INLINE bdistribute #-}

fstF :: Product f g a -> f a
fstF (Pair x _y) = x

sndF :: Product f g a -> g a
sndF (Pair _x y) = y

instance (DistributiveB a, DistributiveB b) => DistributiveB (Product a b) where
  bdistribute xy = Pair (bdistribute $ fstF <$> xy) (bdistribute $ sndF <$> xy)
  {-# INLINE bdistribute #-}

instance (Distributive h, DistributiveB b) => DistributiveB (h `Compose` b) where
  bdistribute = Compose . fmap bdistribute . distribute . fmap getCompose
  {-# INLINE bdistribute #-}
