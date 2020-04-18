{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveB
  ( DistributiveB(..)
  , bshape
  , bdistribute'
  , bcotraverse
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
-- `bdistribute` turns an effectful way of making a Barbie-type
-- into a pure Barbie-type with effectful ways of computing the
-- values of its fields.
--
-- From 'bdistribute', we can define a pure skeleton where very field
-- is a getter of that field:
--
-- @
-- 'bshape' :: ('DistributiveB' b) => b ((->) (b 'Identity'))
-- 'bshape' = 'bdistribute'' 'id'
-- @
--
--  It should satisfy the following law
--
-- @
-- 'bmap' ('Identity' . ('$' b)) 'bshape' = b
-- @
--
--  It is to 'FunctorB' in the same way as 'Distributive'
--  relates to 'Functor'.
--
-- There is a default implementation of 'bdistribute' based on
-- 'Generic'.  Intuitively, it works on product types where the shape
-- of a pure value is uniquely defined and every field is covered by
-- the argument @f@.
class (FunctorB b) => DistributiveB (b :: (k -> Type) -> Type) where
  bdistribute :: Functor f => f (b g) -> b (Compose f g)
  -- bdistribute x = bmap (\f -> Compose $ fmap f . bsequence' <$> x) bshape

  default bdistribute
    :: forall f g
    .  CanDeriveDistributiveB b f g
    => Functor f => f (b g) -> b (Compose f g)
  bdistribute = gbdistributeDefault

bshape :: DistributiveB b => b ((->) (b Identity))
bshape = bdistribute' id

-- | A version of `bdistribute` with @g@ specialized to `Identity`.
bdistribute' :: (DistributiveB b, Functor f) => f (b Identity) -> b f
bdistribute' = bmap (fmap runIdentity . getCompose) . bdistribute

-- | Dual of `Barbies.Internal.TraversableB.btraverse`
bcotraverse :: (DistributiveB b, Functor f) => (forall a . f (g a) -> f a) -> f (b g) -> b f
bcotraverse h = bmap (h . getCompose) . bdistribute

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
