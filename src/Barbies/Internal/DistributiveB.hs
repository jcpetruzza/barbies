{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveB
  ( DistributiveB(..)
  , gbdistributeDefault
  , CanDeriveDistributiveB
  , bshape
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

class (FunctorB b) => DistributiveB (b :: (k -> Type) -> Type) where
  bdistribute :: Functor f => f (b g) -> b (Compose f g)

  default bdistribute
    :: forall f g
    .  CanDeriveDistributiveB b f g
    => Functor f => f (b g) -> b (Compose f g)
  bdistribute = gbdistributeDefault

-- Analogous to `bsequence'`
bdistribute' :: (DistributiveB b, Functor f) => f (b Identity) -> b f
bdistribute' = bmap (fmap runIdentity . getCompose) . bdistribute

bshape :: DistributiveB b => b ((->) (b Identity))
bshape = bdistribute' id

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
