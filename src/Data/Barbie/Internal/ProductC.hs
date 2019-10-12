{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Data.Barbie.Internal.ProductC
  ( ProductBC(..)
  , buniqC

  , CanDeriveProductBC
  , GAll
  , GProductBC(..)
  , gbdictsDefault
  )

where

import Barbies.Generics.Constraints(GAll, Self, Other, X)
import Barbies.Internal.ConstraintsB(ConstraintsB(..), GAllRepB)
import Barbies.Internal.Dicts(Dict (..), requiringDict)
import Barbies.Internal.FunctorB(FunctorB(bmap))
import Barbies.Internal.Trivial(Unit(..))
import Barbies.Internal.Wrappers(Barbie(..))

import Data.Barbie.Internal.Product(ProductB(..))
import Data.Generics.GenericN

import Data.Functor.Product (Product (..))
import Data.Kind(Type)
import Data.Proxy(Proxy (..))

class (ConstraintsB b, ProductB b) => ProductBC (b :: (k -> Type) -> Type) where
  bdicts :: AllB c b => b (Dict c)

  default bdicts :: (CanDeriveProductBC c b, AllB c b) => b (Dict c)
  bdicts = gbdictsDefault


type CanDeriveProductBC c b
  = ( GenericN (b (Dict c))
    , AllB c b ~ GAll 0 c (GAllRepB b)
    , GProductBC c (GAllRepB b) (RepN (b (Dict c)))
    )

{-# DEPRECATED buniqC "Use bpureC instead" #-}
buniqC :: forall c f b . (AllB c b, ProductBC b) => (forall a . c a => f a) -> b f
buniqC x
  = bmap (requiringDict @c x) bdicts

instance ProductBC b => ProductBC (Barbie b) where
  bdicts = Barbie bdicts

instance ProductBC Unit where
  bdicts = Unit


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'bproof' based on 'Generic'.
gbdictsDefault
  :: forall b c
  .  ( CanDeriveProductBC c b
     , AllB c b
     )
  => b (Dict c)
gbdictsDefault
  = toN $ gbdicts @c @(GAllRepB b)
{-# INLINE gbdictsDefault #-}


class GProductBC c repbx repbd where
  gbdicts :: GAll 0 c repbx => repbd x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProductBC c repbx repbd => GProductBC c (M1 i k repbx) (M1 i k repbd) where
  gbdicts = M1 (gbdicts @c @repbx)
  {-# INLINE gbdicts #-}

instance GProductBC c U1 U1 where
  gbdicts = U1
  {-# INLINE gbdicts #-}

instance
  ( GProductBC c lx ld
  , GProductBC c rx rd
  ) => GProductBC c (lx :*: rx)
                    (ld :*: rd) where
  gbdicts = gbdicts @c @lx @ld :*: gbdicts @c @rx @rd
  {-# INLINE gbdicts #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0

instance GProductBC c (Rec (P0 X a_or_pma) (X a))
                      (Rec (P0 (Dict c) a_or_pma) (Dict c a)) where
  gbdicts = Rec (K1 Dict)
  {-# INLINE gbdicts #-}

instance
  ( ProductBC b
  , AllB c b
  ) => GProductBC c (Rec (Self b' (P0 X)) (b X))
                    (Rec      (b' (P0 (Dict c)))
                              (b     (Dict c))) where
  gbdicts = Rec $ K1 $ bdicts @_ @b

instance
  ( ProductBC b
  , AllB c b
  ) => GProductBC c (Rec (Other b' (P0 X)) (b X))
                    (Rec       (b' (P0 (Dict c)))
                               (b      (Dict c))) where
  gbdicts = Rec $ K1 $ bdicts @_ @b


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance ProductBC Proxy where
  bdicts = Proxy
  {-# INLINE bdicts #-}

instance (ProductBC a, ProductBC b) => ProductBC (Product a b) where
  bdicts = Pair bdicts bdicts
  {-# INLINE bdicts #-}
