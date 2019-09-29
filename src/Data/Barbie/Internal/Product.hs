{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module Data.Barbie.Internal.Product
  ( ProductB(buniq, bprod)
  , CanDeriveProductB
  , gbprodDefault, gbuniqDefault
  )

where

import Barbies.Internal.Functor (FunctorB)
import Barbies.Internal.Trivial (Unit)
import Barbies.Internal.Wrappers (Barbie(..))
import qualified Barbies.Internal.Applicative as App

import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))

import Data.Generics.GenericN


{-# DEPRECATED ProductB "Use ApplicativeB" #-}
{-# DEPRECATED buniq "Use bpure" #-}
class App.ApplicativeB b => ProductB (b :: (k -> Type) -> Type) where
  bprod :: b f -> b g -> b (f `Product` g)

  buniq :: (forall a . f a) -> b f

  default bprod :: CanDeriveProductB b f g => b f -> b g -> b (f `Product` g)
  bprod = gbprodDefault

  default buniq :: CanDeriveProductB b f f => (forall a . f a) -> b f
  buniq = gbuniqDefault



type CanDeriveProductB b f g
  = ( GenericN (b f)
    , GenericN (b g)
    , GenericN (b (f `Product` g))
    , GProductB f g (RepN (b f)) (RepN (b g)) (RepN (b (f `Product` g)))
    )

instance {-# OVERLAPPABLE #-} (ProductB b, FunctorB b) => App.ApplicativeB b where
  bpure = Data.Barbie.Internal.Product.buniq
  bprod = Data.Barbie.Internal.Product.bprod

instance ProductB Unit where

instance ProductB b => ProductB (Barbie b) where
    buniq x = Barbie (buniq x)
    bprod (Barbie l) (Barbie r) = Barbie (bprod l r)

-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'bprod' based on 'Generic'.
gbprodDefault
  :: forall b f g
  .  CanDeriveProductB b f g
  => b f -> b g -> b (f `Product` g)
gbprodDefault l r
  = toN $ gbprod (Proxy @f) (Proxy @g) (fromN l) (fromN r)
{-# INLINE gbprodDefault #-}

gbuniqDefault:: forall b f . CanDeriveProductB b f f => (forall a . f a) -> b f
gbuniqDefault x
  = toN $ gbuniq (Proxy @f) (Proxy @(RepN (b f))) (Proxy @(RepN (b (f `Product` f)))) x
{-# INLINE gbuniqDefault #-}

class GProductB (f :: k -> *) (g :: k -> *) repbf repbg repbfg where
  gbprod :: Proxy f -> Proxy g -> repbf x -> repbg x -> repbfg x

  gbuniq :: (f ~ g, repbf ~ repbg) => Proxy f -> Proxy repbf -> Proxy repbfg -> (forall a . f a) -> repbf x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProductB f g repf repg repfg => GProductB f g (M1 i c repf)
                                                        (M1 i c repg)
                                                        (M1 i c repfg) where
  gbprod pf pg (M1 l) (M1 r) = M1 (gbprod pf pg l r)
  {-# INLINE gbprod #-}

  gbuniq pf _ _ x = M1 (gbuniq pf (Proxy @repf) (Proxy @repfg) x)
  {-# INLINE gbuniq #-}


instance GProductB f g U1 U1 U1 where
  gbprod _ _ U1 U1 = U1
  {-# INLINE gbprod #-}

  gbuniq _ _ _ _ = U1
  {-# INLINE gbuniq #-}

instance
  ( GProductB f g lf lg lfg
  , GProductB f g rf rg rfg
  ) => GProductB f g (lf  :*: rf)
                     (lg  :*: rg)
                     (lfg :*: rfg) where
  gbprod pf pg (l1 :*: l2) (r1 :*: r2)
    = (l1 `lprod` r1) :*: (l2 `rprod` r2)
    where
      lprod = gbprod pf pg
      rprod = gbprod pf pg
  {-# INLINE gbprod #-}

  gbuniq pf _ _ x = (gbuniq pf (Proxy @lf) (Proxy @lfg) x :*: gbuniq pf (Proxy @rf) (Proxy @rfg) x)
  {-# INLINE gbuniq #-}

-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0

instance GProductB f g (Rec (P0 f a) (f a))
                       (Rec (P0 g a) (g a))
                       (Rec (P0 (f `Product` g) a) ((f `Product` g) a)) where
  gbprod _ _ (Rec (K1 fa)) (Rec (K1 ga))
    = Rec (K1 (Pair fa ga))
  {-# INLINE gbprod #-}

  gbuniq _ _ _ x = Rec (K1 x)
  {-# INLINE gbuniq #-}


instance
  ( SameOrParam b b'
  , ProductB b'
  ) => GProductB f g (Rec (b (P0 f)) (b' f))
                     (Rec (b (P0 g)) (b' g))
                     (Rec (b (P0 (f `Product` g))) (b' (f `Product` g))) where
  gbprod _ _ (Rec (K1 bf)) (Rec (K1 bg))
    = Rec (K1 (bf `bprod` bg))
  {-# INLINE gbprod #-}

  gbuniq _ _ _ x = Rec (K1 (buniq x))
  {-# INLINE gbuniq #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance ProductB Proxy where
  bprod _ _ = Proxy
  {-# INLINE bprod #-}

  buniq _ = Proxy
  {-# INLINE buniq #-}

instance (ProductB a, ProductB b) => ProductB (Product a b) where
  bprod (Pair ll lr) (Pair rl rr) = Pair (bprod ll rl) (bprod lr rr)
  {-# INLINE bprod #-}

  buniq x = Pair (buniq x) (buniq x)
  {-# INLINE buniq #-}
