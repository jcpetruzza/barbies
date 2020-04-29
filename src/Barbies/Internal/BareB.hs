{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.BareB
  ( Wear, Bare, Covered
  , BareB(..)
  , bstripFrom, bcoverWith
  , WearTwo

  , gbstripDefault
  , gbcoverDefault

  , CanDeriveBareB
  )

where

import Barbies.Generics.Bare(GBare(..))
import Barbies.Internal.FunctorB (FunctorB(..))
import Barbies.Internal.Wear(Bare, Covered, Wear, WearTwo)
import Data.Functor.Identity (Identity(..))

import Data.Generics.GenericN
import Data.Proxy (Proxy(..))


-- | Class of Barbie-types defined using 'Wear' and can therefore
--   have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB (b Covered) => BareB b where
    bstrip :: b Covered Identity -> b Bare Identity
    bcover :: b Bare Identity -> b Covered Identity

    default bstrip :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
    bstrip = gbstripDefault

    default bcover :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
    bcover = gbcoverDefault

-- | Generalization of 'bstrip' to arbitrary functors
bstripFrom :: BareB b => (forall a . f a -> a) -> b Covered f -> b Bare Identity
bstripFrom f
  = bstrip . bmap (Identity . f)

-- | Generalization of 'bcover' to arbitrary functors
bcoverWith :: BareB b => (forall a . a -> f a) -> b Bare Identity -> b Covered f
bcoverWith f
  = bmap (f . runIdentity) . bcover


-- | All types that admit a generic 'FunctorB' instance, and have all
--   their occurrences of @f@ under a 'Wear' admit a generic 'BareB'
--   instance.
type CanDeriveBareB b
  = ( GenericP 0 (b Bare Identity)
    , GenericP 0 (b Covered Identity)
    , GBare 0 (RepP 0 (b Covered Identity)) (RepP 0 (b Bare Identity))
    )

-- | Default implementation of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
gbstripDefault
  = toP (Proxy @0) . gstrip (Proxy @0) . fromP (Proxy @0)
{-# INLINE gbstripDefault #-}

-- | Default implementation of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
gbcoverDefault
  = toP (Proxy @0) . gcover (Proxy @0) . fromP (Proxy @0)
{-# INLINE gbcoverDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorB
-- -----------------------------------------------------------
type P = Param

instance
  ( BareB b
  ) => GBare 0 (Rec (b Covered (P 0 Identity)) (b Covered Identity))
               (Rec (b Bare    (P 0 Identity)) (b Bare    Identity))
  where
  gstrip _ = Rec . K1 . bstrip . unK1 . unRec
  {-# INLINE gstrip #-}

  gcover _ = Rec . K1 .  bcover . unK1 . unRec
  {-# INLINE gcover #-}


instance
  ( Functor h
  , BareB b
  ) =>  GBare 0 (Rec (h (b Covered (P 0 Identity))) (h (b Covered Identity)))
                (Rec (h (b Bare    (P 0 Identity))) (h (b Bare    Identity)))
  where
  gstrip _ = Rec . K1 . fmap bstrip . unK1 . unRec
  {-# INLINE gstrip #-}

  gcover _ = Rec . K1 . fmap bcover . unK1 . unRec
  {-# INLINE gcover #-}

-- This instance is the same as the previous, but for nested Functors
instance
  ( Functor h
  , Functor m
  , BareB b
  ) =>
       GBare 0 (Rec (m (h (b Covered (P 0 Identity)))) (m (h (b Covered Identity))))
               (Rec (m (h (b Bare    (P 0 Identity)))) (m (h (b Bare    Identity))))
  where
  gstrip _ = Rec . K1 . fmap (fmap bstrip) . unK1 . unRec
  {-# INLINE gstrip #-}

  gcover _ = Rec . K1 . fmap (fmap bcover) . unK1 . unRec
  {-# INLINE gcover #-}
