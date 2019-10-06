{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.BareB
  ( Wear, Bare, Covered
  , BareB(..)
  , bstripFrom, bcoverWith

  , gbstripDefault
  , gbcoverDefault

  , CanDeriveBareB
  )

where

import Barbies.Generics.Bare(GBare(..))
import Barbies.Internal.FunctorB (FunctorB(..))
import Barbies.Internal.Wear(Bare, Covered, Wear)
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


-- | All types that admit a generic FunctorB' instance, and have all
--   their occurrences of 'f' under a 'Wear' admit a generic 'BareB'
--   instance.
type CanDeriveBareB b
  = ( GenericN (b Bare Identity)
    , GenericN (b Covered Identity)
    , GBare 0 (RepN (b Covered Identity)) (RepN (b Bare Identity))
    )

-- | Default implementation of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveBareB b => b Covered Identity -> b Bare Identity
gbstripDefault
  = toN . gstrip (Proxy @0) . fromN
{-# INLINE gbstripDefault #-}

-- | Default implementation of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveBareB b => b Bare Identity -> b Covered Identity
gbcoverDefault
  = toN . gcover (Proxy @0) . fromN
{-# INLINE gbcoverDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorB
-- -----------------------------------------------------------
type P = Param

instance
  ( BareB b
  ) => -- b' is b, maybe with 'Param' annotations
       GBare 0 (Rec (b' (P 1 Covered) (P 0 Identity)) (b Covered Identity))
               (Rec (b' (P 1 Bare)    (P 0 Identity)) (b Bare    Identity))
  where
  gstrip _ = Rec . K1 . bstrip . unK1 . unRec
  {-# INLINE gstrip #-}

  gcover _ = Rec . K1 .  bcover . unK1 . unRec
  {-# INLINE gcover #-}


instance
  ( Functor h
  , BareB b
  ) => -- b' is b, maybe with 'Param' annotations. idem h and h'
       GBare 0 (Rec (h' (b' (P 1 Covered) (P 0 Identity))) (h (b Covered Identity)))
               (Rec (h' (b' (P 1 Bare)    (P 0 Identity))) (h (b Bare    Identity)))
  where
  gstrip _ = Rec . K1 . fmap bstrip . unK1 . unRec
  {-# INLINE gstrip #-}

  gcover _ = Rec . K1 . fmap bcover . unK1 . unRec
  {-# INLINE gcover #-}
