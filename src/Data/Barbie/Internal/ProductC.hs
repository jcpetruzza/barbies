{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.ProductC
  ( ProductBC(..)
  , buniqC
  , bmempty

  , CanDeriveProductBC
  , GAllB
  , GProductBC(..)
  , gbdictsDefault

    -- DEPRECATED STUFF
  , ProofB
  , bproof
  )

where

import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Dicts(ClassF, Dict(..), requiringDict)
import Data.Barbie.Internal.Functor(bmap)
import Data.Barbie.Internal.Product(ProductB(..))

import Data.Generics.GenericN

-- | Every type @b@ that is an instance of both 'ProductB' and
--   'ConstraintsB' can be made an instance of 'ProductBC'
--   as well.
--
--   Intuitively, in addition to 'buniq' from 'ProductB', one
--   can define 'buniqC' that takes into account constraints:
--
-- @
-- 'buniq' :: (forall a . f a) -> b f
-- 'buniqC' :: 'AllB' c b => (forall a . c a => f a) -> b f
-- @
--
--  For technical reasons, 'buniqC' is not currently provided
--  as a method of this class and is instead defined in terms
--  'bdicts', which is similar to 'baddDicts' but can produce the
--  instance dictionaries out-of-the-blue. 'bdicts' could also be
--  defined in terms of 'buniqC', so they are essentially equivalent.
--
-- @
-- 'bdicts' :: forall c b . 'AllB' c b => b ('Dict' c)
-- 'bdicts' = 'buniqC' ('Dict' @c)
-- @
--
--
-- There is a default implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProductBC b where
  bdicts :: AllB c b => b (Dict c)

  default bdicts :: (CanDeriveProductBC c b, AllB c b) => b (Dict c)
  bdicts = gbdictsDefault

-- | Every type that admits a generic instance of 'ProductB' and
--   'ConstraintsB', has a generic instance of 'ProductBC' as well.
type CanDeriveProductBC c b
  = ( GenericN (b (Dict c))
    , AllB c b ~ GAllB c (GAllBRep b)
    , GProductBC c (GAllBRep b) (RepN (b (Dict c)))
    )

-- | Like 'buniq' but a constraint is allowed to be required on
--   each element of @b@.
buniqC :: forall c f b . (AllB c b, ProductBC b) => (forall a . c a => f a) -> b f
buniqC x
  = bmap (requiringDict @c x) bdicts

-- | Builds a @b f@, by applying 'mempty' on every field of @b@.
bmempty :: forall f b . (AllBF Monoid f b, ProductBC b) => b f
bmempty
  = buniqC @(ClassF Monoid f) mempty


{-# DEPRECATED bproof "Renamed to bdicts" #-}
bproof :: forall b c . (ProductBC b, AllB c b) => b (Dict c)
bproof = bdicts

{-# DEPRECATED ProofB "Class was renamed to ProductBC" #-}
type ProofB b = ProductBC b


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
  = toN $ gbdicts @c @(GAllBRep b)
{-# INLINE gbdictsDefault #-}


class GProductBC c repbx repbd where
  gbdicts :: GAllB c repbx => repbd x

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

instance GProductBC c (Rec (P0 X a) (X a))
                      (Rec (P0 (Dict c) a) (Dict c a)) where
  gbdicts = Rec (K1 Dict)
  {-# INLINE gbdicts #-}

instance
  ( ProductBC b
  , AllB c b
  ) => GProductBC c (Rec (Self b (P0 X)) (b X))
                    (Rec      (b (P0 (Dict c)))
                              (b     (Dict c))) where
  gbdicts = Rec $ K1 $ bdicts @b

instance
  ( SameOrParam b b'
  , ProductBC b'
  , AllB c b'
  ) => GProductBC c (Rec (Other b (P0 X)) (b' X))
                    (Rec       (b (P0 (Dict c)))
                               (b'    (Dict c))) where
  gbdicts = Rec $ K1 $ bdicts @b'
