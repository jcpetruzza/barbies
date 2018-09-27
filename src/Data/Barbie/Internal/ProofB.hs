{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.ProofB
  ( ProofB(..)
  , buniqC
  , bmempty

  , CanDeriveProofB
  , GAllB
  , GProof
  , gbproofDefault
  )

where

import Data.Barbie.Internal.Constraints
import Data.Barbie.Internal.Dicts(ClassF, Dict(..), requiringDict)
import Data.Barbie.Internal.Functor(bmap)
import Data.Barbie.Internal.Product(ProductB(..))

import Data.Generics.GenericN

-- | Barbie-types with products have a canonical proof of instance.
--
-- There is a default 'bproof' implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: AllB c b => b (Dict c)

  default bproof :: (CanDeriveProofB c b, AllB c b) => b (Dict c)
  bproof = gbproofDefault

-- | Every type that admits a generic instance of 'ProductB' and
--   'ConstraintsB', has a generic instance of 'ProofB' as well.
type CanDeriveProofB c b
  = ( GenericN (b (Dict c))
    , AllB c b ~ GAllB c b (RepN (b X))
    , GProof c b (RepN (b X)) (RepN (b (Dict c)))
    )

-- | Like 'buniq' but an constraint is allowed to be required on
--   each element of @b@.
buniqC :: forall c f b . (AllB c b, ProofB b) => (forall a . c a => f a) -> b f
buniqC x
  = bmap (requiringDict @c x) bproof

-- | Builds a @b f@, bu applying 'mempty' on every field of @b@.
bmempty :: forall f b . (AllB (ClassF Monoid f) b, ProofB b) => b f
bmempty
  = buniqC @(ClassF Monoid f) mempty

-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'bproof' based on 'Generic'.
gbproofDefault
  :: forall b c
  .  ( CanDeriveProofB c b
     , AllB c b
     )
  => b (Dict c)
gbproofDefault
  = toN $ gbproof @c @b @(RepN (b X))
{-# INLINE gbproofDefault #-}


class GProof c b repbx repbd where
  gbproof
    :: ( GAllB c b repbx
       , GAllB c b (RepN (b X)) -- for the recursive case
       )
    => repbd x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProof c b repbx repbd => GProof c b (M1 i k repbx) (M1 i k repbd) where
  gbproof = M1 (gbproof @c @b @repbx)
  {-# INLINE gbproof #-}

instance GProof c b U1 U1 where
  gbproof = U1
  {-# INLINE gbproof #-}

instance
  ( GProof c b lx ld
  , GProof c b rx rd
  ) => GProof c b (lx :*: rx)
                  (ld :*: rd) where
  gbproof = gbproof @c @b @lx @ld :*: gbproof @c @b @rx @rd
  {-# INLINE gbproof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0

instance GProof c b (Rec (P0 X a) (X a))
                    (Rec (P0 (Dict c) a) (Dict c a)) where
  gbproof = Rec (K1 Dict)
  {-# INLINE gbproof #-}

instance (ProofB b', AllB c b')
  => GProof c b (Rec (b' (P0 X)) (b' X))
                (Rec (b' (P0 (Dict c))) (b' (Dict c))) where
  gbproof = Rec $ K1 $ bproof @b'
