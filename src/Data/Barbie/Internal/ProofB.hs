{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.ProofB
  ( ProofB(..)

  , CanDeriveGenericInstance
  , GAllB
  , GProof
  , gbproofDefault
  )

where

import Data.Barbie.Internal.Dicts(Dict(..))
import Data.Barbie.Internal.Constraints hiding (CanDeriveGenericInstance)
import Data.Barbie.Internal.Product(ProductB(..))
import Data.Barbie.Internal.Tag (Tag(..), CoercibleTag(..))

import GHC.Generics

-- | Barbie-types with products have a canonical proof of instance.
--
-- There is a default 'bproof' implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: AllB c b => b (Dict c)

  default bproof :: (CanDeriveGenericInstance c b, AllB c b) => b (Dict c)
  bproof = gbproofDefault

data P_
type P = Tag P_

-- | Every type that admits a generic instance of 'ProductB' and
--   'ConstraintsB', has a generic instance of 'ProofB' as well.
type CanDeriveGenericInstance c b
  = ( Generic (b (P (Dict c)))
    , CoercibleTag P b (Dict c)
    , AllB c b ~ GAllB c b (P (Dict c)) (Rep (b (P (Dict c))))
    , GProof c b (Rep (b (P (Dict c))))
    )

-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'bproof' based on 'Generic'.
gbproofDefault
  :: forall b c
  .  ( CanDeriveGenericInstance c b
     , AllB c b
     )
  => b (Dict c)
gbproofDefault
  = coerceUntag @P $ to $ gbproof @c @b


class GProof c b repbd where
  gbproof
    :: ( GAllB c b (P (Dict c)) repbd
       , GAllB c b (P (Dict c)) (Rep (b (P (Dict c)))) -- for the recursive case
       )
    => repbd x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProof c b repbd => GProof c b (M1 i k repbd) where
  gbproof = M1 (gbproof @c @b)
  {-# INLINE gbproof #-}

instance GProof c b U1 where
  gbproof = U1
  {-# INLINE gbproof #-}

instance (GProof c b l, GProof c b r) => GProof c b (l :*: r) where
  gbproof = gbproof @c @b @l :*: gbproof @c @b @r
  {-# INLINE gbproof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance GProof c b (Rec0 (P (Dict c) a)) where
  gbproof = K1 (Tag Dict)
  {-# INLINE gbproof #-}

instance
  ( Generic (b (P (Dict c)))
  , GProof c b (Rep  (b (P (Dict c))))
  ) => GProof c b (Rec0 (b (P (Dict c)))) where
  gbproof = K1 $ to $ gbproof @c @b


instance {-# OVERLAPPABLE #-}
  ( ProofB b'
  , AllB c b'
  , CoercibleTag P b' (Dict c)
  ) => GProof c b (Rec0 (b' (P (Dict c)))) where
  gbproof = K1 $ coerceTag @P (bproof @b')
