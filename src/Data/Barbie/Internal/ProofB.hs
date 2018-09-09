{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.ProofB
  ( ProofB(..)

  , CanDeriveGenericInstance, AllBMatchesGenericDeriv
  , GAllB
  , GProof
  , gbproofDefault
  )

where

import Data.Barbie.Internal.Classification (BarbieType(..), ClassifyBarbie)
import Data.Barbie.Internal.Dicts(Dict(..))
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Constraints hiding (CanDeriveGenericInstance, AllBMatchesGenericDeriv)
import Data.Barbie.Internal.Product(ProductB(..))
import Data.Barbie.Internal.Tags(P, F)

import Data.Proxy

import GHC.Generics

-- | Barbie-types with products have a canonical proof of instance.
--
-- There is a default 'bproof' implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: AllB c b => b (Dict c)

  default bproof
    :: ( CanDeriveGenericInstance b
       , AllBMatchesGenericDeriv c b
       , AllB c b
       )
    => b (Dict c)
  bproof = gbproofDefault

-- | Every type that admits a generic instance of 'ProductB' and
--   'ConstraintsB', has a generic instance of 'ProofB' as well.
type CanDeriveGenericInstance b
  = ( Generic (b (Target P))
    , GProof (ClassifyBarbie b) b (RecRep (b (Target F)))
    , Rep (b (Target P)) ~ Repl' (Target F) (Target P) (RecRep (b (Target F)))
    )

type AllBMatchesGenericDeriv c b
  = ( AllB c b ~ GAllB c (RecRep (b (Target F)))
    , AllB c b ~ ConstraintByType (ClassifyBarbie b) c (RecRep (b (Target F)))
    )

-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'bproof' based on 'Generic'.
gbproofDefault
  :: forall b c
  .  ( CanDeriveGenericInstance b
     , AllBMatchesGenericDeriv c b
     , AllB c b
     )
  => b (Dict c)
gbproofDefault
  = unsafeUntargetBarbie @P $ to $ gbproof pcbf pbt pb
  where
    pcbf = Proxy :: Proxy (c (b f))
    pbt  = Proxy :: Proxy (ClassifyBarbie b)
    pb   = Proxy :: Proxy (RecRep (b (Target F)) x)



class GProof (bt :: BarbieType) b rep where
  gbproof
    :: ( ConstraintByType bt c rep
       , GAllB c (RecRep (b (Target F))) -- for the recursive case!
       )
    => Proxy (c (b f))
    -> Proxy bt
    -> Proxy (rep x)
    -> Repl' (Target F) (Target P) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProof bt b x => GProof bt b (M1 _i _c x) where
  gbproof pcbf pbt pm1
    = M1 (gbproof pcbf pbt (unM1 <$> pm1))
  {-# INLINE gbproof #-}

instance GProof bt b U1 where
  gbproof _ _ _ = U1
  {-# INLINE gbproof #-}

instance (GProof bt b l, GProof bt b r) => GProof bt b (l :*: r) where
  gbproof pcbf pbt pp
    =
    gbproof pcbf pbt (left <$> pp) :*: gbproof pcbf pbt (right <$> pp)
    where
      left  (l :*: _) = l
      right (_ :*: r) = r
  {-# INLINE gbproof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GProof 'WearBarbie b (K1 R (NonRec (Target (W F) a))) where
  gbproof pcbf _ _
    = K1 $ unsafeTarget @(W P) (mkProof pcbf)
    where
      mkProof :: c a => Proxy (c (b f)) -> Dict c a
      mkProof _ = Dict
  {-# INLINE gbproof #-}

instance {-# OVERLAPPING #-} GProof 'NonWearBarbie b (K1 R (NonRec (Target F a))) where
  gbproof pcbf _ _
    = K1 $ unsafeTarget @P (mkProof pcbf)
    where
      mkProof :: c a => Proxy (c (b f)) -> Dict c a
      mkProof _ = Dict
  {-# INLINE gbproof #-}

instance {-# OVERLAPPING #-}
  ( CanDeriveGenericInstance b
  , bt ~ ClassifyBarbie b
  )
    => GProof bt b (K1 R (RecUsage (b (Target F)))) where
  gbproof pcbf pbt _
    = K1 $ to $ gbproof pcbf pbt pr
      where
        pr = Proxy :: Proxy (RecRep (b (Target F)) x)
  {-# INLINE gbproof #-}

instance {-# OVERLAPPING #-}
  ProofB b' => GProof bt b (K1 R (NonRec (b' (Target F)))) where
  gbproof pcbf _ _
    = K1 $ unsafeTargetBarbie @P (proof' pcbf)
    where
      proof' :: AllB c b' => Proxy (c (b f)) -> b' (Dict c)
      proof' _ = bproof
  {-# INLINE gbproof #-}
