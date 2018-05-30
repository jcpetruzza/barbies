{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.ProofB
  ( ProofB(..)

  , CanDeriveGenericInstance, ConstraintsOfMatchesGenericDeriv
  , GConstraintsOf
  , GProof
  , gbproofDefault
  )

where

import Data.Barbie.Internal.Classification (BarbieType(..), ClassifyBarbie)
import Data.Barbie.Internal.Dicts(DictOf(..), packDict)
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Constraints hiding (CanDeriveGenericInstance, ConstraintsOfMatchesGenericDeriv)
import Data.Barbie.Internal.Product(ProductB(..))
import Data.Barbie.Internal.Tags(P, F)
import Data.Barbie.Internal.Wear(Wear)

import Data.Proxy

import GHC.Generics

-- | Barbie-types with products have a canonical proof of instance.
--
-- There is a default 'bproof' implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: ConstraintsOf c f b => b (DictOf c f)

  default bproof
    :: ( CanDeriveGenericInstance b
       , ConstraintsOfMatchesGenericDeriv c f b
       , ConstraintsOf c f b
       )
    => b (DictOf c f)
  bproof = gbproofDefault

-- | Every type that admits a generic instance of 'ProductB' and
--   'ConstraintsB', has a generic instance of 'ProofB' as well.
type CanDeriveGenericInstance b
  = ( Generic (b (Target P))
    , GProof (ClassifyBarbie b) b (RecRep (b (Target F)))
    , Rep (b (Target P)) ~ Repl' (Target F) (Target P) (RecRep (b (Target F)))
    )

type ConstraintsOfMatchesGenericDeriv c f b
  = ( ConstraintsOf c f b ~ GConstraintsOf c f (RecRep (b (Target F)))
    , ConstraintsOf c f b ~ ConstraintByType (ClassifyBarbie b) c f (RecRep (b (Target F)))
    )

-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'bproof' based on 'Generic'.
gbproofDefault
  :: forall b c f
  .  ( CanDeriveGenericInstance b
     , ConstraintsOfMatchesGenericDeriv c f b
     , ConstraintsOf c f b
     )
  => b (DictOf c f)
gbproofDefault
  = unsafeUntargetBarbie @P $ to $ gbproof pcbf pbt pb
  where
    pcbf = Proxy :: Proxy (c (b f))
    pbt  = Proxy :: Proxy (ClassifyBarbie b)
    pb   = Proxy :: Proxy (RecRep (b (Target F)) x)



class GProof (bt :: BarbieType) b rep where
  gbproof
    :: ( ConstraintByType bt c f rep
       , GConstraintsOf c f (RecRep (b (Target F))) -- for the recursive case!
       )
    => Proxy (c (b f))
    -> Proxy bt
    -> Proxy (rep x)
    -> Repl' (Target F) (Target P) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProof bt b x => GProof bt b (M1 _i _c x) where
  {-# INLINE gbproof #-}
  gbproof pcbf pbt pm1
    = M1 (gbproof pcbf pbt (unM1 <$> pm1))

instance GProof bt b U1 where
  {-# INLINE gbproof #-}
  gbproof _ _ _ = U1

instance (GProof bt b l, GProof bt b r) => GProof bt b (l :*: r) where
  {-# INLINE gbproof #-}
  gbproof pcbf pbt pp
    =
    gbproof pcbf pbt (left <$> pp) :*: gbproof pcbf pbt (right <$> pp)
    where
      left  (l :*: _) = l
      right (_ :*: r) = r


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GProof 'WearBarbie b (K1 R (NonRec (Target (W F) a))) where
  {-# INLINE gbproof #-}
  gbproof pcbf _ _
    = K1 $ unsafeTarget @(W P) (mkProof pcbf)
    where
      mkProof :: (c (f a), Wear f a ~ f a) => Proxy (c (b f)) -> DictOf c f a
      mkProof _ = packDict

instance {-# OVERLAPPING #-} GProof 'NonWearBarbie b (K1 R (NonRec (Target F a))) where
  {-# INLINE gbproof #-}
  gbproof pcbf _ _
    = K1 $ unsafeTarget @P (mkProof pcbf)
    where
      mkProof :: c (f a) => Proxy (c (b f)) -> DictOf c f a
      mkProof _ = packDict

instance {-# OVERLAPPING #-}
  ( CanDeriveGenericInstance b
  , bt ~ ClassifyBarbie b
  )
    => GProof bt b (K1 R (RecUsage (b (Target F)))) where
  {-# INLINE gbproof #-}
  gbproof pcbf pbt _
    = K1 $ to $ gbproof pcbf pbt pr
      where
        pr = Proxy :: Proxy (RecRep (b (Target F)) x)

instance {-# OVERLAPPING #-}
  ProofB b' => GProof bt b (K1 R (NonRec (b' (Target F)))) where
  {-# INLINE gbproof #-}
  gbproof pcbf _ _
    = K1 $ unsafeTargetBarbie @P (proof' pcbf)
    where
      proof' :: ConstraintsOf c f b' => Proxy (c (b f)) -> b' (DictOf c f)
      proof' _ = bproof
