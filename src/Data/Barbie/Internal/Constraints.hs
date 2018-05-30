{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( ConstraintsB(..)

  , CanDeriveGenericInstance
  , ConstraintsOfMatchesGenericDeriv
  , GConstraintsOf
  , GAdjProof
  , gadjProofDefault

  , ConstraintByType
  )

where

import Data.Barbie.Internal.Classification (BarbieType(..), ClassifyBarbie, GClassifyBarbie)
import Data.Barbie.Internal.Dicts(DictOf(..), packDict)
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Tags(F, PxF)
import Data.Barbie.Internal.Wear(Wear)

import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

import Data.Proxy

import GHC.Generics


-- | Example definition:
  --
  -- @
  -- data T f = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
  --
  -- instance 'ConstraintsB' T where
  --   type 'ConstraintsOf' c f T = (c (f 'Int'), c (f 'String'), c (f 'Bool'))
  -- @
  --
  -- There is a default implementation of 'ConstraintsOf' for
  -- 'Generic' types, so in practice one can simply do:
  --
  -- @
  -- derive instance 'Generic' T
  -- instance 'ConstraintsB' T
  -- @
class FunctorB b => ConstraintsB b where
  -- | @'ConstraintsOf' c f b@ should contain a constraint @c (f x)@
  --  for each @f x@ occurring in @b@. E.g.:
  --
  -- @
  -- 'ConstraintsOf' 'Show' f Barbie = ('Show' (f 'String'), 'Show' (f 'Int'))
  -- @
  type ConstraintsOf (c :: * -> Constraint) (f :: * -> *) b :: Constraint
  type ConstraintsOf c f b = GConstraintsOf c f (RecRep (b (Target F)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof
    :: forall c f
    .  ConstraintsOf c f b
    => b f -> b (Product (DictOf c f) f)

  default adjProof
    :: forall c f
    .  ( CanDeriveGenericInstance b
       , ConstraintsOfMatchesGenericDeriv c f b
       , ConstraintsOf c f b
       )
    => b f -> b (Product (DictOf c f) f)
  adjProof = gadjProofDefault

-- | Intuivively, the requirements to have @'ConstraintsB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'ConstraintsB' instance.
type CanDeriveGenericInstance b
  = ( Generic (b (Target F))
    , Generic (b (Target PxF))
    , GAdjProof (ClassifyBarbie b) b (RecRep (b (Target F)))
    , Rep (b (Target PxF)) ~ Repl' (Target F) (Target PxF) (RecRep (b (Target F)))
    )

type ConstraintsOfMatchesGenericDeriv c f b
  = ( ConstraintsOf c f b ~ GConstraintsOf c f (RecRep (b (Target F)))
    , ConstraintsOf c f b ~ ConstraintByType (ClassifyBarbie b) c f (RecRep (b (Target F)))
    )


-- ===============================================================
--  Generic derivations
-- ===============================================================

type family ConstraintByType bt (c :: * -> Constraint) (f :: * -> *) r :: Constraint where
  ConstraintByType bt c f (M1 _i _c x) = ConstraintByType bt c f x
  ConstraintByType bt c f V1 = ()
  ConstraintByType bt c f U1 = ()
  ConstraintByType bt c f (l :*: r) = (ConstraintByType bt c f l, ConstraintByType bt c f r)
  ConstraintByType bt c f (l :+: r) = (ConstraintByType bt c f l, ConstraintByType bt c f r)
  ConstraintByType 'WearBarbie c f (K1 R (NonRec (Target (W F) a))) = (c (Wear f a), Wear f a ~ f a)
  ConstraintByType 'NonWearBarbie c f (K1 R (NonRec (Target F a))) = c (f a)
  ConstraintByType bt c f (K1 R (NonRec (b (Target F)))) = ConstraintsOf c f b
  ConstraintByType bt c f (K1 R (RecUsage (b (Target F)))) = () -- break recursion
  ConstraintByType bt c f (K1 _i _c) = ()

type GConstraintsOf c f r
  = ConstraintByType (GClassifyBarbie r) c f r


-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  . ( CanDeriveGenericInstance b
    , ConstraintsOfMatchesGenericDeriv c f b
    , ConstraintsOf c f b
    )
  => b f -> b (Product (DictOf c f) f)
gadjProofDefault b
  = unsafeUntargetBarbie @PxF $ to $
      gadjProof pcbf pbt $ fromWithRecAnn (unsafeTargetBarbie @F b)
  where
    pcbf = Proxy :: Proxy (c (b f))
    pbt  = Proxy :: Proxy (ClassifyBarbie b)


class GAdjProof (bt :: BarbieType) b rep where

  gadjProof
    :: ( ConstraintByType bt c f rep
       , GConstraintsOf c f (RecRep (b (Target F))) -- for the recursive case!
       )
    => Proxy (c (b f))
    -> Proxy bt
    -> rep x
    -> Repl' (Target F) (Target PxF) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAdjProof bt b x => GAdjProof bt b (M1 _i _c x) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf pbt (M1 x)
    = M1 (gadjProof pcbf pbt x)

instance GAdjProof bt b V1 where
  gadjProof _ _ _ = undefined

instance GAdjProof bt b U1 where
  {-# INLINE gadjProof #-}
  gadjProof _ _ u1 = u1

instance (GAdjProof bt b l, GAdjProof bt b r) => GAdjProof bt b (l :*: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf pbt (l :*: r)
    = (gadjProof pcbf pbt l) :*: (gadjProof pcbf pbt r)

instance (GAdjProof bt b l, GAdjProof bt b r) => GAdjProof bt b (l :+: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf pbt = \case
    L1 l -> L1 (gadjProof pcbf pbt l)
    R1 r -> R1 (gadjProof pcbf pbt r)


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GAdjProof 'WearBarbie b (K1 R (NonRec (Target (W F) a))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf _ (K1 (NonRec fa))
    = K1 $ unsafeTarget @(W PxF) (Pair (mkProof pcbf) $ unsafeUntarget @(W F) fa)
    where
      mkProof :: (c (f a), Wear f a ~ f a) => Proxy (c (b f)) -> DictOf c f a
      mkProof _ = packDict


instance {-# OVERLAPPING #-} GAdjProof 'NonWearBarbie b (K1 R (NonRec (Target F a))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf _ (K1 (NonRec fa))
    = K1 $ unsafeTarget @PxF (Pair (mkProof pcbf) $ unsafeUntarget @F fa)
    where
      mkProof :: c (f a) => Proxy (c (b f)) -> DictOf c f a
      mkProof _ = packDict


instance {-# OVERLAPPING #-}
  ( CanDeriveGenericInstance b
  , bt ~ ClassifyBarbie b
  )
    => GAdjProof bt b (K1 R (RecUsage (b (Target F)))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf pbt (K1 (RecUsage bf))
    = K1 $ to $ gadjProof pcbf pbt $ fromWithRecAnn bf

instance {-# OVERLAPPING #-}
  ConstraintsB b'
    => GAdjProof bt b (K1 R (NonRec (b' (Target F)))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf _ (K1 (NonRec bf))
    = K1 $ unsafeTargetBarbie @PxF $ adjProof' pcbf $ unsafeUntargetBarbie @F bf
    where
      adjProof'
        :: ConstraintsOf c f b'
        => Proxy (c (b f)) -> b' f -> b' (Product (DictOf c f) f)
      adjProof' _ = adjProof

instance
  (K1 i a) ~ Repl' (Target F) (Target PxF) (K1 i (NonRec a))
    => GAdjProof bt b (K1 i (NonRec a)) where
  {-# INLINE gadjProof #-}
  gadjProof _ _ (K1 (NonRec a)) = K1 a
