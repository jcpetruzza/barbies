{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( DictOf(..)
  , requiringDict

  , ConstraintsB(..)

  -- , CanDeriveGenericInstance
  -- , ConstraintsOfMatchesGenericDeriv
  , GConstraintsOf
  -- , GAdjProof
  -- , gadjProofDefault
  )

where

import Data.Barbie.Internal.Wear(Wear)
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Tags(F)

import Data.Functor.Classes(Show1(..))
import Data.Kind(Constraint)

import GHC.Generics


-- | @'DictOf' c f a@ is evidence that there exists an instance
--   of @c (f a)@.
data DictOf c f a where
  PackedDict :: c (f a) => DictOf c f a



instance Eq (DictOf c f a) where
  _ == _ = True

instance Show (DictOf c f a) where
  showsPrec _ PackedDict = showString "PackedDict"

instance Show1 (DictOf c f) where
  liftShowsPrec _ _ = showsPrec

-- | Turn a constrained-function into an unconstrained one
--   that demands proof-of-instance instead.
requiringDict :: (c (f a) => r) -> (DictOf c f a -> r)
requiringDict r
  = \PackedDict -> r


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

  -- -- | Adjoint a proof-of-instance to a barbie-type.
  -- adjProof :: ConstraintsOf c f b => b f -> b (Product (DictOf c f) f)

  -- default adjProof
  --   :: ( CanDeriveGenericInstance b
  --      , ConstraintsOfMatchesGenericDeriv c f b
  --      , ConstraintsOf c f b
  --      )
  --    => b f -> b (Product (DictOf c f) f)
  -- adjProof = gadjProofDefault

-- | Intuivively, the requirements to have @'ConstraintsB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'ConstraintsB' instance.
-- type CanDeriveGenericInstance b
--   = ( Generic (b (Target F))
--     , Generic (b (Target PxF))
--     , GAdjProof b (RecRep (b (Target F)))
--     , Rep (b (Target PxF)) ~ Repl' (Target F) (Target PxF) (RecRep (b (Target F)))
--     )

-- type ConstraintsOfMatchesGenericDeriv c f b
--   = ConstraintsOf c f b ~ GConstraintsOf c f (RecRep (b (Target F)))


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- NB. Duplicated wrt the definition in 'ProofB' since we don't
-- want to export the 'F' constructor for type-safety.
type family GConstraintsOf (c :: * -> Constraint) (f :: * -> *) r :: Constraint where
  GConstraintsOf c f (M1 _i _c x) = GConstraintsOf c f x
  GConstraintsOf c f V1 = ()
  GConstraintsOf c f U1 = ()
  GConstraintsOf c f (l :*: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (l :+: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (K1 R (NonRec (Target (W F) a))) = c (Wear f a)
  GConstraintsOf c f (K1 R (NonRec (Target F a))) = (c (f a), Wear f a ~ f a)
  GConstraintsOf c f (K1 R (NonRec (b (Target F)))) = ConstraintsOf c f b
  GConstraintsOf c f (K1 R (RecUsage (b (Target F)))) = () -- break recursion
  GConstraintsOf c f (K1 _i _c) = ()


-- | Default implementation of 'adjProof' based on 'Generic'.
-- gadjProofDefault
--   :: forall b c f
--   . ( CanDeriveGenericInstance b
--     , ConstraintsOfMatchesGenericDeriv c f b
--     , ConstraintsOf c f b
--     )
--   => b f -> b (Product (DictOf c f) f)
-- gadjProofDefault b
--   = unsafeUntargetBarbie @PxF $ to $
--       gadjProof pcbf $ fromWithRecAnn (unsafeTargetBarbie @F b)
--   where
--     pcbf = Proxy :: Proxy (c (b f))


-- class GAdjProof b rep where
--   gadjProof
--     :: ( GConstraintsOf c f rep
--        , GConstraintsOf c f (RecRep (b (Target F))) -- for the recursive case!
--        )
--     => Proxy (c (b f)) -> rep x -> Repl' (Target F) (Target PxF) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

-- instance GAdjProof b x => GAdjProof b (M1 _i _c x) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (M1 x)
--     = M1 (gadjProof pcbf x)

-- instance GAdjProof b V1 where
--   gadjProof _ _ = undefined

-- instance GAdjProof b U1 where
--   {-# INLINE gadjProof #-}
--   gadjProof _ u1 = u1

-- instance (GAdjProof b l, GAdjProof b r) => GAdjProof b (l :*: r) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (l :*: r)
--     = (gadjProof pcbf l) :*: (gadjProof pcbf r)

-- instance (GAdjProof b l, GAdjProof b r) => GAdjProof b (l :+: r) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf = \case
--     L1 l -> L1 (gadjProof pcbf l)
--     R1 r -> R1 (gadjProof pcbf r)


-- -- --------------------------------
-- -- The interesting cases
-- -- --------------------------------

-- instance {-# OVERLAPPING #-} GAdjProof b (K1 R (NonRec (Target (W F) a))) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (K1 (NonRec fa))
--     = K1 $ unsafeTarget @(W PxF) (Pair (mkProof pcbf) $ unsafeUntarget @(W F) fa)
--     where
--       mkProof :: c (Wear f a) => Proxy (c (b f)) -> DictOf c f a
--       mkProof _ = PackedDict


-- instance {-# OVERLAPPING #-} GAdjProof b (K1 R (NonRec (Target F a))) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (K1 (NonRec fa))
--     = K1 $ unsafeTarget @PxF (Pair (mkProof pcbf) $ unsafeUntarget @F fa)
--     where
--       mkProof :: c (Wear f a) => Proxy (c (b f)) -> DictOf c f a
--       mkProof _ = PackedDict

-- instance {-# OVERLAPPING #-} CanDeriveGenericInstance b => GAdjProof b (K1 R (RecUsage (b (Target F)))) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (K1 (RecUsage bf))
--     = K1 $ to $ gadjProof pcbf $ fromWithRecAnn bf

-- instance {-# OVERLAPPING #-} ConstraintsB b' => GAdjProof b (K1 R (NonRec (b' (Target F)))) where
--   {-# INLINE gadjProof #-}
--   gadjProof pcbf (K1 (NonRec bf))
--     = K1 $ unsafeTargetBarbie @PxF $ adjProof' pcbf $ unsafeUntargetBarbie @F bf
--     where
--       adjProof'
--         :: ConstraintsOf c f b'
--         => Proxy (c (b f)) -> b' f -> b' (Product (DictOf c f) f)
--       adjProof' _ = adjProof

-- instance (K1 i a) ~ Repl' (Target F) (Target PxF) (K1 i (NonRec a)) => GAdjProof b (K1 i (NonRec a)) where
--   {-# INLINE gadjProof #-}
--   gadjProof _ (K1 (NonRec a)) = K1 a
