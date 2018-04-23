{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Internal.Constraints
  ( ProofOf(..)
  , proof
  , requiringProof

  , ConstraintsB(..)
  , ProofB(..)

  , GConstraintsOf
  , GAdjProof
  , gadjProofDefault
  , GProof
  , gbproofDefault
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Product(ProductB(..))
import Data.Barbie.Internal.Generics

import Data.Constraint(Dict(..), withDict)
import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)
import Data.Proxy

import GHC.Generics


-- | @'ProofOf' c f a@ is evidence that there exists an instance
--   of @c (f a)@.
newtype ProofOf c f a
  = Proof { proofDict :: Dict (c (f a)) }

-- | Build proof of instance.
proof :: c (f a) => ProofOf c f a
proof
  = Proof Dict

-- | Turn a constrained-function into an unconstrained one
--   that demands proof-of-instance instead.
requiringProof :: (c (f a) => r) -> (ProofOf c f a -> r)
requiringProof f
  = \p -> withDict (proofDict p) f


-- | Example definition:
  --
  -- @
  -- data T f = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
  --
  -- instance 'ConstraintsB' T where
  --   type 'ConstraintsOf' c f T = (c (f 'Int'), c (f 'String'), c (f 'Bool'))
  --
  --   adjProof t = case t of
  --     A x y -> A ('Pair' 'proof' x) ('Pair' 'proof' y)
  --     B x y -> B ('Pair' 'proof' x) ('Pair' 'proof' y)
  -- @
  --
  -- There are default implementation of 'ConstraintsOf' and 'adjProof' for
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
  type ConstraintsOf c f b = GConstraintsOf c f b (Rep (b (Target F)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: ConstraintsOf c f b => b f -> b (Product (ProofOf c f) f)
  adjProof = adjProofDefault

  adjProofDefault :: ConstraintsOf c f b => b f -> b (Product (ProofOf c f) f)
  default adjProofDefault
    :: ( Generic (b (Target F))
       , Generic (b (Target PxF))
       , GAdjProof (Rep (b (Target F)))
       , ConstraintsOf c f b
       , Rep (b (Target PxF)) ~ Repl (Target F) (Target PxF) (Rep (b (Target F)))
       , ConstraintsOf c f b ~ GConstraintsOf c f b (Rep (b (Target F)))
       )
    => b f
    -> b (Product (ProofOf c f) f)
  adjProofDefault = gadjProofDefault


-- | Barbie-types with products have a canonical proof of instance,
--   which can make them more convenient to use:
--
--  @
--  'adjProof' = 'bprod' 'bproof'
--  @
--
-- There is a default 'bproof' implementation for 'Generic' types, so
-- instances can derived automatically.
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: ConstraintsOf c f b => b (ProofOf c f)
  bproof = bproofDefault


  bproofDefault :: ConstraintsOf c f b => b (ProofOf c f)
  default bproofDefault
    :: ( Generic (b (Target P))
       , GProof (Rep (b (Target F)))
       , ConstraintsOf c f b
       , Rep (b (Target P)) ~ Repl (Target F) (Target P) (Rep (b (Target F)))
       , ConstraintsOf c f b ~ GConstraintsOf c f b (Rep (b (Target F)))
       )
    => b (ProofOf c f)
  bproofDefault = gbproofDefault


-- ===============================================================
--  Generic derivations
-- ===============================================================

type family GConstraintsOf (c :: * -> Constraint) (f :: * -> *) (b :: (* -> *) -> *) r :: Constraint where
  GConstraintsOf c f b (M1 _i _c x) = GConstraintsOf c f b x
  GConstraintsOf c f b V1 = ()
  GConstraintsOf c f b U1 = ()
  GConstraintsOf c f b (l :*: r) = (GConstraintsOf c f b l, GConstraintsOf c f b r)
  GConstraintsOf c f b (l :+: r) = (GConstraintsOf c f b l, GConstraintsOf c f b r)
  GConstraintsOf c f _ (K1 R (Target F a)) = c (f a)
  GConstraintsOf c f b (K1 R (b (Target F))) = () -- break recursion
  GConstraintsOf c f _ (K1 R (b (Target F))) = ConstraintsOf   c f b
  GConstraintsOf c f _ (K1 _i _c) = ()


-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  .  ( Generic (b (Target F))
     , Generic (b (Target PxF))
     , GAdjProof (Rep (b (Target F)))
     , GConstraintsOf c f b (Rep (b (Target F)))
     , Rep (b (Target PxF)) ~ Repl (Target F) (Target PxF) (Rep (b (Target F)))
     )
  => b f
  -> b (Product (ProofOf c f) f)
gadjProofDefault b
  = unsafeUntargetBarbie @PxF $ to $ gadjProof pcbf $ from (unsafeTargetBarbie @F b)
  where
    pcbf = Proxy :: Proxy (c (b f))


-- | Default implementation of 'proof' based on 'Generic'.
gbproofDefault
  :: forall b c f
  .  ( Generic (b (Target P))
     , GProof (Rep (b (Target F)))
     , GConstraintsOf c f b (Rep (b (Target F)))
     , Rep (b (Target P)) ~ Repl (Target F) (Target P) (Rep (b (Target F)))
     )
  => b (ProofOf c f)
gbproofDefault
  = unsafeUntargetBarbie @P $ to $ gbproof pcbf pb
  where
    pcbf = Proxy :: Proxy (c (b f))
    pb = Proxy :: Proxy (Rep (b (Target F)) x)

data F a
data P a
data PxF a

class GAdjProof rep where
  gadjProof
    :: GConstraintsOf c f b rep
    => Proxy (c (b f)) -> rep x -> Repl (Target F) (Target PxF) rep x

class GProof rep where
  gbproof
    :: GConstraintsOf c f b rep
    => Proxy (c (b f)) -> Proxy (rep x) -> Repl (Target F) (Target P) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAdjProof x => GAdjProof (M1 _i _c x) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (M1 x)
    = M1 (gadjProof pcbf x)

instance GAdjProof V1 where
  gadjProof _ _ = undefined

instance GAdjProof U1 where
  {-# INLINE gadjProof #-}
  gadjProof _ u1 = u1

instance (GAdjProof l, GAdjProof r) => GAdjProof (l :*: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (l :*: r)
    = (gadjProof pcbf l) :*: (gadjProof pcbf r)

instance (GAdjProof l, GAdjProof r) => GAdjProof (l :+: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf = \case
    L1 l -> L1 (gadjProof pcbf l)
    R1 r -> R1 (gadjProof pcbf r)



instance GProof x => GProof (M1 _i _c x) where
  {-# INLINE gbproof #-}
  gbproof pcbf pm1
    = M1 (gbproof pcbf (unM1 <$> pm1))

instance GProof U1 where
  {-# INLINE gbproof #-}
  gbproof _ _ = U1

instance (GProof l, GProof r) => GProof (l :*: r) where
  {-# INLINE gbproof #-}
  gbproof pcbf pp
    = gbproof pcbf (left <$> pp) :*: gbproof pcbf (right <$> pp)
    where
      left  (l :*: _) = l
      right (_ :*: r) = r


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GAdjProof (K1 R (Target F a)) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (K1 fa)
    = K1 $ unsafeTarget @PxF (Pair (mkProof pcbf) $ unsafeUntarget @F fa)
    where
      mkProof :: c (f a) => Proxy (c (b f)) -> ProofOf c f a
      mkProof _ = proof

instance {-# OVERLAPPING #-} (ConstraintsB b, ConstraintsOf c f b) => GAdjProof (K1 R (b (Target f))) where
  {-# INLINE gadjProof #-}
  gadjProof p (K1 bf)
    = K1 $ unsafeTargetBarbie @PxF (adjProof' (mkP p) $ unsafeUntargetBarbie @F bf)
    where
      mkP :: Proxy (c (b' f)) -> Proxy (c (b f))
      mkP _ = Proxy


instance (K1 _i _c) ~ Repl (Target F) (Target PxF) (K1 _i _c) => GAdjProof (K1 _i _c) where
  {-# INLINE gadjProof #-}
  gadjProof _ k1 = k1


adjProof'
  :: (ConstraintsB b, ConstraintsOf c f b)
  => Proxy (c (b f)) -> b f -> b (Product (ProofOf c f) f)
adjProof' _  = adjProof

instance {-# OVERLAPPING #-} GProof (K1 R (Target F a)) where
  {-# INLINE gbproof #-}
  gbproof pcbf _
    = K1 $ unsafeTarget @P (mkProof pcbf)
    where
      mkProof :: c (f a) => Proxy (c (b f)) -> ProofOf c f a
      mkProof _ = proof
