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
  --     A x y -> A ('Pair' ('Proof' 'Dict') x) ('Pair' ('Proof' 'Dict') y)
  --     B x y -> B ('Pair' ('Proof' 'Dict') x) ('Pair' ('Proof' 'Dict') y)
  -- @
class FunctorB b => ConstraintsB b where
  -- | @'ConstraintsOf' c f b@ should contain a constraint @c (f x)@
  --  for each @f x@ occurring in @b@. E.g.:
  --
  type ConstraintsOf (c :: * -> Constraint) (f :: * -> *) b :: Constraint
  type ConstraintsOf c f b = GConstraintsOf c f (Rep (b (Target F)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: ConstraintsOf c f b => b f -> b (Product (ProofOf c f) f)

  default adjProof
    :: ( Generic (b (Target F))
       , Generic (b (Target PxF))
       , GAdjProof (Rep (b (Target F)))
       , ConstraintsOf c f b
       , Rep (b (Target PxF)) ~ Repl (Target F) (Target PxF) (Rep (b (Target F)))
       , ConstraintsOf c f b ~ GConstraintsOf c f (Rep (b (Target F)))
       )
    => b f
    -> b (Product (ProofOf c f) f)
  adjProof = gadjProofDefault


-- | Barbie-types with products have a canonical proof of instance,
--   which can make them more convenient to use:
--
--  @
--  'adjProof' = 'bprod' 'bproof'
--  @
--
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: ConstraintsOf c f b => b (ProofOf c f)


-- ===============================================================
--  Generic derivations
-- ===============================================================

type family GConstraintsOf (c :: * -> Constraint) (f :: * -> *) b  :: Constraint where
  GConstraintsOf c f (M1 _i _c x) = GConstraintsOf c f x
  GConstraintsOf c f V1 = ()
  GConstraintsOf c f U1 = ()
  GConstraintsOf c f (l :*: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (l :+: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (K1 R (Target F a)) = c (f a)
  GConstraintsOf c f (K1 _i _c) = ()

-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  .  ( Generic (b (Target F))
     , Generic (b (Target PxF))
     , GAdjProof (Rep (b (Target F)))
     , GConstraintsOf c f (Rep (b (Target F)))
     , Rep (b (Target PxF)) ~ Repl (Target F) (Target PxF) (Rep (b (Target F)))
     )
  => b f
  -> b (Product (ProofOf c f) f)
gadjProofDefault b
  = unsafeUntargetBarbie @PxF $ to $ gadjProof pc pf $ from (unsafeTargetBarbie @F b)
  where
    pc = Proxy :: Proxy c
    pf = Proxy :: Proxy f


data F a
data PxF a

class GAdjProof b where
  gadjProof
    :: GConstraintsOf c f b
    => Proxy c
    -> Proxy f
    -> b x
    -> Repl (Target F) (Target PxF) b x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAdjProof x => GAdjProof (M1 _i _c x) where
  {-# INLINE gadjProof #-}
  gadjProof pc pf (M1 x)
    = M1 (gadjProof pc pf x)

instance GAdjProof V1 where
  gadjProof _ _ _ = undefined

instance GAdjProof U1 where
  {-# INLINE gadjProof #-}
  gadjProof _ _ u1 = u1

instance (GAdjProof l, GAdjProof r) => GAdjProof (l :*: r) where
  {-# INLINE gadjProof #-}
  gadjProof pc pf (l :*: r)
    = (gadjProof pc pf l) :*: (gadjProof pc pf r)

instance (GAdjProof l, GAdjProof r) => GAdjProof (l :+: r) where
  {-# INLINE gadjProof #-}
  gadjProof pc pf = \case
    L1 l -> L1 (gadjProof pc pf l)
    R1 r -> R1 (gadjProof pc pf r)


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GAdjProof (K1 R (Target F a)) where
  {-# INLINE gadjProof #-}
  gadjProof pc pf (K1 fa)
    = K1 $ unsafeTarget @PxF (Pair (mkProof pc pf) $ unsafeUntarget @F fa)
    where
      mkProof :: c (f a) => Proxy c -> Proxy f -> ProofOf c f a
      mkProof _ _ = proof

instance (K1 _i _c) ~ Repl (Target F) (Target PxF) (K1 _i _c) => GAdjProof (K1 _i _c) where
  {-# INLINE gadjProof #-}
  gadjProof _ _ k1 = k1
