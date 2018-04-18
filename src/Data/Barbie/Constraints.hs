-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie
--
-- Support for operating on Barbie-types with constrained functions.
--
-- Consider the following function:
--
-- @
-- showIt :: 'Show' a => 'Maybe' a -> 'Data.Functor.Const' 'String' a
-- showIt = 'Data.Functor.Const' . 'show'
-- @
--
-- We would then like to be able to do:
--
-- @
-- 'bmap' 'showIt' :: 'FunctorB' b => b 'Maybe' -> b ('Data.Functor.Const' 'String')
-- @
--
-- This however doesn't work because of the @'Show' a@ constraint in the
-- the type of @showIt@.
--
-- This module adds support to overcome this problem.
----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TypeFamilies    #-}
module Data.Barbie.Constraints
  ( -- * Proof of instance
    ProofOf(..)
  , proof
  , requiringProof

    -- * Retrieving proofs
  , ConstraintsB(..)
  , ProofB(..)
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Product(ProductB(..))

import Data.Constraint(Dict(..), withDict)
import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

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

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: ConstraintsOf c f b => b f -> b (Product (ProofOf c f) f)


-- | Barbie-types with products have a canonical proof of instance,
--   which can make them more convenient to use:
--
--  @
--  'adjProof' = 'bprod' 'bproof'
--  @
--
class (ConstraintsB b, ProductB b) => ProofB b where
  bproof :: ConstraintsOf c f b => b (ProofOf c f)
