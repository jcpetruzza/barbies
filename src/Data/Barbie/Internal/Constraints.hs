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
  , ConstraintsOf

  , CanDeriveGenericInstance
  , AllBMatchesGenericDeriv
  , GAllB
  , GAdjProof
  , gadjProofDefault

  , ConstraintByType
  )

where

import Data.Barbie.Internal.Classification (BarbieType(..), ClassifyBarbie, GClassifyBarbie)
import Data.Barbie.Internal.Dicts(ClassF, Dict(..))
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Tags(F, PxF)
import Data.Barbie.Internal.Wear(Wear)

import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

import Data.Proxy

import GHC.Generics


-- | Instances of this class provide means to talk about constraints,
--   both at compile-time, using 'AllB' and at run-time,
--   in the form of class instance dictionaries, via 'adjProof'.
--
--   A manual definition would look like this:
--
-- @
-- data T f = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
--
-- instance 'ConstraintsB' T where
--   type 'AllB' c T
--     = (c 'Int', c 'String', c 'Bool')
--
--   type 'NotBare' T f
--     = ()
--
--   adjProof t = case t of
--     A x y -> A ('Pair' ('packDict' x) ('packDict' y))
--     B z w -> B ('Pair' ('packDict' z) ('packDict' w))
-- @
--
-- There is a default implementation of 'AllB' for
-- 'Generic' types, so in practice one will simply do:
--
-- @
-- derive instance 'Generic' T
-- instance 'ConstraintsB' T
-- @
class FunctorB b => ConstraintsB b where
  -- | @'AllB' c b@ should contain a constraint @c x@ for each
  --   @f x@ occurring in @b@. E.g.:
  --
  -- @
  -- 'AllB' 'Show' Barbie = ('Show' 'String', 'Show' 'Int')
  -- @
  --
  -- For requiring constraints of the form @c (f a)@, see 'ConstraintsOf'.
  type AllB (c :: * -> Constraint) b :: Constraint
  type AllB c b = GAllB c (RecRep (b (Target F)))

  -- | When using 'Wear' in the definition of a Barbie-type, one will sometimes
  --   need to express that the @f@ in @b f@ is not 'Data.Barbie.Internal.Wear.Bare'.
  --   We use @'NotBare' b f@ for this. For example:
  --
  --   @
  --   'NotBare' SignUpForm f = ('Wear' f 'String' ~ f 'String', 'Wear' f 'Bool' ~ 'Bool')
  --   'NotBare' Barbie f = ()
  --   @
  type NotBare b (f :: * -> *) :: Constraint
  type NotBare b f = GNotBare f (RecRep (b (Target F)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: forall c f.  AllB c b => b f -> b (Product (Dict c) f)

  default adjProof
    :: forall c f
    .  ( CanDeriveGenericInstance b
       , AllBMatchesGenericDeriv c b
       , AllB c b
       )
    => b f -> b (Product (Dict c) f)
  adjProof = gadjProofDefault

-- | Similar to 'AllB' but will put the functor argument @f@
--   between the constraint @c@ and the type @a@. For example:
--
--   @
--   'ConstraintsOf' 'Show' f Barbie = ('Show' (f 'String'), c (f 'Int'), 'NotBare' Barbie f)
--   @
type ConstraintsOf c f b = (AllB (ClassF c f) b, NotBare b f)


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

type AllBMatchesGenericDeriv c b
  = ( AllB c b ~ GAllB c (RecRep (b (Target F)))
    , AllB c b ~ ConstraintByType (ClassifyBarbie b) c (RecRep (b (Target F)))
    )


-- ===============================================================
--  Generic derivations
-- ===============================================================

type family ConstraintByType bt (c :: * -> Constraint) r :: Constraint where
  ConstraintByType bt c (M1 _i _c x) = ConstraintByType bt c x
  ConstraintByType bt c V1 = ()
  ConstraintByType bt c U1 = ()
  ConstraintByType bt c (l :*: r) = (ConstraintByType bt c l, ConstraintByType bt c r)
  ConstraintByType bt c (l :+: r) = (ConstraintByType bt c l, ConstraintByType bt c r)
  ConstraintByType 'WearBarbie c (K1 R (NonRec (Target (W F) a))) = c a
  ConstraintByType 'NonWearBarbie c (K1 R (NonRec (Target F a))) = c a
  ConstraintByType bt c (K1 R (NonRec (b (Target F)))) = AllB c b
  ConstraintByType bt c (K1 R (RecUsage (b (Target F)))) = () -- break recursion
  ConstraintByType bt c (K1 _i _c) = ()

type GAllB c r
  = ConstraintByType (GClassifyBarbie r) c r

type GNotBare f r
  = NotBareByType (GClassifyBarbie r) f r

type family NotBareByType bt (f :: * -> *) r :: Constraint where
  NotBareByType 'NonWearBarbie _ _ = ()
  NotBareByType wbt f (M1 _i _c x) = NotBareByType wbt f x
  NotBareByType wbt f V1 = ()
  NotBareByType wbt f U1 = ()
  NotBareByType wbt f (l :*: r) = (NotBareByType wbt f l, NotBareByType wbt f r)
  NotBareByType wbt f (l :+: r) = (NotBareByType wbt f l, NotBareByType wbt f r)
  NotBareByType wbt f (K1 R (NonRec (Target (W F) a))) = Wear f a ~ f a
  NotBareByType wbt f (K1 R (NonRec (b (Target F)))) = NotBare b f
  NotBareByType wbt f (K1 R (RecUsage (b (Target F)))) = () -- break recursion
  NotBareByType wbt f (K1 _i _c) = ()


-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  . ( CanDeriveGenericInstance b
    , AllBMatchesGenericDeriv c b
    , AllB c b
    )
  => b f -> b (Product (Dict c) f)
gadjProofDefault b
  = unsafeUntargetBarbie @PxF $ to $
      gadjProof pcbf pbt $ fromWithRecAnn (unsafeTargetBarbie @F b)
  where
    pcbf = Proxy :: Proxy (c (b f))
    pbt  = Proxy :: Proxy (ClassifyBarbie b)


class GAdjProof (bt :: BarbieType) b rep where

  gadjProof
    :: ( ConstraintByType bt c rep
       , GAllB c (RecRep (b (Target F))) -- for the recursive case
       )
    => Proxy (c (b f))
    -> Proxy bt
    -> rep x
    -> Repl' (Target F) (Target PxF) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAdjProof bt b x => GAdjProof bt b (M1 _i _c x) where
  gadjProof pcbf pbt (M1 x)
    = M1 (gadjProof pcbf pbt x)
  {-# INLINE gadjProof #-}

instance GAdjProof bt b V1 where
  gadjProof _ _ _ = undefined

instance GAdjProof bt b U1 where
  gadjProof _ _ u1 = u1
  {-# INLINE gadjProof #-}

instance (GAdjProof bt b l, GAdjProof bt b r) => GAdjProof bt b (l :*: r) where
  gadjProof pcbf pbt (l :*: r)
    = (gadjProof pcbf pbt l) :*: (gadjProof pcbf pbt r)
  {-# INLINE gadjProof #-}

instance (GAdjProof bt b l, GAdjProof bt b r) => GAdjProof bt b (l :+: r) where
  gadjProof pcbf pbt = \case
    L1 l -> L1 (gadjProof pcbf pbt l)
    R1 r -> R1 (gadjProof pcbf pbt r)
  {-# INLINE gadjProof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GAdjProof 'WearBarbie b (K1 R (NonRec (Target (W F) a))) where
  gadjProof pcbf _ (K1 (NonRec fa))
    = K1 $ unsafeTarget @(W PxF) (Pair (mkProof pcbf) $ unsafeUntarget @(W F) fa)
    where
      mkProof :: c a => Proxy (c (b f)) -> Dict c a
      mkProof _ = Dict
  {-# INLINE gadjProof #-}


instance {-# OVERLAPPING #-} GAdjProof 'NonWearBarbie b (K1 R (NonRec (Target F a))) where
  gadjProof pcbf _ (K1 (NonRec fa))
    = K1 $ unsafeTarget @PxF (Pair (mkProof pcbf) $ unsafeUntarget @F fa)
    where
      mkProof :: c a => Proxy (c (b f)) -> Dict c a
      mkProof _ = Dict
  {-# INLINE gadjProof #-}


instance {-# OVERLAPPING #-}
  ( CanDeriveGenericInstance b
  , bt ~ ClassifyBarbie b
  )
    => GAdjProof bt b (K1 R (RecUsage (b (Target F)))) where
  gadjProof pcbf pbt (K1 (RecUsage bf))
    = K1 $ to $ gadjProof pcbf pbt $ fromWithRecAnn bf
  {-# INLINE gadjProof #-}


instance {-# OVERLAPPING #-}
  ConstraintsB b'
    => GAdjProof bt b (K1 R (NonRec (b' (Target F)))) where
  gadjProof pcbf _ (K1 (NonRec bf))
    = K1 $ unsafeTargetBarbie @PxF $ adjProof' pcbf $ unsafeUntargetBarbie @F bf
    where
      adjProof'
        :: AllB c b' => Proxy (c (b f)) -> b' f -> b' (Product (Dict c) f)
      adjProof' _ = adjProof
  {-# INLINE gadjProof #-}

instance
  (K1 i a) ~ Repl' (Target F) (Target PxF) (K1 i (NonRec a))
    => GAdjProof bt b (K1 i (NonRec a)) where
  gadjProof _ _ (K1 (NonRec a)) = K1 a
  {-# INLINE gadjProof #-}
