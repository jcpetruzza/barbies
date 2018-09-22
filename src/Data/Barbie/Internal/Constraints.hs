{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( ConstraintsB(..)
  , ConstraintsOf

  , CanDeriveConstraintsB
  , GAllBC
  , GAllB
  , GAdjProof
  , gadjProofDefault
  )

where

import Data.Barbie.Internal.Dicts(ClassF, Dict(..))
import Data.Barbie.Internal.Functor(FunctorB(..))

import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

import Data.Generics.GenericN (GenericN(..), toN, fromN, Rec(..), Param)
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
  --   @x@ occurring under an @f@ in @b f@. E.g.:
  --
  -- @
  -- 'AllB' 'Show' Barbie = ('Show' 'String', 'Show' 'Int')
  -- @
  --
  -- For requiring constraints of the form @c (f a)@, see 'ConstraintsOf'.
  type AllB (c :: * -> Constraint) b :: Constraint
  type AllB c b = GAllB c b X (RepN (b X))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: forall c f.  AllB c b => b f -> b (Product (Dict c) f)

  default adjProof
    :: forall c f
    .  ( CanDeriveConstraintsB c b f
       , AllB c b
       )
    => b f -> b (Product (Dict c) f)
  adjProof = gadjProofDefault

-- | Similar to 'AllB' but will put the functor argument @f@
--   between the constraint @c@ and the type @a@. For example:
--
--   @
--   'ConstraintsOf' 'Show' f Barbie
--      = ( 'Show' (f 'String')
--        , 'Show' (f 'Int')
--        , 'Wear' f 'String' ~ f 'String'
--        , 'Wear' f 'Int' ~ f 'Int'
--        )
--   @
type ConstraintsOf c f b = AllB (ClassF c f) b

data X a

-- | Intuivively, the requirements to have @'ConstraintsB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'ConstraintsB' instance.
type CanDeriveConstraintsB c b f
  = ( GenericN (b f)
    , GenericN (b (Dict c `Product` f))
    , AllB c b ~ GAllB c b f (RepN (b f))
    , GAdjProof c b f (RepN (b f)) (RepN (b (Dict c `Product` f)))
    )


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  . ( CanDeriveConstraintsB c b f
    , AllB c b
    )
  => b f -> b (Dict c `Product` f)
gadjProofDefault
  = toN . gadjProof @c @b @f . fromN
{-# INLINE gadjProofDefault #-}


class GAllBC (b :: (* -> *) -> *) (repbf :: * -> *) where
  type GAllB (c :: * -> Constraint) b (f :: * -> *) repbf :: Constraint

class GAllBC b repbf => GAdjProof c b f repbf repbdf where
  gadjProof
    :: ( GAllB c b f repbf
       , GAllB c b f (RepN (b f)) -- for the recursive case
       )
    => repbf x -> repbdf x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAllBC b repbf => GAllBC b (M1 i k repbf) where
  type GAllB c b f (M1 i k repbf) = GAllB c b f repbf

instance GAdjProof c b f repbf repbdf => GAdjProof c b f (M1 i k repbf) (M1 i k repbdf) where
  gadjProof = M1 . gadjProof @c @b @f . unM1
  {-# INLINE gadjProof #-}



instance GAllBC b V1 where
  type GAllB c b f V1 = ()

instance GAdjProof c b f V1 V1 where
  gadjProof _ = undefined



instance GAllBC b U1 where
  type GAllB c b f U1 = ()

instance GAdjProof c b f U1 U1 where
  gadjProof = id
  {-# INLINE gadjProof #-}


instance (GAllBC b l, GAllBC b r) => GAllBC b (l :*: r) where
  type GAllB c b f (l :*: r) = (GAllB c b f l, GAllB c b f r)

instance (GAdjProof c b f l l', GAdjProof c b f r r') => GAdjProof c b f (l :*: r) (l' :*: r') where
  gadjProof (l :*: r)
    = (gadjProof @c @b @f l) :*: (gadjProof @c @b @f r)
  {-# INLINE gadjProof #-}


instance (GAllBC b l, GAllBC b r) => GAllBC b (l :+: r) where
  type GAllB c b f (l :+: r) = (GAllB c b f l, GAllB c b f r)

instance (GAdjProof c b f l l', GAdjProof c b f r r') => GAdjProof c b f (l :+: r) (l' :+: r') where
  gadjProof = \case
    L1 l -> L1 (gadjProof @c @b @f l)
    R1 r -> R1 (gadjProof @c @b @f r)
  {-# INLINE gadjProof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance GAllBC b (Rec p a) where
  type GAllB c b f (Rec p a) = GAllB_Rec c b f a

type family GAllB_Rec c b f a :: Constraint where
  GAllB_Rec c b f (f a)  = c a
  GAllB_Rec c b f (b  f) = () -- break recursion
  GAllB_Rec c b f (b' f) = AllB c b'
  GAllB_Rec c b f a      = ()

type P = Param 0

instance GAdjProof c b f (Rec (P f a) (f a))
                         (Rec (P (Dict c `Product` f) a) ((Dict c `Product` f) a)) where
  gadjProof
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gadjProof #-}


instance
  ( ConstraintsB b'
  , AllB c b'
  ) => GAdjProof c b f (Rec (b' (P f)) (b' f))
                       (Rec (b' (P (Dict c `Product` f))) (b' (Dict c `Product` f))) where
  gadjProof
    = Rec . K1 . adjProof . unK1 . unRec
  {-# INLINE gadjProof #-}


instance a ~ a' => GAdjProof c b f (Rec a a) (Rec a' a') where
  gadjProof = id
  {-# INLINE gadjProof #-}
