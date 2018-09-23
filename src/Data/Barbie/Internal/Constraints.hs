{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( ConstraintsB(..)
  , ConstraintsOf

  , CanDeriveGenericInstance
  , GAllBC
  , GAllB
  , GAdjProof
  , gadjProofDefault
  )

where

import Data.Barbie.Internal.Dicts(ClassF, Dict(..))
import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Tag (Tag(..), CoercibleTag(..))
import Data.Barbie.Internal.Deprecated.Wear

import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

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
  type AllB c b = GAllB c b (F X) (Rep (b (F X)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: forall c f.  AllB c b => b f -> b (Product (Dict c) f)

  default adjProof
    :: forall c f
    .  ( CanDeriveGenericInstance c b f
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
type ConstraintsOf c f b = (AllB (ClassF c f) b, AllB (NotBare f) b)

data X a

data F_
data P_

type F = Tag F_
type P = Tag P_

-- | Intuivively, the requirements to have @'ConstraintsB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'ConstraintsB' instance.
type CanDeriveGenericInstance c b f
  = ( Generic (b (F f))
    , Generic (b (P (Product (Dict c) f)))
    , CoercibleTag F b f
    , CoercibleTag P b (Product (Dict c) f)
    , AllB c b ~ GAllB c b (F f) (Rep (b (F f)))
    , GAdjProof c b f (Rep (b (F f))) (Rep (b (P (Product (Dict c) f))))
    )


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  . ( CanDeriveGenericInstance c b f
    , AllB c b
    )
  => b f -> b (Product (Dict c) f)
gadjProofDefault
  = coerceUntag @P
      . to
      . gadjProof @c @b @f
      . from
      . coerceTag @F


class GAllBC (b :: (* -> *) -> *) (repbf :: * -> *) where
  type GAllB (c :: * -> Constraint) b (f :: * -> *) repbf :: Constraint

class GAllBC b repbf => GAdjProof c b f repbf repbdf where
  gadjProof
    :: ( GAllB c b (F f) repbf
       , GAllB c b (F f) (Rep (b (F f))) -- for the recursive case
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

instance GAllBC b (Rec0 a) where
  type GAllB c b f (Rec0 a) = GAllB_Rec0 c b f a

type family GAllB_Rec0 c b f a :: Constraint where
  GAllB_Rec0 c b f (f a)  = c a
  GAllB_Rec0 c b f (b  f) = () -- break recursion
  GAllB_Rec0 c b f (b' f) = AllB c b'
  GAllB_Rec0 c b f a      = ()


instance GAdjProof c b f (Rec0 (F f a)) (Rec0 (P (Product (Dict c) f) a)) where
  gadjProof
    = K1 . Tag . Pair Dict . unTag . unK1
  {-# INLINE gadjProof #-}


instance
  ( Generic (b (F f))
  , Generic (b (P (Product (Dict c) f)))
  , GAdjProof c b f (Rep (b (F f)))
                    (Rep (b (P (Product (Dict c) f))))
  )
  => GAdjProof c b f (Rec0 (b (F f)))
                     (Rec0 (b (P (Product (Dict c) f)))) where
  gadjProof
    = K1 . to . gadjProof @c @b @f . from . unK1
  {-# INLINE gadjProof #-}


instance {-# OVERLAPPABLE #-}
  ( ConstraintsB b'
  , AllB c b'
  , CoercibleTag F b' f
  , CoercibleTag P b' (Product (Dict c) f)
  )
    => GAdjProof c b f (Rec0 (b' (F f)))
                       (Rec0 (b' (P (Product (Dict c) f)))) where
  gadjProof
    = K1 . coerceTag @P . adjProof . coerceUntag @F . unK1
  {-# INLINE gadjProof #-}


instance {-# OVERLAPPABLE #-} a ~ a' => GAdjProof c b f (Rec0 a) (Rec0 a') where
  gadjProof = id
  {-# INLINE gadjProof #-}
