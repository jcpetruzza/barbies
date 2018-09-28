{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( ConstraintsB(..)
  , AllBF
  , ConstraintsOf

  , CanDeriveConstraintsB
  , GAllBC(..)
  , GAllBRep, X
  , TagSelf, Self, Other
  , GConstraintsB(..)
  , gadjProofDefault
  )

where

import Data.Barbie.Internal.Dicts(ClassF, Dict(..))
import Data.Barbie.Internal.Functor(FunctorB(..))

import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)

import Data.Generics.GenericN


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
  -- For requiring constraints of the form @c (f a)@, see 'AllBF'.
  type AllB (c :: * -> Constraint) b :: Constraint
  type AllB c b = GAllB c (GAllBRep b)

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
--   'AllBF' 'Show' f Barbie
--      = ( 'Show' (f 'String')
--        , 'Show' (f 'Int')
--        , 'Wear' f 'String' ~ f 'String'
--        , 'Wear' f 'Int' ~ f 'Int'
--        )
--   @
type AllBF c f b = AllB (ClassF c f) b


{-# DEPRECATED ConstraintsOf "Use AllBF" #-}
type ConstraintsOf c f b = AllBF c f b

-- | The representation used for the generic computation of the @'AllB' c b@
--   constraints. Here 'X' is an arbitrary constant since the actual
--   argument to @b@ is irrelevant.
type GAllBRep b = TagSelf b (RepN (b X))
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
    , AllB c b ~ GAllB c (GAllBRep b)
    , GConstraintsB c f (GAllBRep b) (RepN (b f)) (RepN (b (Dict c `Product` f)))
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
  = toN . gadjProof @c @f @(GAllBRep b) . fromN
{-# INLINE gadjProofDefault #-}

class GAllBC (repbf :: * -> *) where
  type GAllB (c :: * -> Constraint) repbf :: Constraint

class GAllBC repbx => GConstraintsB c (f :: * -> *) repbx repbf repbdf where
  gadjProof
    :: GAllB c repbx => repbf x -> repbdf x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAllBC repbf => GAllBC (M1 i k repbf) where
  type GAllB c (M1 i k repbf) = GAllB c repbf

instance
  GConstraintsB c f repbx repbf repbdf
    => GConstraintsB c f (M1 i k repbx)
                         (M1 i k repbf)
                         (M1 i k repbdf) where
  gadjProof = M1 . gadjProof @c @f @repbx . unM1
  {-# INLINE gadjProof #-}



instance GAllBC V1 where
  type GAllB c V1 = ()

instance GConstraintsB c f V1 V1 V1 where
  gadjProof _ = undefined



instance GAllBC U1 where
  type GAllB c U1 = ()

instance GConstraintsB c f U1 U1 U1 where
  gadjProof = id
  {-# INLINE gadjProof #-}


instance (GAllBC l, GAllBC r) => GAllBC (l :*: r) where
  type GAllB c (l :*: r) = (GAllB c l, GAllB c r)

instance
  ( GConstraintsB c f lx lf ldf
  , GConstraintsB c f rx rf rdf
  ) => GConstraintsB c f (lx  :*: rx)
                         (lf  :*: rf)
                         (ldf :*: rdf) where
  gadjProof (l :*: r)
    = (gadjProof @c @f @lx l) :*: (gadjProof @c @f @rx r)
  {-# INLINE gadjProof #-}


instance (GAllBC l, GAllBC r) => GAllBC (l :+: r) where
  type GAllB c (l :+: r) = (GAllB c l, GAllB c r)

instance
  ( GConstraintsB c f lx lf ldf
  , GConstraintsB c f rx rf rdf
  ) => GConstraintsB c f (lx  :+: rx)
                         (lf  :+: rf)
                         (ldf :+: rdf) where
  gadjProof = \case
    L1 l -> L1 (gadjProof @c @f @lx l)
    R1 r -> R1 (gadjProof @c @f @rx r)
  {-# INLINE gadjProof #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0


instance GAllBC (Rec (P0 X a) (X a)) where
  type GAllB c (Rec (P0 X a) (X a)) = c a

instance GConstraintsB c f (Rec (P0 X a) (X a))
                           (Rec (P0 f a) (f a))
                           (Rec (P0 (Dict c `Product` f) a)
                                   ((Dict c `Product` f) a)) where
  gadjProof
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gadjProof #-}



instance GAllBC (Rec (Self b (P0 X)) (b X)) where
   type GAllB c (Rec (Self b (P0 X)) (b X)) = ()

instance
  ( ConstraintsB b
  , AllB c b
  ) => GConstraintsB c f (Rec (Self b (P0 X)) (b X))
                         (Rec (b (P0 f)) (b f))
                         (Rec (b (P0 (Dict c `Product` f)))
                              (b     (Dict c `Product` f))) where
  gadjProof
    = Rec . K1 . adjProof . unK1 . unRec
  {-# INLINE gadjProof #-}

instance
  ( ConstraintsB b'
  , SameOrParam b b'
  ) => GAllBC (Rec (Other b (P0 X)) (b' X)) where
  type GAllB c (Rec (Other b (P0 X)) (b' X)) = AllB c b'

instance
  ( SameOrParam b b'
  , ConstraintsB b'
  , AllB c b'
  ) => GConstraintsB c f (Rec (Other b (P0 X)) (b' X))
                         (Rec (b (P0 f)) (b' f))
                         (Rec (b (P0 (Dict c `Product` f)))
                              (b'    (Dict c `Product` f))) where
  gadjProof
    = Rec . K1 . adjProof . unK1 . unRec
  {-# INLINE gadjProof #-}



instance GAllBC (Rec a a) where
  type GAllB c (Rec a a) = ()

instance GConstraintsB c f (Rec a a)
                           (Rec a a)
                           (Rec a a) where
  gadjProof = id
  {-# INLINE gadjProof #-}


-- ============================================================================
-- ## Identifying recursive usages of the barbie-type ##
--
-- We use type-families to generically compute @'AllB' c b@. Intuitively, if
-- @b' f@ occurs inside @b f@, then we should just add @AllB b' c@ to
-- @AllB b c@. The problem is that if @b@ is a recursive type, and @b'@ is @b@,
-- then ghc will choke and blow the stack (instead of computing a fixpoint).
--
-- So, we would like to behave differently when @b = b'@ and add @()@ instead
-- of `AllB b f` to break the recursion. Our trick will be to use a type
-- family to inspect @RepN (b f)@ and distinguish recursive usages from
-- non-recursive ones, tagging them with different types, so we can distinguish
-- them in the instances.
-- ============================================================================

data Self  (b :: (* -> *) -> *) (f :: * -> *)
data Other (b :: (* -> *) -> *) (f :: * -> *)

type family TagSelf (b :: (* -> *) -> *) (repbf :: * -> *) :: * -> * where
  TagSelf b (M1 mt m s)
    = M1 mt m (TagSelf b s)

  TagSelf b (l :+: r)
    = TagSelf b l :+: TagSelf b r

  TagSelf b (l :*: r)
    = TagSelf b l :*: TagSelf b r

  TagSelf b (Rec (b f) (b g))
    = Rec (Self b f) (b g)

  TagSelf b (Rec (b' f) (b'' (g :: * -> *)))
    = Rec (Other b' f) (b'' g)

  TagSelf b (Rec p a)
    = Rec p a

  TagSelf b U1
    = U1

  TagSelf b V1
    = V1
