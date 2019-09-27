{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Internal.Constraints
  ( ConstraintsB(..)
  , bmapC
  , btraverseC
  , AllBF

  , CanDeriveConstraintsB
  , GAllBC(..)
  , GAllBRep, X
  , TagSelf, Self, Other
  , GConstraintsB(..)
  , gbaddDictsDefault
  )

where

import Data.Barbie.Internal.Dicts       (ClassF, Dict (..), requiringDict)
import Data.Barbie.Internal.Functor     (FunctorB (..))
import Data.Barbie.Internal.Traversable (TraversableB (..))

import Data.Functor.Compose (Compose (..))
import Data.Functor.Const   (Const (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum     (Sum (..))
import Data.Kind            (Constraint)
import Data.Proxy           (Proxy (..))

import Data.Generics.GenericN


-- | Instances of this class provide means to talk about constraints,
--   both at compile-time, using 'AllB', and at run-time, in the form
--   of 'Dict', via 'baddDicts'.
--
--   A manual definition would look like this:
--
-- @
-- data T f = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
--
-- instance 'ConstraintsB' T where
--   type 'AllB' c T = (c 'Int', c 'String', c 'Bool')
--
--   'baddDicts' t = case t of
--     A x y -> A ('Pair' 'Dict' x) ('Pair' 'Dict' y)
--     B z w -> B ('Pair' 'Dict' z) ('Pair' 'Dict' w)
-- @
--
-- Now if we given a @T f@, we need to use the 'Show' instance of
-- their fields, we can use:
--
-- @
-- 'baddDicts' :: AllB Show b => b f -> b ('Dict' 'Show' `Product` b)
-- @
--
-- There is a default implementation of 'ConstraintsB' for
-- 'Generic' types, so in practice one will simply do:
--
-- @
-- derive instance 'Generic' (T f)
-- instance 'ConstraintsB' T
-- @
class FunctorB b => ConstraintsB (b :: (k -> *) -> *) where
  -- | @'AllB' c b@ should contain a constraint @c a@ for each
  --   @a@ occurring under an @f@ in @b f@. E.g.:
  --
  -- @
  -- 'AllB' 'Show' Barbie ~ ('Show' 'String', 'Show' 'Int')
  -- @
  --
  -- For requiring constraints of the form @c (f a)@, use 'AllBF'.
  type AllB (c :: k -> Constraint) b :: Constraint
  type AllB c b = GAllB c (GAllBRep b)

  baddDicts :: forall c f.  AllB c b => b f -> b (Dict c `Product` f)

  default baddDicts
    :: forall c f
    .  ( CanDeriveConstraintsB c b f
       , AllB c b
       )
    => b f -> b (Dict c `Product` f)
  baddDicts = gbaddDictsDefault


-- | Like 'bmap' but a constraint is allowed to be required on
--   each element of @b@
--
-- E.g. If all fields of 'b' are 'Show'able then you
-- could store each shown value in it's slot using 'Const':
--
-- > showFields :: (AllB Show b, ConstraintsB b) => b Identity -> b (Const String)
-- > showFields = bmapC @Show showField
-- >   where
-- >     showField :: forall a. Show a => Identity a -> Const String a
-- >     showField (Identity a) = Const (show a)
bmapC :: forall c b f g.
      (AllB c b, ConstraintsB b)
      => (forall a. c a => f a -> g a)
      -> b f
      -> b g
bmapC f bf = bmap go (baddDicts bf)
  where
    go :: forall a. (Dict c `Product` f) a -> g a
    go (d `Pair` fa) = requiringDict (f fa) d

-- | Like 'btraverse' but with a constraint on the elements of @b@.
btraverseC
  :: forall c b f g h
  .  (TraversableB b, ConstraintsB b, AllB c b, Applicative g)
  => (forall a. c a => f a -> g (h a))
  -> b f
  -> g (b h)
btraverseC f b = btraverse (\(Pair (Dict :: Dict c a) x) -> f x) (baddDicts b)

-- | Similar to 'AllB' but will put the functor argument @f@
--   between the constraint @c@ and the type @a@. For example:
--
--   @
--   'AllB'  'Show'   Barbie ~ ('Show'    'String',  'Show'    'Int')
--   'AllBF' 'Show' f Barbie ~ ('Show' (f 'String'), 'Show' (f 'Int'))
--   @
type AllBF c f b = AllB (ClassF c f) b


-- | The representation used for the generic computation of the @'AllB' c b@
--   constraints. Here 'X' is an arbitrary constant since the actual
--   argument to @b@ is irrelevant.
type GAllBRep b = TagSelf b (RepN (b X))
data X a

-- | @'CanDeriveConstraintsB' B f g@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B f@ can contain fields of type @b f@ as long as there exists a
--       @'ConstraintsB' b@ instance. In particular, recursive usages of @B f@
--       are allowed.
type CanDeriveConstraintsB c b f
  = ( GenericN (b f)
    , GenericN (b (Dict c `Product` f))
    , AllB c b ~ GAllB c (GAllBRep b)
    , GConstraintsB c f (GAllBRep b) (RepN (b f)) (RepN (b (Dict c `Product` f)))
    )


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of 'baddDicts' based on 'Generic'.
gbaddDictsDefault
  :: forall b c f
  . ( CanDeriveConstraintsB c b f
    , AllB c b
    )
  => b f -> b (Dict c `Product` f)
gbaddDictsDefault
  = toN . gbaddDicts @c @f @(GAllBRep b) . fromN
{-# INLINE gbaddDictsDefault #-}

class GAllBC (repbf :: * -> *) where
  type GAllB (c :: k -> Constraint) repbf :: Constraint

class GAllBC repbx => GConstraintsB c f repbx repbf repbdf where
  gbaddDicts :: GAllB c repbx => repbf x -> repbdf x


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
  gbaddDicts = M1 . gbaddDicts @c @f @repbx . unM1
  {-# INLINE gbaddDicts #-}



instance GAllBC V1 where
  type GAllB c V1 = ()

instance GConstraintsB c f V1 V1 V1 where
  gbaddDicts _ = undefined



instance GAllBC U1 where
  type GAllB c U1 = ()

instance GConstraintsB c f U1 U1 U1 where
  gbaddDicts = id
  {-# INLINE gbaddDicts #-}


instance (GAllBC l, GAllBC r) => GAllBC (l :*: r) where
  type GAllB c (l :*: r) = (GAllB c l, GAllB c r)

instance
  ( GConstraintsB c f lx lf ldf
  , GConstraintsB c f rx rf rdf
  ) => GConstraintsB c f (lx  :*: rx)
                         (lf  :*: rf)
                         (ldf :*: rdf) where
  gbaddDicts (l :*: r)
    = (gbaddDicts @c @f @lx l) :*: (gbaddDicts @c @f @rx r)
  {-# INLINE gbaddDicts #-}


instance (GAllBC l, GAllBC r) => GAllBC (l :+: r) where
  type GAllB c (l :+: r) = (GAllB c l, GAllB c r)

instance
  ( GConstraintsB c f lx lf ldf
  , GConstraintsB c f rx rf rdf
  ) => GConstraintsB c f (lx  :+: rx)
                         (lf  :+: rf)
                         (ldf :+: rdf) where
  gbaddDicts = \case
    L1 l -> L1 (gbaddDicts @c @f @lx l)
    R1 r -> R1 (gbaddDicts @c @f @rx r)
  {-# INLINE gbaddDicts #-}


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
  gbaddDicts
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gbaddDicts #-}



instance GAllBC (Rec (Self b (P0 X)) (b X)) where
   type GAllB c (Rec (Self b (P0 X)) (b X)) = ()

instance
  ( ConstraintsB b
  , AllB c b
  ) => GConstraintsB c f (Rec (Self b (P0 X)) (b X))
                         (Rec (b (P0 f)) (b f))
                         (Rec (b (P0 (Dict c `Product` f)))
                              (b     (Dict c `Product` f))) where
  gbaddDicts
    = Rec . K1 . baddDicts . unK1 . unRec
  {-# INLINE gbaddDicts #-}

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
  gbaddDicts
    = Rec . K1 . baddDicts . unK1 . unRec
  {-# INLINE gbaddDicts #-}



instance GAllBC (Rec a a) where
  type GAllB c (Rec a a) = ()

instance GConstraintsB c f (Rec a a)
                           (Rec a a)
                           (Rec a a) where
  gbaddDicts = id
  {-# INLINE gbaddDicts #-}


-- ============================================================================
-- ## Identifying recursive usages of the barbie-type ##
--
-- ============================================================================

data Self  (b :: (k -> *) -> *) (f :: k -> *)
data Other (b :: (k -> *) -> *) (f :: k -> *)

-- | We use type-families to generically compute @'AllB' c b@. Intuitively, if
--   @b' f@ occurs inside @b f@, then we should just add @AllB b' c@ to
--   @AllB b c@. The problem is that if @b@ is a recursive type, and @b'@ is @b@,
--   then ghc will choke and blow the stack (instead of computing a fixpoint).
--
--   So, we would like to behave differently when @b = b'@ and add @()@ instead
--   of `AllB b f` to break the recursion. Our trick will be to use a type
--   family to inspect @RepN (b f)@ and distinguish recursive usages from
--   non-recursive ones, tagging them with different types, so we can distinguish
--   them in the instances.
type family TagSelf (b :: (k -> *) -> *) (repbf :: * -> *) :: * -> * where
  TagSelf b (M1 mt m s)
    = M1 mt m (TagSelf b s)

  TagSelf b (l :+: r)
    = TagSelf b l :+: TagSelf b r

  TagSelf b (l :*: r)
    = TagSelf b l :*: TagSelf b r

  TagSelf b (Rec (b f) (b g))
    = Rec (Self b f) (b g)

  TagSelf (b :: (k -> *) -> *) (Rec (b' f) ((b'' :: (k -> *) -> *) g))
    = Rec (Other b' f) (b'' g)

  TagSelf b (Rec p a)
    = Rec p a

  TagSelf b U1
    = U1

  TagSelf b V1
    = V1


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance ConstraintsB Proxy where
  type AllB c Proxy = ()

  baddDicts _ = Proxy
  {-# INLINE baddDicts #-}

instance (ConstraintsB a, ConstraintsB b) => ConstraintsB (Product a b) where
  type AllB c (Product a b) = (AllB c a, AllB c b)

  baddDicts (Pair x y) = Pair (baddDicts x) (baddDicts y)
  {-# INLINE baddDicts #-}

instance (ConstraintsB a, ConstraintsB b) => ConstraintsB (Sum a b) where
  type AllB c (Sum a b) = (AllB c a, AllB c b)

  baddDicts (InL x) = InL (baddDicts x)
  baddDicts (InR x) = InR (baddDicts x)
  {-# INLINE baddDicts #-}

instance ConstraintsB (Const a) where
  type AllB c (Const a) = ()

  baddDicts (Const x) = Const x
  {-# INLINE baddDicts #-}

instance (Functor f, ConstraintsB b) => ConstraintsB (f `Compose` b) where
  type AllB c (f `Compose` b) = AllB c b

  baddDicts (Compose x)
    = Compose (baddDicts <$> x)
  {-# INLINE baddDicts #-}
