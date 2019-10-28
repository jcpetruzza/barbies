{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.ConstraintsT
  ( ConstraintsT(..)
  , tmapC
  , ttraverseC
  , AllTF
  , tdicts
  , tpureC
  , tmempty
  , tzipWithC
  , tzipWith3C
  , tzipWith4C
  , tfoldMapC

  , CanDeriveConstraintsT
  , gtaddDictsDefault
  )

where

import Barbies.Internal.ApplicativeT(ApplicativeT (..))
import Barbies.Generics.Constraints(GConstraints(..), GAll, TagSelf, Self, Other, X, Y)
import Barbies.Internal.Dicts(ClassF, Dict (..), requiringDict)
import Barbies.Internal.FunctorT(FunctorT (..))
import Barbies.Internal.TraversableT(TraversableT (..))

import Data.Functor.Const(Const(..))
import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)
import Data.Proxy(Proxy(..))

import Data.Generics.GenericN


-- | Instances of this class provide means to talk about constraints,
--   both at compile-time, using 'AllT', and at run-time, in the form
--   of 'Dict', via 'taddDicts'.
--
--   A manual definition would look like this:
--
-- @
-- data T f a = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
--
-- instance 'ConstraintsT' T where
--   type 'AllT' c T = (c 'Int', c 'String', c 'Bool')
--
--   'taddDicts' t = case t of
--     A x y -> A ('Pair' 'Dict' x) ('Pair' 'Dict' y)
--     B z w -> B ('Pair' 'Dict' z) ('Pair' 'Dict' w)
-- @
--
-- Now, when we given a @T f@, if we need to use the 'Show' instance of
-- their fields, we can use:
--
-- @
-- 'taddDicts' :: AllT Show t => t f -> t ('Dict' 'Show' `'Product'` f)
-- @
--
-- There is a default implementation of 'ConstraintsT' for
-- 'Generic' types, so in practice one will simply do:
--
-- @
-- derive instance 'Generic' (T f a)
-- instance 'ConstraintsT' T
-- @
class FunctorT t => ConstraintsT (t :: (kl -> *) -> (kr -> *)) where
  -- | @'AllT' c t@ should contain a constraint @c a@ for each
  --   @a@ occurring under an @f@ in @t f@.
  --
  -- For requiring constraints of the form @c (f a)@, use 'AllTF'.
  type AllT (c :: k -> Constraint) t :: Constraint
  type AllT c t = GAll 1 c (GAllRepT t)

  taddDicts
    :: forall c f x
    .  AllT c t
    => t f x
    -> t (Dict c `Product` f) x

  default taddDicts
    :: forall c f x
    .  ( CanDeriveConstraintsT c t f x
       , AllT c t
       )
    => t f x
    -> t (Dict c `Product` f) x
  taddDicts = gtaddDictsDefault


-- | Like 'tmap' but a constraint is allowed to be required on
--   each element of @t@.
tmapC :: forall c t f g x
      .  (AllT c t, ConstraintsT t)
      => (forall a. c a => f a -> g a)
      -> t f x
      -> t g x
tmapC f tf
  = tmap go (taddDicts tf)
  where
    go :: forall a. (Dict c `Product` f) a -> g a
    go (d `Pair` fa) = requiringDict (f fa) d

-- | Like 'ttraverse' but with a constraint on the elements of @t@.
ttraverseC
  :: forall c t f g e x
  .  (TraversableT t, ConstraintsT t, AllT c t, Applicative e)
  => (forall a. c a => f a -> e (g a))
  -> t f x
  -> e (t g x)
ttraverseC f t
  = ttraverse (\(Pair (Dict :: Dict c a) x) -> f x) (taddDicts t)

-- | Like 'tfoldMap' but with a constraint on the function.
tfoldMapC
  :: forall c t m f x
  .  (TraversableT t, ConstraintsT t,  AllT c t, Monoid m)
  => (forall a. c a => f a -> m)
  -> t f x
  -> m
tfoldMapC f = getConst . ttraverseC @c (Const . f)


-- | Like 'Data.Functor.Barbie.tzipWith' but with a constraint on the elements of @t@.
tzipWithC
  :: forall c t f g h x
  .  (AllT c t, ConstraintsT t, ApplicativeT t)
  => (forall a. c a => f a -> g a -> h a)
  -> t f x
  -> t g x
  -> t h x
tzipWithC f tf tg
  = tmapC @c go (tf `tprod` tg)
  where
    go :: forall a. c a => Product f g a -> h a
    go (Pair fa ga) = f fa ga

-- | Like 'Data.Functor.Barbie.tzipWith3' but with a constraint on the elements of @t@.
tzipWith3C
  :: forall c t f g h i x
  .  (AllT c t, ConstraintsT t, ApplicativeT t)
  => (forall a. c a => f a -> g a -> h a -> i a)
  -> t f x
  -> t g x
  -> t h x
  -> t i x
tzipWith3C f tf tg th
  = tmapC @c go (tf `tprod` tg `tprod` th)
  where
    go :: forall a. c a => Product (Product f g) h a -> i a
    go (Pair (Pair fa ga) ha) = f fa ga ha

-- | Like 'Data.Functor.Barbie.tzipWith4' but with a constraint on the elements of @t@.
tzipWith4C
  :: forall c t f g h i j x
  .  (AllT c t, ConstraintsT t, ApplicativeT t)
  => (forall a. c a => f a -> g a -> h a -> i a -> j a)
  -> t f x
  -> t g x
  -> t h x
  -> t i x
  -> t j x
tzipWith4C f tf tg th ti
  = tmapC @c go (tf `tprod` tg `tprod` th `tprod` ti)
  where
    go :: forall a. c a => Product (Product (Product f g) h) i a -> j a
    go (Pair (Pair (Pair fa ga) ha) ia) = f fa ga ha ia


-- | Similar to 'AllT' but will put the functor argument @f@
--   between the constraint @c@ and the type @a@.
type AllTF c f t = AllT (ClassF c f) t


-- | Similar to 'itddDicts' but can produce the instance dictionaries
--   "out of the blue".
tdicts
  :: forall c t x
  . (ConstraintsT t, ApplicativeT t,  AllT c t)
  => t (Dict c) x
tdicts
  = tmap (\(Pair c _) -> c) $ taddDicts $ tpure Proxy


-- | Like 'tpure' but a constraint is allowed to be required on
--   each element of @t@.
tpureC
  :: forall c f t x
  .  ( AllT c t
     , ConstraintsT t
     , ApplicativeT t
     )
  => (forall a . c a => f a)
  -> t f x
tpureC fa
  = tmap (requiringDict @c fa) tdicts

-- | Builds a @t f x@, by applying 'mempty' on every field of @t@.
tmempty
  :: forall f t x
  .  ( AllTF Monoid f t
     , ConstraintsT t
     , ApplicativeT t
     )
  => t f x
tmempty
  = tpureC @(ClassF Monoid f) mempty


-- | @'CanDeriveConstraintsT' T f g x@ is in practice a predicate about @T@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@ and @x@:
--
--     * There is an instance of @'Generic' (T f x)@.
--
--     * @T f@ can contain fields of type @t f x@ as long as there exists a
--       @'ConstraintsT' t@ instance. In particular, recursive usages of @T f x@
--       are allowed.
type CanDeriveConstraintsT c t f x
  = ( GenericN (t f x)
    , GenericN (t (Dict c `Product` f) x)
    , AllT c t ~ GAll 1 c (GAllRepT t)
    , GConstraints 1 c f (GAllRepT t) (RepN (t f x)) (RepN (t (Dict c `Product` f) x))
    )

-- | The representation used for the generic computation of the @'AllT' c t@
--   constraints. Here 'X' and 'Y' are arbitrary constants since the actual
--   argument to @t@ is irrelevant.
type GAllRepT t = TagSelf 1 t (RepN (t X Y))

-- ===============================================================
--  Generic derivations
-- ===============================================================

-- | Default implementation of ibaddDicts' based on 'Generic'.
gtaddDictsDefault
  :: forall t c f x
  . ( CanDeriveConstraintsT c t f x
    , AllT c t
    )
  => t f x
  -> t (Dict c `Product` f) x
gtaddDictsDefault
  = toN . gaddDicts @1 @c @f @(GAllRepT t) . fromN
{-# INLINE gtaddDictsDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for ConstraintsT
-- -----------------------------------------------------------

type P = Param

instance
  ( ConstraintsT t
  , AllT c t
  ) => -- t' is t, maybe with 'Param' annotations
       GConstraints 1 c f (Rec (Self t' (P 1 X) (P 0 Y)) (t X Y))
                          (Rec (t' (P 1 f) (P 0 y)) (t f y))
                          (Rec (t' (P 1 (Dict c `Product` f)) (P 0 y))
                               (t       (Dict c `Product` f)       y))
  where
  gaddDicts
    = Rec . K1 . taddDicts . unK1 . unRec
  {-# INLINE gaddDicts #-}


type instance GAll 1 c (Rec (Other t (P 1 X) (P 0 Y)) (t' X Y)) = AllT c t'

instance
  ( ConstraintsT t
  , AllT c t
  ) => GConstraints 1 c f (Rec (Other t' (P 1 X) (P 0 Y)) (t X Y))
                          (Rec (t' (P 1 f) (P 0 y)) (t f y))
                          (Rec (t' (P 1 (Dict c `Product` f)) (P 0 y))
                               (t       (Dict c `Product` f)       y))
  where
  gaddDicts
    = Rec . K1 . taddDicts . unK1 . unRec
  {-# INLINE gaddDicts #-}
