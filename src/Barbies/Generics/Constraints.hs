{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
module Barbies.Generics.Constraints
  ( GAll
  , GAllRep, X
  , TagSelf, Self, Other
  , GConstraints(..)
  )

where

import Barbies.Internal.Dicts(Dict (..))

import Data.Functor.Product (Product (..))
import Data.Kind            (Constraint, Type)
import GHC.TypeLits         (Nat)

import Data.Generics.GenericN

class GConstraints n c f repbx repbf repbdf where
  gaddDicts :: GAll n c repbx => repbf x -> repbdf x

type family GAll (n :: Nat) (c :: k -> Constraint) (repbf :: Type -> Type) :: Constraint

-- | The representation used for the generic computation of the @'AllB' c b@
--   constraints. Here 'X' is an arbitrary constant since the actual
--   argument to @b@ is irrelevant.
type GAllRep b = TagSelf b (RepN (b X))
data X a

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

type instance GAll n c (M1 i k repbf) = GAll n c repbf

instance
  GConstraints n c f repbx repbf repbdf
    => GConstraints n c f (M1 i k repbx)
                          (M1 i k repbf)
                          (M1 i k repbdf)
  where
  gaddDicts
    = M1 . gaddDicts @n @c @f @repbx . unM1
  {-# INLINE gaddDicts #-}



type instance GAll n c V1 = ()

instance GConstraints n c f V1 V1 V1 where
  gaddDicts _ = undefined



type instance GAll n c U1 = ()

instance GConstraints n c f U1 U1 U1 where
  gaddDicts = id
  {-# INLINE gaddDicts #-}


type instance GAll n c (l :*: r)
  = (GAll n c l, GAll n c r)

instance
  ( GConstraints n c f lx lf ldf
  , GConstraints n c f rx rf rdf
  ) => GConstraints n c f (lx  :*: rx)
                          (lf  :*: rf)
                          (ldf :*: rdf)
  where
  gaddDicts (l :*: r)
    = (gaddDicts @n @c @f @lx l) :*: (gaddDicts @n @c @f @rx r)
  {-# INLINE gaddDicts #-}


type instance GAll n c (l :+: r) = (GAll n c l, GAll n c r)

instance
  ( GConstraints n c f lx lf ldf
  , GConstraints n c f rx rf rdf
  ) => GConstraints n c f (lx  :+: rx)
                          (lf  :+: rf)
                          (ldf :+: rdf)
  where
  gaddDicts = \case
    L1 l -> L1 (gaddDicts @n @c @f @lx l)
    R1 r -> R1 (gaddDicts @n @c @f @rx r)
  {-# INLINE gaddDicts #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P = Param


type instance GAll n c (Rec (P n X _) (X a)) = c a

instance GConstraints n c f (Rec (P n X a_or_pma) (X a))
                            (Rec (P n f a_or_pma) (f a))
                            (Rec (P n (Dict c `Product` f) a_or_pma)
                                     ((Dict c `Product` f) a))
  where
  gaddDicts
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gaddDicts #-}


-- Break all recursive cases
-- b' is b, maybe with 'Param' annotations
type instance GAll n c (Rec (Self b' (P n X)) (b X)) = ()

type instance GAll n c (Rec a a) = ()

instance GConstraints n c f (Rec a a)
                            (Rec a a)
                            (Rec a a)
  where
  gaddDicts = id
  {-# INLINE gaddDicts #-}


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
type TagSelf b repbf
  = TagSelf' b (Indexed b 1) repbf

type family TagSelf' (b :: (k -> *) -> *) (b' :: (k -> *) -> *) (repbf :: * -> *) :: * -> * where
  TagSelf' b b' (M1 mt m s)
    = M1 mt m (TagSelf' b b' s)

  TagSelf' b b' (l :+: r)
    = TagSelf' b b' l :+: TagSelf' b b' r

  TagSelf' b b' (l :*: r)
    = TagSelf' b b' l :*: TagSelf' b b' r

  TagSelf' b  b' (Rec (b' f) (b g))
    = Rec (Self b' f) (b g)

  TagSelf' (b :: (k -> *) -> *) b' (Rec (b'' f) ((b''' :: (k -> *) -> *) g))
    = Rec (Other b'' f) (b''' g)

  TagSelf' b b' (Rec p a)
    = Rec p a

  TagSelf' b b' U1
    = U1

  TagSelf' b b' V1
    = V1
