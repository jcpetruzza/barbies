{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
module Barbies.Generics.Constraints
  ( GAll
  , X, Y
  , Self, Other, SelfOrOther
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

data X a
data family Y :: k

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

type instance GAll n c (Rec l r) = GAllRec n c l r
type family GAllRec
  (n :: Nat)
  (c :: k -> Constraint)
  (l :: Type)
  (r :: Type) :: Constraint
  where
    GAllRec n c (P n X _) (X a) = c a
    GAllRec _ _ _ _ = ()

-- {{ Functor application -----------------------------------------------------
instance
  -- a' is a, maybe with Param applications
  GConstraints n c f (Rec (P n X a') (X a))
                     (Rec (P n f a') (f a))
                     (Rec (P n (Dict c `Product` f) a')
                              ((Dict c `Product` f) a))
  where
  gaddDicts
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gaddDicts #-}

-- }} Functor application -----------------------------------------------------

-- {{ Not a functor application -----------------------------------------------

instance
  -- b is a, but with X or Y instead of Param ...
  -- a' is a, maybe with occurrences of Param
  -- b' is b, maybe with occurences of Param
  GConstraints n c f (Rec a' a) -- a' may contain Y or Param m (m > n)
                     (Rec b' b) -- a'' may only contain Param m (m > n)
                     (Rec b' b)
  where
  gaddDicts = id
  {-# INLINE gaddDicts #-}
-- }} Not a functor application -----------------------------------------------


-- ============================================================================
-- ## Identifying recursive usages of the barbie-type ##
--
-- ============================================================================

data Self  (p :: Type) (a :: Type) (x :: Type)
data Other (p :: Type) (a :: Type) (x :: Type)

type family SelfOrOther (b :: k) (b' :: k) :: Type -> Type -> Type -> Type where
  SelfOrOther b b = Self
  SelfOrOther b b' = Other
