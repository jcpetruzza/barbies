{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE PolyKinds            #-}
module Barbies.Generics.Constraints
  ( GAll
  , X, Y
  , TagSelf, Self, Other
  , GConstraints(..)
  )

where

import Barbies.Internal.Dicts(Dict (..))

import Data.Functor.Product (Product (..))
import Data.Kind            (Constraint, Type)
import GHC.TypeLits         (Nat, type (+))

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


type instance GAll n c (Rec (P n X _) (X a)) = c a

-- {{ Functor application -----------------------------------------------------
instance
  GConstraints n c f (Rec (P n X a') (X a))
                     (Rec (P n f a') (f a))
                     (Rec (P n (Dict c `Product` f) a')
                              ((Dict c `Product` f) a))
  where
  gaddDicts
    = Rec . K1 . Pair Dict . unK1 . unRec
  {-# INLINE gaddDicts #-}


instance
  GConstraints n c f (Rec (P m h a') (h a))
                     (Rec (P m h a') (h a))
                     (Rec (P m h a') (h a))
  where
  gaddDicts = id
  {-# INLINE gaddDicts #-}
-- }} Functor application -----------------------------------------------------

-- {{ Not a functor application -----------------------------------------------

-- Break all recursive cases
-- b' is b, maybe with 'Param' annotations
type instance GAll 0 c (Rec (Self b' (P 0 X)) (b X)) = ()
type instance GAll 1 c (Rec (Self b' (P 1 X) (P 0 Y)) (b X Y)) = ()

type instance GAll n c (Rec a a) = ()

instance
  GConstraints n c f (Rec a a)
                     (Rec a a)
                     (Rec a a)
  where
  gaddDicts = id
  {-# INLINE gaddDicts #-}

instance
  GConstraints n c f (Rec (P m a') a)
                     (Rec (P m a') a)
                     (Rec (P m a') a)
  where
  gaddDicts = id
  {-# INLINE gaddDicts #-}
-- }} Not a functor application -----------------------------------------------


-- ============================================================================
-- ## Identifying recursive usages of the barbie-type ##
--
-- ============================================================================

data family Self  (b :: k -> k') :: k -> k'
data family Other (b :: k -> k') :: k -> k'

-- | We use type-families to generically compute @'AllB' c b@. Intuitively, if
--   @b' f@ occurs inside @b f@, then we should just add @AllB b' c@ to
--   @AllB b c@. The problem is that if @b@ is a recursive type, and @b'@ is @b@,
--   then ghc will choke and blow the stack (instead of computing a fixpoint).
--
--   So, we would like to behave differently when @b = b'@ and add @()@ instead
--   of @'AllB' b f@ to break the recursion. Our trick will be to use a type
--   family to inspect @RepN (b f)@ and distinguish recursive usages from
--   non-recursive ones, tagging them with different types, so we can distinguish
--   them in the instances.
type TagSelf n b repbf
  = TagSelf' n b (Indexed b (n + 1)) repbf

type family TagSelf' (n :: Nat) (b :: kb) (b' :: kb) (repbf :: * -> *) :: * -> * where
  TagSelf' n b b' (M1 mt m s)
    = M1 mt m (TagSelf' n b b' s)

  TagSelf' n b b' (l :+: r)
    = TagSelf' n b b' l :+: TagSelf' n b b' r

  TagSelf' n b b' (l :*: r)
    = TagSelf' n b b' l :*: TagSelf' n b b' r

  TagSelf' 0 b  b' (Rec (b' f) (b g))
    = Rec (Self b' f) (b g)

  TagSelf' 0 (b :: k -> *) b' (Rec ((b'' :: k -> *) f) ((b''' :: k -> *) g))
    = Rec (Other b'' f) (b''' g)

  TagSelf' 1 b  b' (Rec (b' fl fr) (b gl gr))
    = Rec (Self b' fl fr) (b gl gr)

  TagSelf' 1 (b :: kl -> kr ->  *) b' (Rec ((b'' :: kl -> kr -> *) fl fr) ((b''' :: kl -> kr -> *) gl gr))
    = Rec (Other b'' fl fr) (b''' gl gr)

  TagSelf' n b b' (Rec p a)
    = Rec p a

  TagSelf' n b b' U1
    = U1

  TagSelf' n b b' V1
    = V1
