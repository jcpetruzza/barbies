{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
module Data.Barbie.Internal.Bare
  ( Wear, Bare
  , BareB(..)
  , bstripFrom, bcoverWith

  , Gbstrip(..)
  , gbstripDefault
  , gbcoverDefault
  )

where

import Data.Barbie.Internal.Functor (FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Functor.Identity (Identity(..))

import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

-- | The 'Wear' type-function allows one to define a type as
--
-- @
-- data B f
--   = B { f1 :: Wear f Int
--       , f2 :: Wear f Bool
--       }
-- @
--
-- The advantage being that on can then retrieve a "naked"
-- value by doing:
--
-- @
-- B { f1 :: 5, f2 = True } :: B Bare
-- #
type family Wear f a where
  Wear Bare a = a
  Wear f    a = f a

-- | 'Bare' is the only type such that 'Wear Bare a ~ a'.
data Bare a

-- | Class of Barbie-types defined using 'Wear' and can therefore
--   have 'Bare' versions. Must satisfy:
--
-- @
-- 'bcover' . 'bstrip' = 'id'
-- 'bstrip' . 'bcover' = 'id'
-- @
class FunctorB b => BareB b where
    bstrip :: b Identity -> b Bare
    bcover :: b Bare -> b Identity

    default bstrip :: CanDeriveGenericInstance b => b Identity -> b Bare
    bstrip = gbstripDefault

    default bcover :: CanDeriveGenericInstance' b => b Bare -> b Identity
    bcover = gbcoverDefault

-- | Generalization of 'bstrip' to arbitrary functors
bstripFrom :: BareB b => (forall a . f a -> a) -> b f -> b Bare
bstripFrom f
  = bstrip . bmap (Identity . f)

-- | Generalization of 'bcover' to arbitrary functors
bcoverWith :: BareB b => (forall a . a -> f a) -> b Bare -> b f
bcoverWith f
  = bmap (f . runIdentity) . bcover

-- | All types that admit a generic FunctorB' instance, and have all
--   their occurrences of 'f' under a 'Wear' admit a generic 'BareB'
--   instance.
type CanDeriveGenericInstance b
  = ( Generic (b (Target I))
    , Generic (b Bare)
    , Gbstrip (Rep (b (Target I)))
    , Rep (b Bare) ~ ReplWear (Target I) (Target B) (Rep (b (Target I)))
    )

type CanDeriveGenericInstance' b
  = ( Generic (b (Target I))
    , Generic (b (Target B))
    , Gbcover (Rep (b (Target B)))
    , Rep (b (Target I)) ~ ReplWear (Target B) (Target I) (Rep (b (Target B)))
    )


-- | Default implementatio of 'bstrip' based on 'Generic'.
gbstripDefault :: CanDeriveGenericInstance b => b Identity -> b Bare
gbstripDefault b
  = to $ gbstrip $ from (unsafeTargetBarbie @I b)

-- | Default implementatio of 'bstrip' based on 'Generic'.
gbcoverDefault :: CanDeriveGenericInstance' b => b Bare -> b Identity
gbcoverDefault b
  = unsafeUntargetBarbie @I $ to $ gbcover $ from (unsafeTargetBarbie @B b)


data I a
data B a

unsafeUntargetBare :: Target B a -> a
unsafeUntargetBare = unsafeCoerce


type family ReplWear f g rep where
  ReplWear f g (M1 i c x) = M1 i c (ReplWear f g x)
  ReplWear f g V1         = V1
  ReplWear f g U1         = U1
  ReplWear f g (l :+: r)  = ReplWear f g l :+: ReplWear f g r
  ReplWear f g (l :*: r)  = ReplWear f g l :*: ReplWear f g r

  ReplWear (Target B) g (K1 i (Target B a)) = K1 i (g a)
  ReplWear (Target B) g (K1 i (b (Target B))) = K1 i (b g)
  ReplWear (Target B) g (K1 i (h (b (Target B)))) = K1 i (h (b g))

  ReplWear f (Target B) (K1 i (f a)) = K1 i a
  ReplWear f (Target B) (K1 i (b f)) = K1 i (b Bare)
  ReplWear f (Target B) (K1 i (h (b f))) = K1 i (h (b Bare))


class Gbstrip rep where
  gbstrip :: rep x -> ReplWear (Target I) (Target B) rep x

class Gbcover rep where
  gbcover :: rep x -> ReplWear (Target B) (Target I) rep x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance Gbstrip x => Gbstrip (M1 i c x) where
  {-# INLINE gbstrip #-}
  gbstrip (M1 x) = M1 (gbstrip x)

instance Gbstrip V1 where
  gbstrip _ = undefined

instance Gbstrip U1 where
  {-# INLINE gbstrip #-}
  gbstrip u1 = u1

instance (Gbstrip l, Gbstrip r) => Gbstrip (l :*: r) where
  {-# INLINE gbstrip #-}
  gbstrip (l :*: r)
    = (gbstrip l) :*: gbstrip r

instance (Gbstrip l, Gbstrip r) => Gbstrip (l :+: r) where
  {-# INLINE gbstrip #-}
  gbstrip = \case
    L1 l -> L1 (gbstrip l)
    R1 r -> R1 (gbstrip r)


instance Gbcover x => Gbcover (M1 i c x) where
  {-# INLINE gbcover #-}
  gbcover (M1 x) = M1 (gbcover x)

instance Gbcover V1 where
  gbcover _ = undefined

instance Gbcover U1 where
  {-# INLINE gbcover #-}
  gbcover u1 = u1

instance (Gbcover l, Gbcover r) => Gbcover (l :*: r) where
  {-# INLINE gbcover #-}
  gbcover (l :*: r)
    = (gbcover l) :*: gbcover r

instance (Gbcover l, Gbcover r) => Gbcover (l :+: r) where
  {-# INLINE gbcover #-}
  gbcover = \case
    L1 l -> L1 (gbcover l)
    R1 r -> R1 (gbcover r)

-- --------------------------------
-- The interesting cases (gbstrip)
-- --------------------------------


instance {-# OVERLAPPING #-} Gbstrip (K1 R (Target I a)) where
  {-# INLINE gbstrip #-}
  gbstrip (K1 ia)
    = K1 $ runIdentity $ unsafeUntarget @I ia

instance {-# OVERLAPPING #-} BareB b => Gbstrip (K1 R (b (Target I))) where
  {-# INLINE gbstrip #-}
  gbstrip (K1 bf)
    = K1 $ bstrip $ unsafeUntargetBarbie @I bf

instance {-# OVERLAPPING #-}
  ( Functor h
  , BareB b
  , ReplWear (Target I) (Target B) (K1 R (h (b (Target I))))  -- shouldn't be
      ~ (K1 R (h (b Bare)))  -- necessary but ghc chokes otherwise
  )
   => Gbstrip (K1 R (h (b (Target I)))) where
  {-# INLINE gbstrip #-}
  gbstrip (K1 hbf)
    = K1 (fmap (bstrip . unsafeUntargetBarbie @I) hbf)


instance (K1 i c) ~ ReplWear (Target I) (Target B) (K1 i c) => Gbstrip (K1 i c) where
  {-# INLINE gbstrip #-}
  gbstrip k1 = k1


-- --------------------------------
-- The interesting cases (gbcover)
-- --------------------------------


instance {-# OVERLAPPING #-} Gbcover (K1 R (Target B a)) where
  {-# INLINE gbcover #-}
  gbcover (K1 a)
    = K1 $ unsafeTarget @I $ Identity $ unsafeUntargetBare a

instance {-# OVERLAPPING #-} BareB b => Gbcover (K1 R (b (Target B))) where
  {-# INLINE gbcover #-}
  gbcover (K1 bf)
    = K1 $ unsafeTargetBarbie @I $ bcover $ unsafeUntargetBarbie @B bf

instance {-# OVERLAPPING #-}
  ( Functor h
  , BareB b
  , ReplWear (Target B) (Target I) (K1 R (h (b (Target B))))  -- shouldn't be
      ~ (K1 R (h (b (Target I))))  -- necessary but ghc chokes otherwise
  )
   => Gbcover (K1 R (h (b (Target B)))) where
  {-# INLINE gbcover #-}
  gbcover (K1 hbb)
    = K1 (fmap (unsafeTargetBarbie @I . bcover . unsafeUntargetBarbie @B) hbb)

instance (K1 i c) ~ ReplWear (Target B) (Target I) (K1 i c) => Gbcover (K1 i c) where
  {-# INLINE gbcover #-}
  gbcover k1 = k1
