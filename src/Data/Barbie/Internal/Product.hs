{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Internal.Product
  ( ProductB(buniq, bprod)
  , (/*/), (/*)

  , GProductB
  , gbprodDefault, gbuniqDefault
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Functor.Product (Product(..))
import Data.Functor.Prod
import Data.Kind (Constraint)

import GHC.Generics
import GHC.TypeLits (TypeError, ErrorMessage(Text, ShowType, (:<>:)))


-- | Barbie-types that can form products, subject to the laws:
--
-- @
-- 'bmap' 'Data.Bifunctor.first'  . 'uncurry' . 'bprod' = 'Data.Bifunctor.first'
-- 'bmap' 'Data.Bifunctor.second' . 'uncurry' . 'bprod' = 'Data.Bifunctor.second'
-- @
--
-- Notice that because of the laws, having an internal product structure is not
-- enough to have a lawful instance. E.g.
--
-- @
-- data Ok  f = Ok {o1 :: f 'String', o2 :: f 'Int'}        -- has an instance
-- data Bad f = Bad{b1 :: f 'String', hiddenFromArg: 'Int'} -- no lawful instance
-- @
class FunctorB b => ProductB b where
  bprod :: b f -> b g -> b (Product f g)

  -- | Intuitively, the laws for this class require that `b` hides no structure
  --   from its argument @f@. Because of this, any @x :: forall a . f a@
  --   determines a unique value of @b f@. Formally:
  --
  -- @
  -- 'const' ('buniq' x) = 'bmap' ('const' x)
  -- @
  --
  buniq :: (forall a . f a) -> b f

  default bprod
    :: ( Generic (b (Target F))
       , Generic (b (Target G))
       , Generic (b (Target FxG))
       , GProductB (Rep (b (Target F)))
       , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
       , Rep (b (Target FxG)) ~ Repl (Target F) (Target FxG) (Rep (b (Target F)))
       )
    => b f -> b g -> b (Product f g)
  bprod = gbprodDefault


  default buniq
    :: ( Generic (b (Target F))
       , GProductB (Rep (b (Target F)))
       )
    => (forall a . f a) -> b f
  buniq = gbuniqDefault


-- | Like 'bprod', but returns a binary 'Prod', instead of 'Product', which
--   composes better.
--
--   See '/*/' for usage.
(/*/)
  :: ProductB b => b f -> b g -> b (Prod '[f, g])
l /*/ r
  = bmap (\(Pair f g) -> Cons f (Cons g Unit)) (l `bprod` r)
infixr 4 /*/

-- | Similar to '/*/' but one of the sides is already a 'Prod fs'.
--
--   Note that '/*', '/*/' and 'uncurryn' are meant to be used together:
--   '/*' and '/*/' combine @b f1, b f2...b fn@ into a single product that
--   can then be consumed by using `uncurryn` on an n-ary function. E.g.
--
-- @
-- f :: f a -> g a -> h a -> i a
--
-- 'bmap' ('uncurryn' f) (bf '/*' bg '/*/' bh)
-- @
(/*) :: ProductB b => b f -> b (Prod fs) -> b (Prod (f ': fs))
l /* r =
  bmap (\(Pair f fs) -> oneTuple f `prod` fs) (l `bprod` r)
infixr 4 /*

-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'bprod' based on 'Generic'.
gbprodDefault
  :: ( Generic (b (Target F))
     , Generic (b (Target G))
     , Generic (b (Target FxG))
     , GProductB (Rep (b (Target F)))
     , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
     , Rep (b (Target FxG)) ~ Repl (Target F) (Target FxG) (Rep (b (Target F)))
     )
  => b f -> b g -> b (Product f g)
gbprodDefault l r
  = let l' = from (unsafeTargetBarbie @F l)
        r' = from (unsafeTargetBarbie @G r)
     in unsafeUntargetBarbie @FxG $ to (gbprod l' r')

gbuniqDefault
  :: ( Generic (b (Target F))
     , GProductB (Rep (b (Target F)))
     )
  => (forall a . f a) -> b f
gbuniqDefault x
  = unsafeUntargetBarbie @F $ to (gbuniq x)

class GProductB b where
  gbprod
    :: b x
    -> Repl (Target F) (Target G) b x
    -> Repl (Target F) (Target FxG) b x

  gbuniq
    :: (forall a . f a) -> b x

data F a
data G a
data FxG a


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProductB x => GProductB (M1 i c x) where
  {-# INLINE gbprod #-}
  gbprod (M1 l) (M1 r) = M1 (gbprod l r)

  {-# INLINE gbuniq #-}
  gbuniq x = M1 (gbuniq x)

instance GProductB U1 where
  {-# INLINE gbprod #-}
  gbprod U1 U1 = U1

  {-# INLINE gbuniq #-}
  gbuniq _ = U1

instance(GProductB l, GProductB r) => GProductB (l :*: r) where
  {-# INLINE gbprod #-}
  gbprod (l1 :*: l2) (r1 :*: r2)
    = (l1 `gbprod` r1) :*: (l2 `gbprod` r2)

  {-# INLINE gbuniq #-}
  gbuniq x = (gbuniq x :*: gbuniq x)

-- ----------------------------------------------
-- Error cases
-- ----------------------------------------------

data VoidType
data SumType
data HiddenStructure

type family CantDeriveFor t :: Constraint where
  CantDeriveFor VoidType
    = TypeError
        ('Text "Can't derive " ':<>: 'ShowType ProductB ':<>: 'Text ": void-types")

  CantDeriveFor SumType
    = TypeError
        ('Text "Can't derive " ':<>: 'ShowType ProductB ':<>: 'Text ": sum-types.")

  CantDeriveFor HiddenStructure
    = TypeError
        ('Text "Can't derive " ':<>: 'ShowType ProductB ':<>:
         'Text ": hidden-structure.")

instance CantDeriveFor VoidType => GProductB V1 where
  gbprod   = error "GBProductB V1 -- Can't happen"
  gbuniq _ = error "GBProductB V1 -- Can't happen"

instance CantDeriveFor SumType => GProductB (l :+: r) where
  gbprod   = error "GProductB :+: -- Can't happen"
  gbuniq _ = error "GProductB :+: -- Can't happen"

instance CantDeriveFor HiddenStructure => GProductB (K1 i c) where
  gbprod   = error "GProductB K1  -- Can't happen"
  gbuniq _ = error "GProductB K1  -- Can't happen"

-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GProductB (K1 R (Target F a)) where
  {-# INLINE gbprod #-}
  gbprod (K1 fa) (K1 ga)
    = let fxga = Pair (unsafeUntarget @F fa) (unsafeUntarget @G ga)
      in K1 (unsafeTarget @FxG fxga)

  {-# INLINE gbuniq #-}
  gbuniq x = K1 (unsafeTarget @F x)
