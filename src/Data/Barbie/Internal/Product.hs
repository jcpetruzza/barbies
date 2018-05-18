{-# LANGUAGE ConstraintKinds      #-}
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
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4
  , (/*/), (/*)

  , CanDeriveGenericInstance, CanDeriveGenericInstance'
  , GProductB
  , gbprodDefault, gbuniqDefault
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics
import Data.Barbie.Internal.Tags(F, G, FxG)
import Data.Functor.Product (Product(..))
import Data.Functor.Prod

import GHC.Generics


-- | Barbie-types that can form products, subject to the laws:
--
-- @
-- 'bmap' \('Pair' a _) . 'uncurry' . 'bprod' = 'fst'
-- 'bmap' \('Pair' _ b) . 'uncurry' . 'bprod' = 'snd
-- @
--
-- Notice that because of the laws, having an internal product structure is not
-- enough to have a lawful instance. E.g.
--
-- @
-- data Ok  f = Ok {o1 :: f 'String', o2 :: f 'Int'}        -- has an instance
-- data Bad f = Bad{b1 :: f 'String', hiddenFromArg: 'Int'} -- no lawful instance
-- @
--
-- There is a default implementation of 'bprod' and 'buniq' for 'Generic' types,
-- so instances can derived automatically.
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

  default bprod :: CanDeriveGenericInstance b => b f -> b g -> b (Product f g)
  bprod = gbprodDefault

  default buniq :: CanDeriveGenericInstance' b => (forall a . f a) -> b f
  buniq = gbuniqDefault


-- | An alias of 'bprod', since this is like a 'zip' for Barbie-types.
bzip :: ProductB b => b f -> b g -> b (Product f g)
bzip = bprod

-- | An equivalent of 'unzip' for Barbie-types.
bunzip :: ProductB b => b (Product f g) -> (b f, b g)
bunzip bfg = (bmap (\(Pair a _) -> a) bfg, bmap (\(Pair _ b) -> b) bfg)

-- | An equivalent of 'Data.List.zipWith' for Barbie-types.
bzipWith :: ProductB b => (forall a. f a -> g a -> h a) -> b f -> b g -> b h
bzipWith f bf bg
  = bmap (\(Pair fa ga) -> f fa ga) (bf `bprod` bg)

-- | An equivalent of 'Data.List.zipWith3' for Barbie-types.
bzipWith3
  :: ProductB b
  => (forall a. f a -> g a -> h a -> i a)
  -> b f -> b g -> b h -> b i
bzipWith3 f bf bg bh
  = bmap (\(Pair (Pair fa ga) ha) -> f fa ga ha)
         (bf `bprod` bg `bprod` bh)


-- | An equivalent of 'Data.List.zipWith4' for Barbie-types.
bzipWith4
  :: ProductB b
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> b f -> b g -> b h -> b i -> b j
bzipWith4 f bf bg bh bi
  = bmap (\(Pair (Pair (Pair fa ga) ha) ia) -> f fa ga ha ia)
         (bf `bprod` bg `bprod` bh `bprod` bi)

-- | The requirements to to derive @'ProductB' (B f)@ are more strict than those for
--   'FunctorB' or 'TraversableB'. Intuitively, we need:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * @B@ has only one constructor.
--
--     * Every field of @B@' constructor is of the form 'f t'. That is, @B@ has no
--       hidden structure.
type CanDeriveGenericInstance b
  = ( Generic (b (Target F))
    , Generic (b (Target G))
    , Generic (b (Target FxG))
    , GProductB (Rep (b (Target F)))
    , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
    , Rep (b (Target FxG)) ~ Repl (Target F) (Target FxG) (Rep (b (Target F)))
    )

type CanDeriveGenericInstance' b
  = ( Generic (b (Target F))
    , GProductB (Rep (b (Target F)))
    )

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
  :: CanDeriveGenericInstance b
  => b f -> b g -> b (Product f g)
gbprodDefault l r
  = let l' = from (unsafeTargetBarbie @F l)
        r' = from (unsafeTargetBarbie @G r)
     in unsafeUntargetBarbie @FxG $ to (gbprod l' r')

gbuniqDefault
  :: CanDeriveGenericInstance' b
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


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GProductB (K1 R (Target (W F) a)) where
  {-# INLINE gbprod #-}
  gbprod (K1 fa) (K1 ga)
    = let fxga = Pair (unsafeUntarget @(W F) fa) (unsafeUntarget @(W G) ga)
      in K1 (unsafeTarget @(W FxG) fxga)

  {-# INLINE gbuniq #-}
  gbuniq x = K1 (unsafeTarget @(W F) x)

instance {-# OVERLAPPING #-} GProductB (K1 R (Target F a)) where
  {-# INLINE gbprod #-}
  gbprod (K1 fa) (K1 ga)
    = let fxga = Pair (unsafeUntarget @F fa) (unsafeUntarget @G ga)
      in K1 (unsafeTarget @FxG fxga)

  {-# INLINE gbuniq #-}
  gbuniq x = K1 (unsafeTarget @F x)


instance {-# OVERLAPPING #-} ProductB b => GProductB (K1 R (b (Target F))) where
  {-# INLINE gbprod #-}
  gbprod (K1 bf) (K1 bg)
    = let bfxg = unsafeUntargetBarbie @F bf `bprod` unsafeUntargetBarbie @G bg
      in K1 (unsafeTargetBarbie @FxG bfxg)

  {-# INLINE gbuniq #-}
  gbuniq x = K1 (unsafeTargetBarbie @F (buniq x))
