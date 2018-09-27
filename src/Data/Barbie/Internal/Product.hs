{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Internal.Product
  ( ProductB(buniq, bprod)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4
  , (/*/), (/*)

  , CanDeriveProductB
  , GProductB
  , gbprodDefault, gbuniqDefault
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))

import Data.Functor.Product (Product(..))
import Data.Functor.Prod

import Data.Generics.GenericN (GenericN(..), toN, fromN, Rec(..), Param, SameOrParam)
import GHC.Generics


-- | Barbie-types that can form products, subject to the laws:
--
-- @
-- 'bmap' \('Pair' a _) . 'uncurry' . 'bprod' = 'fst'
-- 'bmap' \('Pair' _ b) . 'uncurry' . 'bprod' = 'snd'
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
-- Intuitively, the laws for this class require that `b` hides no structure
-- from its argument @f@. Because of this, any @x :: forall a . f a@
-- determines a unique value of @b f@, witnessed by the 'buniq' method.
-- Formally:
--
-- @
-- 'const' ('buniq' x) = 'bmap' ('const' x)
-- @
--
-- There is a default implementation of 'bprod' and 'buniq' for 'Generic' types,
-- so instances can derived automatically.
class FunctorB b => ProductB b where
  bprod :: b f -> b g -> b (Product f g)

  buniq :: (forall a . f a) -> b f

  default bprod :: CanDeriveProductB b f g => b f -> b g -> b (Product f g)
  bprod = gbprodDefault

  default buniq :: CanDeriveProductB b f f => (forall a . f a) -> b f
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
type CanDeriveProductB b f g
  = ( GenericN (b f)
    , GenericN (b g)
    , GenericN (b (f `Product` g))
    , GProductB f g (RepN (b f)) (RepN (b g)) (RepN (b (f `Product` g)))
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
  :: forall b f g
  .  CanDeriveProductB b f g
  => b f -> b g -> b (Product f g)
gbprodDefault l r
  = toN $ gbprod @f @g (fromN l) (fromN r)
{-# INLINE gbprodDefault #-}

gbuniqDefault:: forall b f . CanDeriveProductB b f f => (forall a . f a) -> b f
gbuniqDefault x
  = toN (gbuniq @f @f @_ @(RepN (b f)) @(RepN (b (f `Product` f))) x)
{-# INLINE gbuniqDefault #-}

class GProductB (f :: * -> *) (g :: * -> *) repbf repbg repbfg where
  gbprod :: repbf x -> repbg x -> repbfg x

  gbuniq :: (forall a . f a) -> repbf x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GProductB f g repf repg repfg => GProductB f g (M1 i c repf)
                                                        (M1 i c repg)
                                                        (M1 i c repfg) where
  gbprod (M1 l) (M1 r) = M1 (gbprod @f @g l r)
  {-# INLINE gbprod #-}

  gbuniq x = M1 (gbuniq @f @g @repf @repg @repfg x)
  {-# INLINE gbuniq #-}


instance GProductB f g U1 U1 U1 where
  gbprod U1 U1 = U1
  {-# INLINE gbprod #-}

  gbuniq _ = U1
  {-# INLINE gbuniq #-}

instance
  ( GProductB f g lf lg lfg
  , GProductB f g rf rg rfg
  ) => GProductB f g (lf  :*: rf)
                     (lg  :*: rg)
                     (lfg :*: rfg) where
  gbprod (l1 :*: l2) (r1 :*: r2)
    = (l1 `lprod` r1) :*: (l2 `rprod` r2)
    where
      lprod = gbprod @f @g
      rprod = gbprod @f @g
  {-# INLINE gbprod #-}

  gbuniq x = (gbuniq @f @g @lf @lg @lfg x :*: gbuniq @f @g @rf @rg @rfg x)
  {-# INLINE gbuniq #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0

instance GProductB f g (Rec (P0 f a) (f a))
                       (Rec (P0 g a) (g a))
                       (Rec (P0 (f `Product` g) a) ((f `Product` g) a)) where
  gbprod (Rec (K1 fa)) (Rec (K1 ga))
    = Rec (K1 (Pair fa ga))
  {-# INLINE gbprod #-}

  gbuniq x = Rec (K1 x)
  {-# INLINE gbuniq #-}


instance
  ( SameOrParam b b'
  , ProductB b'
  ) => GProductB f g (Rec (b (P0 f)) (b' f))
                     (Rec (b (P0 g)) (b' g))
                     (Rec (b (P0 (f `Product` g))) (b' (f `Product` g))) where
  gbprod (Rec (K1 bf)) (Rec (K1 bg))
    = Rec (K1 (bf `bprod` bg))
  {-# INLINE gbprod #-}

  gbuniq x = Rec (K1 (buniq x))
  {-# INLINE gbuniq #-}
