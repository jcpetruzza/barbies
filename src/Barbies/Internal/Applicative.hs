{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies.Internal.Applicative
  ( ApplicativeB(bpure, bprod)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4

  , CanDeriveApplicativeB
  , GApplicativeB(..)
  , gbprodDefault, gbpureDefault
  )

where

import Barbies.Internal.Functor (FunctorB (..))

import Data.Functor.Const   (Const (..))
import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))
import Data.Semigroup       (Semigroup(..))

import Data.Generics.GenericN

import Prelude hiding (Semigroup(..))

-- | A 'FunctorB' with application, providing operations to:
--     * embed an "empty" value ('bpure')
--     * align and combine values ('bprod')
--
--  It should satisfy the following laws:
--
--  [Naturality of 'bprod']
--
--    @
--    'bmap' (\('Pair' a b) -> 'Pair' (f a) (g b)) (u `'bprod'` v) = 'bmap' f u `'bprod'` 'bmap' g v
--    @
--
--  [Left and right identity]
--
--  @
--  'bmap' (\('Pair' _ b) -> b) ('bpure' e ``bprod'` v) = v
--  'bmap' (\('Pair' a _) -> a) (u ``bprod' `'bpure' e) = u
--  @
--
-- [Associativity]
--
--  @
--  'bmap' (\('Pair' a ('Pair' b c)) -> 'Pair' ('Pair' a b) c) (u `'bprod'` (v `'bprod'` w)) = (u `'bprod'` v) `'bprod'` w
--  @
--
--  It is to 'FunctorB' in is that 'Applicative'
--  relates to 'Functor'. For a presentation of 'Applicative' as
--  a monoidal functor, see Section 7 of
--  <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>.
--
-- There is a default implementation of 'bprod' and 'bpure' based on 'Generic'.
-- Intuitively, it works on types where the value of `bpure` is uniquely defined.
-- This corresponds rougly to record types (in the presence of sums, there would
-- be several candidates for `bpure`), where the argument @f@ covers every field.
class FunctorB b => ApplicativeB (b :: (k -> Type) -> Type) where
  bpure :: (forall a . f a) -> b f
  bprod :: b f -> b g -> b (f `Product` g)

  default bpure :: CanDeriveApplicativeB b f f => (forall a . f a) -> b f
  bpure = gbpureDefault

  default bprod :: CanDeriveApplicativeB b f g => b f -> b g -> b (f `Product` g)
  bprod = gbprodDefault


-- | An alias of 'bprod', since this is like a 'zip' for Barbie-types.
bzip :: ApplicativeB b => b f -> b g -> b (f `Product` g)
bzip = bprod

-- | An equivalent of 'unzip' for Barbie-types.
bunzip :: ApplicativeB b => b (f `Product` g) -> (b f, b g)
bunzip bfg = (bmap (\(Pair a _) -> a) bfg, bmap (\(Pair _ b) -> b) bfg)

-- | An equivalent of 'Data.List.zipWith' for Barbie-types.
bzipWith :: ApplicativeB b => (forall a. f a -> g a -> h a) -> b f -> b g -> b h
bzipWith f bf bg
  = bmap (\(Pair fa ga) -> f fa ga) (bf `bprod` bg)

-- | An equivalent of 'Data.List.zipWith3' for Barbie-types.
bzipWith3
  :: ApplicativeB b
  => (forall a. f a -> g a -> h a -> i a)
  -> b f -> b g -> b h -> b i
bzipWith3 f bf bg bh
  = bmap (\(Pair (Pair fa ga) ha) -> f fa ga ha)
         (bf `bprod` bg `bprod` bh)


-- | An equivalent of 'Data.List.zipWith4' for Barbie-types.
bzipWith4
  :: ApplicativeB b
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> b f -> b g -> b h -> b i -> b j
bzipWith4 f bf bg bh bi
  = bmap (\(Pair (Pair (Pair fa ga) ha) ia) -> f fa ga ha ia)
         (bf `bprod` bg `bprod` bh `bprod` bi)


-- | @'CanDeriveApplicativeB' B f g@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B@ has only one constructor (that is, it is not a sum-type).
--
--     * Every field of @B f@ is of the form @f a@, for some type @a@.
--       In other words, @B@ has no "hidden" structure.
type CanDeriveApplicativeB b f g
  = ( GenericN (b f)
    , GenericN (b g)
    , GenericN (b (f `Product` g))
    , GApplicativeB f g (RepN (b f)) (RepN (b g)) (RepN (b (f `Product` g)))
    )


-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'bprod' based on 'Generic'.
gbprodDefault
  :: forall b f g
  .  CanDeriveApplicativeB b f g
  => b f -> b g -> b (f `Product` g)
gbprodDefault l r
  = toN $ gbprod (Proxy @f) (Proxy @g) (fromN l) (fromN r)
{-# INLINE gbprodDefault #-}

gbpureDefault:: forall b f . CanDeriveApplicativeB b f f => (forall a . f a) -> b f
gbpureDefault x
  = toN $ gbpure (Proxy @f) (Proxy @(RepN (b f))) (Proxy @(RepN (b (f `Product` f)))) x
{-# INLINE gbpureDefault #-}

class GApplicativeB (f :: k -> *) (g :: k -> *) repbf repbg repbfg where
  gbprod :: Proxy f -> Proxy g -> repbf x -> repbg x -> repbfg x

  gbpure :: (f ~ g, repbf ~ repbg) => Proxy f -> Proxy repbf -> Proxy repbfg -> (forall a . f a) -> repbf x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GApplicativeB f g repf repg repfg => GApplicativeB f g (M1 i c repf)
                                                        (M1 i c repg)
                                                        (M1 i c repfg) where
  gbprod pf pg (M1 l) (M1 r) = M1 (gbprod pf pg l r)
  {-# INLINE gbprod #-}

  gbpure pf _ _ x = M1 (gbpure pf (Proxy @repf) (Proxy @repfg) x)
  {-# INLINE gbpure #-}


instance GApplicativeB f g U1 U1 U1 where
  gbprod _ _ U1 U1 = U1
  {-# INLINE gbprod #-}

  gbpure _ _ _ _ = U1
  {-# INLINE gbpure #-}

instance
  ( GApplicativeB f g lf lg lfg
  , GApplicativeB f g rf rg rfg
  ) => GApplicativeB f g (lf  :*: rf)
                         (lg  :*: rg)
                         (lfg :*: rfg) where
  gbprod pf pg (l1 :*: l2) (r1 :*: r2)
    = (l1 `lprod` r1) :*: (l2 `rprod` r2)
    where
      lprod = gbprod pf pg
      rprod = gbprod pf pg
  {-# INLINE gbprod #-}

  gbpure pf _ _ x = (gbpure pf (Proxy @lf) (Proxy @lfg) x :*: gbpure pf (Proxy @rf) (Proxy @rfg) x)
  {-# INLINE gbpure #-}

-- --------------------------------
-- The interesting cases
-- --------------------------------

type P0 = Param 0

instance GApplicativeB f g (Rec (P0 f a) (f a))
                           (Rec (P0 g a) (g a))
                           (Rec (P0 (f `Product` g) a) ((f `Product` g) a)) where
  gbprod _ _ (Rec (K1 fa)) (Rec (K1 ga))
    = Rec (K1 (Pair fa ga))
  {-# INLINE gbprod #-}

  gbpure _ _ _ x = Rec (K1 x)
  {-# INLINE gbpure #-}


instance
  ( SameOrParam b b'
  , ApplicativeB b'
  ) => GApplicativeB f g (Rec (b (P0 f)) (b' f))
                         (Rec (b (P0 g)) (b' g))
                         (Rec (b (P0 (f `Product` g))) (b' (f `Product` g))) where
  gbprod _ _ (Rec (K1 bf)) (Rec (K1 bg))
    = Rec (K1 (bf `bprod` bg))
  {-# INLINE gbprod #-}

  gbpure _ _ _ x = Rec (K1 (bpure x))
  {-# INLINE gbpure #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance ApplicativeB Proxy where
  bpure _ = Proxy
  {-# INLINE bpure #-}

  bprod _ _ = Proxy
  {-# INLINE bprod #-}

instance Monoid a => ApplicativeB (Const a) where
  bpure _
    = Const mempty
  {-# INLINE bpure #-}

  bprod (Const l) (Const r)
    = Const (l <> r)
  {-# INLINE bprod #-}

instance (ApplicativeB a, ApplicativeB b) => ApplicativeB (Product a b) where
  bpure x
    = Pair (bpure x) (bpure x)
  {-# INLINE bpure #-}

  bprod (Pair ll lr) (Pair rl rr)
    = Pair (bprod ll rl) (bprod lr rr)
  {-# INLINE bprod #-}
