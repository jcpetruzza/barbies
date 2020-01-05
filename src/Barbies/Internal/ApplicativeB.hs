{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.ApplicativeB
  ( ApplicativeB(bpure, bprod)
  , bzip, bunzip, bzipWith, bzipWith3, bzipWith4

  , CanDeriveApplicativeB
  , gbprodDefault, gbpureDefault
  )

where

import Barbies.Generics.Applicative(GApplicative(..))
import Barbies.Internal.FunctorB (FunctorB (..))

import Data.Functor.Const   (Const (..))
import Data.Functor.Constant(Constant (..))
import Data.Functor.Product (Product (..))
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))

import Data.Generics.GenericN

-- | A 'FunctorB' with application, providing operations to:
--
--     * embed an "empty" value ('bpure')
--
--     * align and combine values ('bprod')
--
--  It should satisfy the following laws:
--
--  [Naturality of 'bprod']
--
-- @
-- 'bmap' (\('Pair' a b) -> 'Pair' (f a) (g b)) (u `'bprod'` v) = 'bmap' f u `'bprod'` 'bmap' g v
-- @
--
--
--  [Left and right identity]
--
-- @
-- 'bmap' (\('Pair' _ b) -> b) ('bpure' e `'bprod'` v) = v
-- 'bmap' (\('Pair' a _) -> a) (u `'bprod'` 'bpure' e) = u
-- @
--
-- [Associativity]
--
-- @
-- 'bmap' (\('Pair' a ('Pair' b c)) -> 'Pair' ('Pair' a b) c) (u `'bprod'` (v `'bprod'` w)) = (u `'bprod'` v) `'bprod'` w
-- @
--
--  It is to 'FunctorB' in the same way as 'Applicative'
--  relates to 'Functor'. For a presentation of 'Applicative' as
--  a monoidal functor, see Section 7 of
--  <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>.
--
-- There is a default implementation of 'bprod' and 'bpure' based on 'Generic'.
-- Intuitively, it works on types where the value of `bpure` is uniquely defined.
-- This corresponds rougly to record types (in the presence of sums, there would
-- be several candidates for `bpure`), where every field is either a 'Monoid' or
-- covered by the argument @f@.
class FunctorB b => ApplicativeB (b :: (k -> Type) -> Type) where
  bpure
    :: (forall a . f a)
    -> b f

  bprod
    :: b f
    -> b g
    -> b (f `Product` g)

  default bpure
    :: CanDeriveApplicativeB b f f
    => (forall a . f a)
    -> b f
  bpure = gbpureDefault

  default bprod
    :: CanDeriveApplicativeB b f g
    => b f
    -> b
    g -> b (f `Product` g)
  bprod = gbprodDefault


-- | An alias of 'bprod', since this is like a 'zip'.
bzip
  :: ApplicativeB b
  => b f
  -> b g
  -> b (f `Product` g)
bzip = bprod

-- | An equivalent of 'unzip'.
bunzip
  :: ApplicativeB b
  => b (f `Product` g)
  -> (b f, b g)
bunzip bfg
  = (bmap (\(Pair a _) -> a) bfg, bmap (\(Pair _ b) -> b) bfg)

-- | An equivalent of 'Data.List.zipWith'.
bzipWith
  :: ApplicativeB b
  => (forall a. f a -> g a -> h a)
  -> b f
  -> b g
  -> b h
bzipWith f bf bg
  = bmap (\(Pair fa ga) -> f fa ga) (bf `bprod` bg)

-- | An equivalent of 'Data.List.zipWith3'.
bzipWith3
  :: ApplicativeB b
  => (forall a. f a -> g a -> h a -> i a)
  -> b f
  -> b g
  -> b h
  -> b i
bzipWith3 f bf bg bh
  = bmap (\(Pair (Pair fa ga) ha) -> f fa ga ha)
         (bf `bprod` bg `bprod` bh)


-- | An equivalent of 'Data.List.zipWith4'.
bzipWith4
  :: ApplicativeB b
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> b f
  -> b g
  -> b h
  -> b
  i -> b j
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
--     * Every field of @B f@ is either a monoid, or of the form @f a@, for
--       some type @a@.
type CanDeriveApplicativeB b f g
  = ( GenericP 0 (b f)
    , GenericP 0 (b g)
    , GenericP 0 (b (f `Product` g))
    , GApplicative 0 f g (RepP 0 (b f)) (RepP 0 (b g)) (RepP 0 (b (f `Product` g)))
    )


-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'bprod' based on 'Generic'.
gbprodDefault
  :: forall b f g
  .  CanDeriveApplicativeB b f g
  => b f
  -> b g
  -> b (f `Product` g)
gbprodDefault l r
  = toP p0 $ gprod p0 (Proxy @f) (Proxy @g) (fromP p0 l) (fromP p0 r)
  where
    p0 = Proxy @0
{-# INLINE gbprodDefault #-}

gbpureDefault
  :: forall b f
  .  CanDeriveApplicativeB b f f
  => (forall a . f a)
  -> b f
gbpureDefault fa
  = toP (Proxy @0) $ gpure
      (Proxy @0)
      (Proxy @f)
      (Proxy @(RepP 0 (b f)))
      (Proxy @(RepP 0 (b (f `Product` f))))
      fa
{-# INLINE gbpureDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for ApplicativeB
-- -------------------------------------------------------------

type P = Param

instance
  (  ApplicativeB b
  ) => GApplicative 0 f g (Rec (b (P 0 f)) (b f))
                          (Rec (b (P 0 g)) (b g))
                          (Rec (b (P 0 (f `Product` g))) (b (f `Product` g)))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (bpure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 bf)) (Rec (K1 bg))
    = Rec (K1 (bf `bprod` bg))
  {-# INLINE gprod #-}



instance
  ( Applicative h
  , ApplicativeB b
  ) => GApplicative 0 f g (Rec (h (b (P 0 f))) (h (b f)))
                          (Rec (h (b (P 0 g))) (h (b g)))
                          (Rec (h (b (P 0 (f `Product` g)))) (h (b (f `Product` g))))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (pure $ bpure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 hbf)) (Rec (K1 hbg))
    = Rec (K1 (bprod <$> hbf <*> hbg))
  {-# INLINE gprod #-}

-- This is the same as the previous instance, but for nested Applicatives.
instance
  ( Applicative h
  , Applicative m
  , ApplicativeB b
  ) => GApplicative 0 f g (Rec (m (h (b (P 0 f)))) (m (h (b f))))
                          (Rec (m (h (b (P 0 g)))) (m (h (b g))))
                          (Rec (m (h (b (P 0 (f `Product` g))))) (m (h (b (f `Product` g)))))
  where
  gpure _ _ _ _ x
    = Rec (K1 (pure . pure $ bpure x))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 hbf)) (Rec (K1 hbg))
    = Rec (K1 (go <$> hbf <*> hbg))
    where
      go a b = bprod <$> a <*> b
  {-# INLINE gprod #-}


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
    = Const (l `mappend` r)
  {-# INLINE bprod #-}

instance (ApplicativeB a, ApplicativeB b) => ApplicativeB (Product a b) where
  bpure x
    = Pair (bpure x) (bpure x)
  {-# INLINE bpure #-}

  bprod (Pair ll lr) (Pair rl rr)
    = Pair (bprod ll rl) (bprod lr rr)
  {-# INLINE bprod #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance Monoid a => ApplicativeB (Constant a) where
  bpure _
    = Constant mempty
  {-# INLINE bpure #-}

  bprod (Constant l) (Constant r)
    = Constant (l `mappend` r)
  {-# INLINE bprod #-}
