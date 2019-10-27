{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

#if __GLASGOW_HASKELL__ >= 806

{-# LANGUAGE QuantifiedConstraints #-}

#endif

{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.ApplicativeI
  ( ApplicativeI(ipure, iprod)
  , izip, iunzip, izipWith, izipWith3, izipWith4

  , CanDeriveApplicativeI
  , giprodDefault, gipureDefault
  )

where

import Barbies.Generics.Applicative(GApplicative(..))
import Barbies.Internal.FunctorI (FunctorI (..))

import Data.Functor.Product (Product (..))
import Data.Functor.Reverse (Reverse (..))
import Data.Functor.Sum (Sum (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Data.Generics.GenericN

-- | A 'FunctorI' with application, providing operations to:
--
--     * embed an "empty" value ('ipure')
--
--     * align and combine values ('iprod')
--
--  It should satisfy the following laws:
--
--  [Naturality of 'iprod']
--
--  @
--  'imap' (\('Pair' a b) -> 'Pair' (f a) (g b)) (u `'iprod'` v) = 'imap' f u `'iprod'` 'imap' g v
--  @
--
--  [Left and right identity]
--
--  @
--  'imap' (\('Pair' _ b) -> b) ('ipure' e `'iprod'` v) = v
--  'imap' (\('Pair' a _) -> a) (u `'iprod'` 'ipure' e) = u
--  @
--
-- [Associativity]
--
--  @
--  'imap' (\('Pair' a ('Pair' b c)) -> 'Pair' ('Pair' a b) c) (u `'iprod'` (v `'iprod'` w)) = (u `'iprod'` v) `'iprod'` w
--  @
--
--  It is to 'FunctorI' in the same way is 'Applicative'
--  relates to 'Functor'. For a presentation of 'Applicative' as
--  a monoidal functor, see Section 7 of
--  <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>.
--
-- There is a default implementation of 'iprod' and 'ipure' based on 'Generic'.
-- Intuitively, it works on types where the value of `ipure` is uniquely defined.
-- This corresponds rougly to record types (in the presence of sums, there would
-- be several candidates for `ipure`), where every field is either a 'Monoid' or
-- covered by the argument @f@.
class FunctorI b => ApplicativeI (b :: (k -> Type) -> (k' -> Type)) where
  ipure
    :: (forall a . f a)
    -> (forall x . b f x)

  iprod
    :: b f x
    -> b g x
    -> b (f `Product` g) x

  default ipure
    :: CanDeriveApplicativeI b f f x
    => (forall a . f a)
    -> b f x
  ipure = gipureDefault

  default iprod
    :: CanDeriveApplicativeI b f g x
    => b f x
    -> b g x
    -> b (f `Product` g) x
  iprod = giprodDefault


-- | An alias of 'iprod'.
izip
  :: ApplicativeI b
  => b f x
  -> b g x
  -> b (f `Product` g) x
izip = iprod

-- | An equivalent of 'unzip'.
iunzip
  :: ApplicativeI b
  => b (f `Product` g) x
  -> (b f x, b g x)
iunzip bfg
  = (imap (\(Pair a _) -> a) bfg, imap (\(Pair _ b) -> b) bfg)

-- | An equivalent of 'Data.List.zipWith'.
izipWith
  :: ApplicativeI b
  => (forall a. f a -> g a -> h a)
  -> b f x
  -> b g x
  -> b h x
izipWith f bf bg
  = imap (\(Pair fa ga) -> f fa ga) (bf `iprod` bg)

-- | An equivalent of 'Data.List.zipWith3'.
izipWith3
  :: ApplicativeI b
  => (forall a. f a -> g a -> h a -> i a)
  -> b f x
  -> b g x
  -> b h x
  -> b i x
izipWith3 f bf bg bh
  = imap (\(Pair (Pair fa ga) ha) -> f fa ga ha)
         (bf `iprod` bg `iprod` bh)


-- | An equivalent of 'Data.List.zipWith4'.
izipWith4
  :: ApplicativeI b
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> b f x
  -> b g x
  -> b h x
  -> b i x
  -> b j x
izipWith4 f bf bg bh bi
  = imap (\(Pair (Pair (Pair fa ga) ha) ia) -> f fa ga ha ia)
         (bf `iprod` bg `iprod` bh `iprod` bi)


-- | @'CanDeriveApplicativeI' B f g x@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B@ has only one constructor (that is, it is not a sum-type).
--
--     * Every field of @B f x@ is either a monoid, or of the form @f a@, for
--       some type @a@.
type CanDeriveApplicativeI b f g x
  = ( GenericN (b f x)
    , GenericN (b g x)
    , GenericN (b (f `Product` g) x)
    , GApplicative 1 f g (RepN (b f x)) (RepN (b g x)) (RepN (b (f `Product` g) x))
    )


-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'iprod' based on 'Generic'.
giprodDefault
  :: forall b f g x
  .  CanDeriveApplicativeI b f g x
  => b f x
  -> b g x
  -> b (f `Product` g) x
giprodDefault l r
  = toN $ gprod (Proxy @1) (Proxy @f) (Proxy @g) (fromN l) (fromN r)
{-# INLINE giprodDefault #-}

gipureDefault
  :: forall b f x
  .  CanDeriveApplicativeI b f f x
  => (forall a . f a)
  -> b f x
gipureDefault fa
  = toN $ gpure
      (Proxy @1)
      (Proxy @f)
      (Proxy @(RepN (b f x)))
      (Proxy @(RepN (b (f `Product` f) x)))
      fa
{-# INLINE gipureDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for ApplicativeI
-- -------------------------------------------------------------

type P = Param

-- b' is b, maybe with 'Param' annotations
instance
  (  ApplicativeI b
  ) => GApplicative 1 f g (Rec (b' (P 1 f) (P 0 x)) (b f x))
                          (Rec (b' (P 1 g) (P 0 x)) (b g x))
                          (Rec (b' (P 1 (f `Product` g)) (P 0 x)) (b (f `Product` g) x))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (ipure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 bf)) (Rec (K1 bg))
    = Rec (K1 (bf `iprod` bg))
  {-# INLINE gprod #-}



-- h' and b' are essentially  h and b, but maybe
-- with 'Param' annotations
instance
  ( Applicative h
  , ApplicativeI b
  ) => GApplicative 1 f g (Rec (h' (b' (P 1 f) (P 0 x))) (h (b f x)))
                          (Rec (h' (b' (P 1 g) (P 0 x))) (h (b g x)))
                          (Rec (h' (b' (P 1 (f `Product` g)) (P 0 x))) (h (b (f `Product` g) x)))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (pure $ ipure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 hbf)) (Rec (K1 hbg))
    = Rec (K1 (iprod <$> hbf <*> hbg))
  {-# INLINE gprod #-}


-- This is the same as the previous instance, but for nested Applicatives.
instance
  ( Applicative h
  , Applicative m
  , ApplicativeI b
  ) => GApplicative 1 f g (Rec (m' (h' (b' (P 1 f) (P 0 x)))) (m (h (b f x))))
                          (Rec (m' (h' (b' (P 1 g) (P 0 x)))) (m (h (b g x))))
                          (Rec (m' (h' (b' (P 1 (f `Product` g)) (P 0 x)))) (m (h (b (f `Product` g) x))))
  where
  gpure _ _ _ _ x
    = Rec (K1 (pure . pure $ ipure x))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 hbfx)) (Rec (K1 hbgx))
    = Rec (K1 (go <$> hbfx <*> hbgx))
    where
      go a b = iprod <$> a <*> b
  {-# INLINE gprod #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance ApplicativeI Reverse where
  ipure fa
    = Reverse fa
  {-# INLINE ipure #-}

  iprod (Reverse fa) (Reverse ga)
    = Reverse (Pair fa ga)
  {-# INLINE iprod #-}

#if __GLASGOW_HASKELL__ >= 806

instance (forall a . Monoid (f a)) => ApplicativeI (Product f) where
  ipure fa
    = Pair mempty fa
  {-# INLINE ipure #-}

  iprod (Pair fl gl) (Pair fr gr)
    = Pair (fl `mappend` fr) (Pair gl gr)
  {-# INLINE iprod #-}


instance (forall a . Semigroup (f a)) => ApplicativeI (Sum f) where
  ipure fa
    = InR fa
  {-# INLINE ipure #-}

  iprod l r
    = case (l, r) of
        (InR gl, InR gr) -> InR (Pair gl gr)
        (InR _,  InL fr) -> InL fr
        (InL fl, InR _)  -> InL fl
        (InL fl, InL fr) -> InL (fl <> fr)
  {-# INLINE iprod #-}
#endif
