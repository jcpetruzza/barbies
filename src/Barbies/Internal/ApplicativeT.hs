{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806

{-# LANGUAGE QuantifiedConstraints #-}

#endif

{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.ApplicativeT
  ( ApplicativeT(tpure, tprod)
  , tzip, tunzip, tzipWith, tzipWith3, tzipWith4

  , CanDeriveApplicativeT
  , gtprodDefault, gtpureDefault
  )

where

import Barbies.Generics.Applicative(GApplicative(..))
import Barbies.Internal.FunctorT (FunctorT (..))

import Control.Applicative (Alternative(..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Reverse (Reverse (..))
import Data.Functor.Sum (Sum (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

import Data.Generics.GenericN

-- | A 'FunctorT' with application, providing operations to:
--
--     * embed an "empty" value ('tpure')
--
--     * align and combine values ('tprod')
--
--  It should satisfy the following laws:
--
--  [Naturality of 'tprod']
--
-- @
-- 'tmap' (\('Pair' a b) -> 'Pair' (f a) (g b)) (u `'tprod'` v) = 'tmap' f u `'tprod'` 'tmap' g v
-- @
--
--  [Left and right identity]
--
-- @
-- 'tmap' (\('Pair' _ b) -> b) ('tpure' e `'tprod'` v) = v
-- 'tmap' (\('Pair' a _) -> a) (u `'tprod'` 'tpure' e) = u
-- @
--
-- [Associativity]
--
-- @
-- 'tmap' (\('Pair' a ('Pair' b c)) -> 'Pair' ('Pair' a b) c) (u `'tprod'` (v `'tprod'` w)) = (u `'tprod'` v) `'tprod'` w
-- @
--
--  It is to 'FunctorT' in the same way is 'Applicative'
--  relates to 'Functor'. For a presentation of 'Applicative' as
--  a monoidal functor, see Section 7 of
--  <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>.
--
-- There is a default implementation of 'tprod' and 'tpure' based on 'Generic'.
-- Intuitively, it works on types where the value of `tpure` is uniquely defined.
-- This corresponds rougly to record types (in the presence of sums, there would
-- be several candidates for `tpure`), where every field is either a 'Monoid' or
-- covered by the argument @f@.
class FunctorT t => ApplicativeT (t :: (k -> Type) -> (k' -> Type)) where
  tpure
    :: (forall a . f a)
    -> t f x

  tprod
    :: t f x
    -> t g x
    -> t (f `Product` g) x

  default tpure
    :: CanDeriveApplicativeT t f f x
    => (forall a . f a)
    -> t f x
  tpure = gtpureDefault

  default tprod
    :: CanDeriveApplicativeT t f g x
    => t f x
    -> t g x
    -> t (f `Product` g) x
  tprod = gtprodDefault


-- | An alias of 'tprod'.
tzip
  :: ApplicativeT t
  => t f x
  -> t g x
  -> t (f `Product` g) x
tzip = tprod

-- | An equivalent of 'unzip'.
tunzip
  :: ApplicativeT t
  => t (f `Product` g) x
  -> (t f x, t g x)
tunzip tfg
  = (tmap (\(Pair a _) -> a) tfg, tmap (\(Pair _ b) -> b) tfg)

-- | An equivalent of 'Data.List.zipWith'.
tzipWith
  :: ApplicativeT t
  => (forall a. f a -> g a -> h a)
  -> t f x
  -> t g x
  -> t h x
tzipWith f tf tg
  = tmap (\(Pair fa ga) -> f fa ga) (tf `tprod` tg)

-- | An equivalent of 'Data.List.zipWith3'.
tzipWith3
  :: ApplicativeT t
  => (forall a. f a -> g a -> h a -> i a)
  -> t f x
  -> t g x
  -> t h x
  -> t i x
tzipWith3 f tf tg th
  = tmap (\(Pair (Pair fa ga) ha) -> f fa ga ha)
         (tf `tprod` tg `tprod` th)


-- | An equivalent of 'Data.List.zipWith4'.
tzipWith4
  :: ApplicativeT t
  => (forall a. f a -> g a -> h a -> i a -> j a)
  -> t f x
  -> t g x
  -> t h x
  -> t i x
  -> t j x
tzipWith4 f tf tg th ti
  = tmap (\(Pair (Pair (Pair fa ga) ha) ia) -> f fa ga ha ia)
         (tf `tprod` tg `tprod` th `tprod` ti)


-- | @'CanDeriveApplicativeT' T f g x@ is in practice a predicate about @T@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (T f)@.
--
--     * @T@ has only one constructor (that is, it is not a sum-type).
--
--     * Every field of @T f x@ is either a monoid, or of the form @f a@, for
--       some type @a@.
type CanDeriveApplicativeT t f g x
  = ( GenericP 1 (t f x)
    , GenericP 1 (t g x)
    , GenericP 1 (t (f `Product` g) x)
    , GApplicative 1 f g (RepP 1 (t f x)) (RepP 1 (t g x)) (RepP 1 (t (f `Product` g) x))
    )


-- ======================================
-- Generic derivation of instances
-- ======================================

-- | Default implementation of 'tprod' based on 'Generic'.
gtprodDefault
  :: forall t f g x
  .  CanDeriveApplicativeT t f g x
  => t f x
  -> t g x
  -> t (f `Product` g) x
gtprodDefault l r
  = toP p1 $ gprod p1 (Proxy @f) (Proxy @g) (fromP p1 l) (fromP p1 r)
  where
      p1 = Proxy @1
{-# INLINE gtprodDefault #-}

gtpureDefault
  :: forall t f x
  .  CanDeriveApplicativeT t f f x
  => (forall a . f a)
  -> t f x
gtpureDefault fa
  = toP (Proxy @1) $ gpure
      (Proxy @1)
      (Proxy @f)
      (Proxy @(RepP 1 (t f x)))
      (Proxy @(RepP 1 (t (f `Product` f) x)))
      fa
{-# INLINE gtpureDefault #-}


-- ------------------------------------------------------------
-- Generic derivation: Special cases for ApplicativeT
-- -------------------------------------------------------------

type P = Param

instance
  (  ApplicativeT t
  ) => GApplicative 1 f g (Rec (t (P 1 f) x) (t f x))
                          (Rec (t (P 1 g) x) (t g x))
                          (Rec (t (P 1 (f `Product` g)) x) (t (f `Product` g) x))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (tpure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 tf)) (Rec (K1 tg))
    = Rec (K1 (tf `tprod` tg))
  {-# INLINE gprod #-}



instance
  ( Applicative h
  , ApplicativeT t
  ) => GApplicative 1 f g (Rec (h (t (P 1 f) x)) (h (t f x)))
                          (Rec (h (t (P 1 g) x)) (h (t g x)))
                          (Rec (h (t (P 1 (f `Product` g)) x)) (h (t (f `Product` g) x)))
  where
  gpure _ _ _ _ fa
    = Rec (K1 (pure $ tpure fa))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 htf)) (Rec (K1 htg))
    = Rec (K1 (tprod <$> htf <*> htg))
  {-# INLINE gprod #-}


-- This is the same as the previous instance, but for nested Applicatives.
instance
  ( Applicative h
  , Applicative m
  , ApplicativeT t
  ) => GApplicative 1 f g (Rec (m (h (t (P 1 f) x))) (m (h (t f x))))
                          (Rec (m (h (t (P 1 g) x))) (m (h (t g x))))
                          (Rec (m (h (t (P 1 (f `Product` g)) x))) (m (h (t (f `Product` g) x))))
  where
  gpure _ _ _ _ x
    = Rec (K1 (pure . pure $ tpure x))
  {-# INLINE gpure #-}

  gprod _ _ _ (Rec (K1 htfx)) (Rec (K1 htgx))
    = Rec (K1 (go <$> htfx <*> htgx))
    where
      go a b = tprod <$> a <*> b
  {-# INLINE gprod #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance Applicative f => ApplicativeT (Compose f) where
  tpure fa
    = Compose (pure fa)
  {-# INLINE tpure #-}

  tprod (Compose fga) (Compose fha)
    = Compose (Pair <$> fga <*> fha)
  {-# INLINE tprod #-}

instance ApplicativeT Reverse where
  tpure fa
    = Reverse fa
  {-# INLINE tpure #-}

  tprod (Reverse fa) (Reverse ga)
    = Reverse (Pair fa ga)
  {-# INLINE tprod #-}


instance Alternative f => ApplicativeT (Product f) where
  tpure fa
    = Pair empty fa
  {-# INLINE tpure #-}

  tprod (Pair fl gl) (Pair fr gr)
    = Pair (fl <|> fr) (Pair gl gr)
  {-# INLINE tprod #-}

instance Alternative f => ApplicativeT (Sum f) where
  tpure fa
    = InR fa
  {-# INLINE tpure #-}

  tprod l r
    = case (l, r) of
        (InR gl, InR gr) -> InR (Pair gl gr)
        (InR _,  InL fr) -> InL fr
        (InL fl, InR _)  -> InL fl
        (InL fl, InL fr) -> InL (fl <|> fr)
  {-# INLINE tprod #-}
