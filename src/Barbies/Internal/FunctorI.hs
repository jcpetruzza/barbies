{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.FunctorI
  ( FunctorI(..)
  , gimapDefault
  , CanDeriveFunctorI
  )

where

import Barbies.Generics.Functor (GFunctor(..))

import Data.Functor.Product   (Product (..))
import Data.Functor.Sum       (Sum (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Kind              (Type)

-- | Functor from indexed-types to indexed-types. Instances of 'FunctorI' should
--   satisfy the following laws:
--
-- @
--   'imap' 'id' = 'id'
--   'imap' f . 'imap' g = 'imap' (f . g)
-- @
--
-- There is a default 'imap' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorI (b :: (k -> Type) -> k' -> Type) where
  imap :: (forall a . f a -> g a) -> (forall x. b f x -> b g x)

  default imap
    :: forall f g x
    .  CanDeriveFunctorI b f g x
    => (forall a . f a -> g a)
    -> b f x -> b g x
  imap = gimapDefault

-- | @'CanDeriveFunctorI' B f g x@ is in practice a predicate about @B@ only.
--   Intuitively, it says that the following holds, for any arbitrary @f@:
--
--     * There is an instance of @'Generic' (B f)@.
--
--     * @B f x@ can contain fields of type @b f y@ as long as there exists a
--       @'FunctorI' b@ instance. In particular, recursive usages of @B f y@
--       are allowed.
--
--     * @B f x@ can also contain usages of @b f y@ under a @'Functor' h@.
--       For example, one could use @'Maybe' (B f y)@ when defining @B f y@.
type CanDeriveFunctorI b f g x
  = ( GenericN (b f x)
    , GenericN (b g x)
    , GFunctor 1 f g (RepN (b f x)) (RepN (b g x))
    )

-- | Default implementation of 'imap' based on 'Generic'.
gimapDefault
  :: CanDeriveFunctorI b f g x
  => (forall a . f a -> g a)
  -> b f x -> b g x
gimapDefault f
  = toN . gmap (Proxy @1) f . fromN
{-# INLINE gimapDefault #-}

-- ------------------------------------------------------------
-- Generic derivation: Special cases for FunctorI
-- -----------------------------------------------------------

type P = Param

-- b' is b, maybe with 'Param' annotations
instance
  ( FunctorI b
  ) => GFunctor 1 f g (Rec (b' (P 1 f) (P 0 x)) (b f x))
                      (Rec (b' (P 1 g) (P 0 x)) (b g x))
  where
  gmap _ h (Rec (K1 bf)) = Rec (K1 (imap h bf))
  {-# INLINE gmap #-}

-- b' and h' are b and h, maybe with 'Param' annotations
instance
  ( Functor h
  , FunctorI b
  ) => GFunctor 1 f g (Rec (h' (b' (P 1 f) (P 0 x))) (h (b f x)))
                      (Rec (h' (b' (P 1 g) (P 0 x))) (h (b g x)))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (imap h) hbf))
  {-# INLINE gmap #-}


-- This is the same as the previous instance, but for nested (normal-flavoured)
-- functors.
instance
  ( Functor h
  , Functor m
  , FunctorI b
  ) => GFunctor 1 f g (Rec (m' (h' (b' (P 1 f) (P 0 x)))) (m (h (b f x))))
                      (Rec (m' (h' (b' (P 1 g) (P 0 x)))) (m (h (b g x))))
  where
  gmap _ h (Rec (K1 hbf)) = Rec (K1 (fmap (fmap (imap h)) hbf))
  {-# INLINE gmap #-}


-- --------------------------------
-- Instances for base types
-- --------------------------------

instance FunctorI (Product f) where
  imap h (Pair fa ga) = Pair fa (h ga)
  {-# INLINE imap #-}

instance FunctorI (Sum f) where
  imap h = \case
    InL fa -> InL fa
    InR ga -> InR (h ga)
  {-# INLINE imap #-}
