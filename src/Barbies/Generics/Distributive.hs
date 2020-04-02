{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Barbies.Generics.Distributive
  ( GDistributive(..)
  )

where

import Data.Generics.GenericN
import Data.Proxy (Proxy (..))

import Data.Functor.Compose   (Compose (..))
import Data.Distributive      (Distributive(..))

import GHC.TypeLits (Nat)

class (Functor f) => GDistributive (n :: Nat) f repbg repbfg where
  gdistribute :: Proxy n -> f (repbg x) -> repbfg x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance
  ( GDistributive n f bg bfg
  ) => GDistributive n f (M1 i c bg) (M1 i c bfg)
  where
  gdistribute pn = M1 . gdistribute pn . fmap unM1
  {-# INLINE gdistribute #-}


instance
  ( Functor f
  ) => GDistributive n f U1 U1
  where
  gdistribute _ = const U1
  {-# INLINE gdistribute #-}


fstF :: (l :*: r) a -> l a
fstF (x :*: _y) = x

sndF :: (l :*: r) a -> r a
sndF (_x :*: y) = y

instance
  ( GDistributive n f l l'
  , GDistributive n f r r'
  )
  => GDistributive n f (l :*: r) (l' :*: r')
  where
  gdistribute pn lr = gdistribute pn (fstF <$> lr) :*: gdistribute pn (sndF <$> lr)
  {-# INLINE gdistribute #-}


-- ---------------------------------------------------------
-- The interesting cases.
-- There are more interesting cases for specific values of n
-- ---------------------------------------------------------

type P = Param

instance
  ( Functor f
  ) =>
  GDistributive n f (Rec (P n g a) (g a)) (Rec (P n (Compose f g) a) (Compose f g a))
  where
  gdistribute _ = Rec . K1 . Compose . id . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}

instance
  ( Functor f
  , Distributive h
  ) =>
  GDistributive n f (Rec (h (P n g a)) (h (g a))) (Rec (h (P n (Compose f g) a)) (h (Compose f g a)))
  where
  gdistribute _ = Rec . K1 . fmap Compose . distribute . fmap (unK1 . unRec)
  {-# INLINE gdistribute #-}
