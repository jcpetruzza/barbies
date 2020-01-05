{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Barbies.Generics.Functor
  ( GFunctor(..)
  )

where

import Data.Generics.GenericN
import Data.Proxy (Proxy (..))

import GHC.TypeLits (Nat)

class GFunctor (n :: Nat) f g repbf repbg where
  gmap :: Proxy n -> (forall a . f a -> g a) -> repbf x -> repbg x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance
  ( GFunctor n f g bf bg
  ) => GFunctor n f g (M1 i c bf) (M1 i c bg)
  where
  gmap pn h = M1 . gmap pn h . unM1
  {-# INLINE gmap #-}


instance GFunctor n f g V1 V1 where
  gmap _ _ _ = undefined


instance GFunctor n f g U1 U1 where
  gmap _ _ = id
  {-# INLINE gmap #-}


instance
  ( GFunctor n f g l l'
  , GFunctor n f g r r'
  )
  => GFunctor n f g (l :*: r) (l' :*: r')
  where
  gmap pn h (l :*: r) = (gmap pn h l) :*: gmap pn h r
  {-# INLINE gmap #-}


instance
  ( GFunctor n f g l l'
  , GFunctor n f g r r'
  ) => GFunctor n f g (l :+: r) (l' :+: r')
  where
  gmap pn h = \case
    L1 l -> L1 (gmap pn h l)
    R1 r -> R1 (gmap pn h r)
  {-# INLINE gmap #-}


-- ---------------------------------------------------------
-- The interesting cases.
-- There are more interesting cases for specific values of n
-- ---------------------------------------------------------

type P = Param

-- {{ Functor application ------------------------------------
instance
  GFunctor n f g (Rec (P n f a') (f a))
                 (Rec (P n g a') (g a))
  where
  gmap _ h (Rec (K1 fa)) = Rec (K1 (h fa))
  {-# INLINE gmap #-}

instance
  ( Functor h
  ) =>
  GFunctor n f g (Rec (h (P n f a')) (h (f a)))
                 (Rec (h (P n g a')) (h (g a)))
  where
  gmap _ h (Rec (K1 hfa)) = Rec (K1 (h <$> hfa))
  {-# INLINE gmap #-}
-- }} Functor application ------------------------------------


-- {{ Not a functor application --------------------------
instance
  GFunctor n f g (Rec x x) (Rec x x)
  where
  gmap _ _ = id
  {-# INLINE gmap #-}
-- }} Not a functor application --------------------------
