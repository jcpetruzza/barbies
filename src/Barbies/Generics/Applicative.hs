{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
module Barbies.Generics.Applicative
  ( GApplicative(..)
  )

where


import Data.Functor.Product(Product(..))
import Data.Proxy(Proxy (..))

import Data.Generics.GenericN


class GApplicative n (f :: k -> *) (g :: k -> *) repbf repbg repbfg where
  gprod
    :: Proxy n
    -> Proxy f
    -> Proxy g
    -> repbf x
    -> repbg x
    -> repbfg x

  gpure
    :: (f ~ g, repbf ~ repbg)
    => Proxy n
    -> Proxy f
    -> Proxy repbf
    -> Proxy repbfg
    -> (forall a . f a)
    -> repbf x

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance
  ( GApplicative n f g repf repg repfg
  ) => GApplicative n f g (M1 i c repf)
                          (M1 i c repg)
                          (M1 i c repfg)
  where
  gprod pn pf pg (M1 l) (M1 r)
    = M1 (gprod pn pf pg l r)
  {-# INLINE gprod #-}

  gpure pn pf _ _ x
    = M1 (gpure pn pf (Proxy @repf) (Proxy @repfg) x)
  {-# INLINE gpure #-}


instance GApplicative n f g U1 U1 U1 where
  gprod _ _ _ U1 U1 = U1
  {-# INLINE gprod #-}

  gpure _ _ _ _ _ = U1
  {-# INLINE gpure #-}


instance
  ( GApplicative n f g lf lg lfg
  , GApplicative n f g rf rg rfg
  ) => GApplicative n f g (lf  :*: rf)
                          (lg  :*: rg)
                          (lfg :*: rfg) where
  gprod pn pf pg (l1 :*: l2) (r1 :*: r2)
    = (l1 `lprod` r1) :*: (l2 `rprod` r2)
    where
      lprod = gprod pn pf pg
      rprod = gprod pn pf pg
  {-# INLINE gprod #-}

  gpure pn pf _ _ x
    =   gpure pn pf (Proxy @lf) (Proxy @lfg) x
    :*: gpure pn pf (Proxy @rf) (Proxy @rfg) x
  {-# INLINE gpure #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

type P = Param

instance GApplicative n f g (Rec (P n f a_or_pma) (f a))
                            (Rec (P n g a_or_pma) (g a))
                            (Rec (P n (f `Product` g) a_or_pma) ((f `Product` g) a))
  where
  gprod _ _ _ (Rec (K1 fa)) (Rec (K1 ga))
    = Rec (K1 (Pair fa ga))
  {-# INLINE gprod #-}

  gpure _ _ _ _ x
    = Rec (K1 x)
  {-# INLINE gpure #-}
