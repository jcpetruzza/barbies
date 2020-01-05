{-# LANGUAGE PolyKinds #-}
module Barbies.Generics.Traversable
  ( GTraversable(..)
  )

where

import Data.Generics.GenericN
import Data.Proxy (Proxy (..))

class GTraversable n f g repbf repbg where
  gtraverse
    :: Applicative t
    => Proxy n
    -> (forall a . f a -> t (g a))
    -> repbf x
    -> t (repbg x)

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance
  ( GTraversable n f g bf bg
  ) => GTraversable n f g (M1 i c bf) (M1 i c bg)
  where
  gtraverse pn h
    = fmap M1 . gtraverse pn h . unM1
  {-# INLINE gtraverse #-}

instance GTraversable n f g V1 V1 where
  gtraverse _ _ _ = undefined
  {-# INLINE gtraverse #-}

instance GTraversable n f g U1 U1 where
  gtraverse _ _ = pure
  {-# INLINE gtraverse #-}

instance
  ( GTraversable n f g l l'
  , GTraversable n f g r r'
  ) => GTraversable n f g (l :*: r) (l' :*: r')
  where
  gtraverse pn h (l :*: r)
    = (:*:) <$> gtraverse pn h l <*> gtraverse pn h r
  {-# INLINE gtraverse #-}

instance
  ( GTraversable n f g l l'
  , GTraversable n f g r r'
  ) => GTraversable n f g (l :+: r) (l' :+: r')
  where
  gtraverse pn h = \case
    L1 l -> L1 <$> gtraverse pn h l
    R1 r -> R1 <$> gtraverse pn h r
  {-# INLINE gtraverse #-}

-- --------------------------------
-- The interesting cases
-- --------------------------------

type P = Param

-- {{ Functor application ------------------------------------------------------
instance
  GTraversable n f g (Rec (P n f a') (f a))
                     (Rec (P n g a') (g a))
  where
  gtraverse _ h
    = fmap (Rec . K1) . h . unK1 . unRec
  {-# INLINE gtraverse #-}


instance
  ( Traversable h
  ) =>
  GTraversable n f g (Rec (h (P n f a)) (h (f a)))
                     (Rec (h (P n g a)) (h (g a)))
  where
  gtraverse _ h
    = fmap (Rec . K1) . traverse h . unK1 . unRec
  {-# INLINE gtraverse #-}
-- }} Functor application ------------------------------------------------------


-- {{ Not a functor application -----------------------------------------------
instance GTraversable n f g (Rec a a) (Rec a a) where
  gtraverse _ _ = pure
  {-# INLINE gtraverse #-}
-- }} Not a functor application -----------------------------------------------
