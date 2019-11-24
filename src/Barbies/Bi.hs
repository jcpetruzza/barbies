{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806

{-# LANGUAGE QuantifiedConstraints #-}

#endif

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Barbies.Bi
  ( -- * Functor
    -- | A bifunctor is simultaneously a 'FunctorT' and a 'FunctorB'.
    btmap
  , btmap1

    -- * Traversable
    -- | A traversable bifunctor is simultaneously a 'TraversableT
    --   and a 'TraversableB'
  , bttraverse
  , bttraverse1

    -- * Wrappers
  , Flip(..)
  ) where

import Barbies.Internal.ApplicativeB(ApplicativeB(..))
import Barbies.Internal.ApplicativeT(ApplicativeT(..))
import Barbies.Internal.FunctorB(FunctorB(..))
import Barbies.Internal.FunctorT(FunctorT(..))
import Barbies.Internal.TraversableB(TraversableB(..))
import Barbies.Internal.TraversableT(TraversableT(..))

import Control.Monad ((>=>))

-- {{ Functor -----------------------------------------------------------------

-- | Map over both arguments at the same time.
btmap
  :: ( FunctorB (b f)
     , FunctorT b
     )
  => (forall a . f a -> f' a)
  -> (forall a . g a -> g' a)
  -> b f g
  -> b f' g'
btmap hf hg
  = tmap hf . bmap hg
{-# INLINE btmap #-}

-- | A version of 'btmap' specialized to a single argument.
btmap1
  :: ( FunctorB (b f)
     , FunctorT b
     )
  => (forall a . f a -> g a)
  -> b f f
  -> b g g
btmap1 h
  = btmap h h
{-# INLINE btmap1 #-}

-- }} Functor -----------------------------------------------------------------


-- {{ Traversable -------------------------------------------------------------

-- | Traverse over both arguments, first over @f@, then over @g@..
bttraverse
  :: ( TraversableB (b f)
     , TraversableT b
     , Monad t
     )
  => (forall a . f a -> t (f' a))
  -> (forall a . g a -> t (g' a))
  -> b f g
  -> t (b f' g')
bttraverse hf hg
  = btraverse hg >=> ttraverse hf
{-# INLINE bttraverse #-}

-- | A version of 'bttraverse' specialized to a single argument.
bttraverse1
  :: ( TraversableB (b f)
     , TraversableT b
     , Monad t
     )
  => (forall a . f a -> t (g a))
  -> b f f
  -> t (b g g)
bttraverse1 h
  = bttraverse h h
{-# INLINE bttraverse1 #-}


-- }} Traversable -------------------------------------------------------------


-- | Convert a 'FunctorB' into a 'FunctorT' and vice-versa.
newtype Flip b l r
  = Flip { runFlip :: b r l }
  deriving (Eq, Ord, Read, Show)


instance FunctorT b => FunctorB (Flip b f) where
  bmap h (Flip bfx)
    = Flip (tmap h bfx)
  {-# INLINE bmap #-}


instance TraversableT b => TraversableB (Flip b f) where
  btraverse h (Flip bfx)
    = Flip <$> ttraverse h bfx
  {-# INLINE btraverse #-}


instance ApplicativeT b => ApplicativeB (Flip b f) where
  bpure fa
    = Flip (tpure fa)
  {-# INLINE bpure #-}

  bprod (Flip bfx) (Flip bgx)
    = Flip (tprod bfx bgx)
  {-# INLINE bprod #-}


#if __GLASGOW_HASKELL__ >= 806
-- ** The following instances require QuantifiedConstraints ** --

instance (forall f. FunctorB (b f)) => FunctorT (Flip b) where
  tmap h (Flip bxf)
    = Flip (bmap h bxf)
  {-# INLINE tmap #-}

instance (forall f. TraversableB (b f)) => TraversableT (Flip b) where
  ttraverse h (Flip bxf)
    = Flip <$> btraverse h bxf
  {-# INLINE ttraverse #-}


instance (forall f. ApplicativeB (b f)) => ApplicativeT (Flip b) where
  tpure fa
    = Flip (bpure fa)
  {-# INLINE tpure #-}

  tprod (Flip bxf) (Flip bxg)
    = Flip (bprod bxf bxg)
  {-# INLINE tprod #-}
#endif
