{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 806

{-# LANGUAGE QuantifiedConstraints #-}

#endif

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Barbies.Bi
  ( FunctorBI
  , bimap
  , bimap1

  , TraversableBI
  , bitraverse
  , bitraverse1

  , Flip(..)
  ) where

import Barbies.Internal.ApplicativeB(ApplicativeB(..))
import Barbies.Internal.ApplicativeI(ApplicativeI(..))
import Barbies.Internal.FunctorB(FunctorB(..))
import Barbies.Internal.FunctorI(FunctorI(..))
import Barbies.Internal.TraversableB(TraversableB(..))
import Barbies.Internal.TraversableI(TraversableI(..))

import Control.Monad ((>=>))

-- {{ Functor -----------------------------------------------------------------

-- | A 'FunctorB' and a 'FunctorI' together give us a bifunctor.
--   One may wonder why we mention @f@ in @'FunctorBI' b f@. Two
--   reasons: first, to avoid the need for @QuantifiedConstraints@,
--   but more importantly, because @'FunctorB' (b f)@ may require
--   a @'Functor' f@ instance, as will usually happen with nested
--   types.
class
  ( FunctorB (b f)
  , FunctorI b
  ) => FunctorBI b f

instance
  ( FunctorB (b f)
  , FunctorI b
  ) => FunctorBI b f

bimap
  :: FunctorBI b f
  => (forall a . f a -> f' a)
  -> (forall a . g a -> g' a)
  -> b f g
  -> b f' g'
bimap hf hg
  = imap hf . bmap hg
{-# INLINE bimap #-}

-- | 'bimap' specialized to a single functor argument.
bimap1
  :: FunctorBI b f
  => (forall a . f a -> g a)
  -> b f f
  -> b g g
bimap1 h
  = bimap h h
{-# INLINE bimap1 #-}

-- }} Functor -----------------------------------------------------------------


-- {{ Traversable -------------------------------------------------------------

-- | A 'TraversableB' and a 'TraversableI' together give us a bitraversable
class
  ( TraversableB (b f)
  , TraversableI b
  ) => TraversableBI b f

instance
  ( TraversableB (b f)
  , TraversableI b
  ) => TraversableBI b f

bitraverse
  :: ( TraversableBI b f
     , Monad t
     )
  => (forall a . f a -> t (f' a))
  -> (forall a . g a -> t (g' a))
  -> b f g
  -> t (b f' g')
bitraverse hf hg
  = btraverse hg >=> itraverse hf
{-# INLINE bitraverse #-}


bitraverse1
  :: ( TraversableBI b f
     , Monad t
     )
  => (forall a . f a -> t (g a))
  -> b f f
  -> t (b g g)
bitraverse1 h
  = bitraverse h h
{-# INLINE bitraverse1 #-}


-- }} Traversable -------------------------------------------------------------


-- | Convert a 'FunctorB' into a 'FunctorI' and vice-versa.
newtype Flip b l r
  = Flip { runFlip :: b r l }
  deriving (Eq, Ord, Read, Show)


instance FunctorI b => FunctorB (Flip b f) where
  bmap h (Flip bfx)
    = Flip (imap h bfx)
  {-# INLINE bmap #-}


instance TraversableI b => TraversableB (Flip b f) where
  btraverse h (Flip bfx)
    = Flip <$> itraverse h bfx
  {-# INLINE btraverse #-}


instance ApplicativeI b => ApplicativeB (Flip b f) where
  bpure fa
    = Flip (ipure fa)
  {-# INLINE bpure #-}

  bprod (Flip bfx) (Flip bgx)
    = Flip (iprod bfx bgx)
  {-# INLINE bprod #-}


#if __GLASGOW_HASKELL__ >= 806
-- ** The following instances require QuantifiedConstraints ** --

instance (forall f. FunctorB (b f)) => FunctorI (Flip b) where
  imap h (Flip bxf)
    = Flip (bmap h bxf)
  {-# INLINE imap #-}

instance (forall f. TraversableB (b f)) => TraversableI (Flip b) where
  itraverse h (Flip bxf)
    = Flip <$> btraverse h bxf
  {-# INLINE itraverse #-}


instance (forall f. ApplicativeB (b f)) => ApplicativeI (Flip b) where
  ipure fa
    = Flip (bpure fa)
  {-# INLINE ipure #-}

  iprod (Flip bxf) (Flip bxg)
    = Flip (bprod bxf bxg)
  {-# INLINE iprod #-}
#endif
