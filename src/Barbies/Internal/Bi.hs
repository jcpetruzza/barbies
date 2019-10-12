{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
module Barbies.Internal.Bi
  ( Flip(..)
  ) where

import Barbies.Internal.ApplicativeB(ApplicativeB(..))
import Barbies.Internal.ApplicativeI(ApplicativeI(..))
import Barbies.Internal.FunctorB(FunctorB(..))
import Barbies.Internal.FunctorI(FunctorI(..))
import Barbies.Internal.TraversableB(TraversableB(..))
import Barbies.Internal.TraversableI(TraversableI(..))

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
