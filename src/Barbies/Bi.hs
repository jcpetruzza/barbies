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
    -- | A traversable bifunctor is simultaneously a 'TraversableT'
    --   and a 'TraversableB'.
  , bttraverse
  , bttraverse1
  , btfor1
  , bttraverse_
  , btfoldMap

   -- * Applicative
   -- | If @t@ is an 'ApplicativeT', the type of 'tpure' shows that its
   --   second argument must be a phantom-type, so there are really no
   --   interesting types that are both 'ApplicativeT' and 'ApplicativeB'.
   --   However, we can sometimes reconstruct a bi-applicative from an
   --   'ApplicativeB' and a 'FunctorT'.
  , btpure
  , btpure1
  , btprod

    -- * Wrappers
  , Flip(..)
  ) where


import Barbies.Internal.Trivial (Unit(..))
import Barbies.Internal.Writer (execWr, tell)
import Data.Functor.Barbie
import Data.Functor.Transformer

import Control.Applicative (Alternative(..))
import Control.Monad ((>=>))
import Data.Monoid (Alt(..))
import Data.Functor (void)
import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))

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

-- | 'bttraverse1' with the arguments flipped.
--
-- @since 2.1.0.0
btfor1
  :: ( TraversableB (b f)
     , TraversableT b
     , Monad t
     )
  => b f f
  -> (forall a . f a -> t (g a))
  -> t (b g g)
btfor1 b f = bttraverse1 f b

-- | Map each element to an action, evaluate these actions from left to right
--   and ignore the results.
bttraverse_
  :: ( TraversableB (b f)
     , TraversableT b
     , Monad e
     )
  => (forall a. f a -> e c)
  -> (forall a. g a -> e d)
  -> b f g
  -> e ()
bttraverse_ hf hg
  = void . bttraverse (neuter . hf) (neuter . hg)
  where
    neuter
      = fmap (const $ Const ())

-- | Map each element to a monoid, and combine the results.
btfoldMap
  :: ( TraversableB (b f)
     , TraversableT b
     , Monoid m
     )
  => (forall a. f a -> m)
  -> (forall a. g a -> m)
  -> b f g -> m
btfoldMap hf hg
  = execWr . bttraverse_ (tell . hf) (tell . hg)

-- }} Traversable -------------------------------------------------------------


-- {{ Applicative -------------------------------------------------------------
-- | Conceptually, this is like simultaneously using `bpure' and 'tpure'.
btpure
 :: ( ApplicativeB (b Unit)
    , FunctorT b
    )
 => (forall a . f a)
 -> (forall a . g a)
 -> b f g
btpure fa ga
  = tmap (\Unit-> fa) (bpure ga)
{-# INLINE btpure #-}

-- | A version of 'btpure' specialized to a single argument.
btpure1
  :: ( ApplicativeB (b Unit)
     , FunctorT b
     )
  => (forall a . f a)
  -> b f f
btpure1 h
  = btpure h h
{-# INLINE btpure1 #-}

-- | Simultaneous product on both arguments.
btprod
  :: ( ApplicativeB (b (Alt (Product f f')))
     , FunctorT b
     , Alternative f
     , Alternative f'
     )
  => b f g
  -> b f' g'
  -> b (f `Product` f') (g `Product` g')
btprod l r
  = tmap getAlt $ (tmap oneL l) `bprod` (tmap oneR r)
  where
      oneL la = Alt (Pair la empty)
      oneR ga = Alt (Pair empty ga)
{-# INLINE btprod #-}

-- }} Applicative -------------------------------------------------------------


-- | Convert a 'FunctorB' into a 'FunctorT' and vice-versa.
newtype Flip b l r
  = Flip { runFlip :: b r l }
  deriving (Eq, Ord, Read, Show)


instance FunctorT b => FunctorB (Flip b f) where
  bmap h (Flip bfx)
    = Flip (tmap h bfx)
  {-# INLINE bmap #-}

instance DistributiveT b => DistributiveB (Flip b f) where
  bdistribute = Flip . tdistribute . fmap runFlip
  {-# INLINE bdistribute #-}

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

instance (forall f. DistributiveB (b f)) => DistributiveT (Flip b) where
  tdistribute = Flip . bdistribute . fmap runFlip
  {-# INLINE tdistribute #-}

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
