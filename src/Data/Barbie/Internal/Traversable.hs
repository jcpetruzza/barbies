-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie.Internal.Traversable
----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies       #-}
module Data.Barbie.Internal.Traversable
  ( TraversableB(..)
  , btraverse_
  , bsequence
  , bsequence'
  , bfoldMap

  , CanDeriveGenericInstance
  , GTraversableB
  , gbtraverseDefault
  )

where

import Data.Barbie.Internal.Functor (FunctorB(..))
import Data.Barbie.Internal.Tag (Tag(..), CoercibleTag(..))

import Data.Functor (void)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import GHC.Generics


-- | Barbie-types that can be traversed from left to right. Instances should
--   satisfy the following laws:
--
-- @
--  t . 'btraverse' f = 'btraverse' (t . f)  -- naturality
-- 'btraverse' 'Data.Functor.Identity' = 'Data.Functor.Identity'         -- identity
-- 'btraverse' ('Compose' . 'fmap' g . f) = 'Compose' . 'fmap' ('btraverse' g) . 'btraverse' f -- composition
-- @
--
-- There is a default 'btraverse' implementation for 'Generic' types, so
-- instances can derived automatically.
class FunctorB b => TraversableB b where
  btraverse :: Applicative t => (forall a . f a -> t (g a)) -> b f -> t (b g)

  default btraverse
    :: ( Applicative t, CanDeriveGenericInstance b f g)
    => (forall a . f a -> t (g a)) -> b f -> t (b g)
  btraverse = gbtraverseDefault



-- | Map each element to an action, evaluate these actions from left to right,
--   and ignore the results.
btraverse_ :: (TraversableB b, Applicative t) => (forall a. f a -> t c) -> b f -> t ()
btraverse_ f
  = void . btraverse (fmap (const $ Const ()) . f)


-- | Evaluate each action in the structure from left to right,
--   and collect the results.
bsequence :: (Applicative f, TraversableB b) => b (Compose f g) -> f (b g)
bsequence
  = btraverse getCompose

-- | A version of 'bsequence' with @g@ specialized to 'Identity'.
bsequence' :: (Applicative f, TraversableB b) => b f -> f (b Identity)
bsequence'
  = btraverse (fmap Identity)


-- | Map each element to a monoid, and combine the results.
bfoldMap :: (TraversableB b, Monoid m) => (forall a. f a -> m) -> b f -> m
bfoldMap f
  = execWr . btraverse_ (tell . f)


data F_
data G_

type F = Tag F_
type G = Tag G_

-- | Intuivively, the requirements to have @'TraversableB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'TraversableB' instance.
--
--     * Recursive usages of @B f@ are allowed to appear as argument to a
--       'Traversable' (e.g. @'Maybe' (B f)')
type CanDeriveGenericInstance b f g
  = ( Generic (b (F f))
    , Generic (b (G g))
    , CoercibleTag F b f
    , CoercibleTag G b g
    , GTraversableB f g (Rep (b (F f))) (Rep (b (G g)))
    )

-- | Default implementation of 'btraverse' based on 'Generic'.
gbtraverseDefault
  :: ( Applicative t, CanDeriveGenericInstance b f g)
  => (forall a . f a -> t (g a))
  -> b f -> t (b g)
gbtraverseDefault h
  = fmap (coerceUntag @G . to)
      . gbtraverse h
      . from
      . coerceTag @F


class GTraversableB f g repbf repbg where
  gbtraverse
    :: Applicative t => (forall a . f a -> t (g a)) -> repbf x -> t (repbg x)

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GTraversableB f g bf bg => GTraversableB f g (M1 i c bf) (M1 i c bg) where
  gbtraverse h = fmap M1 . gbtraverse h . unM1
  {-# INLINE gbtraverse #-}

instance GTraversableB f g V1 V1 where
  gbtraverse _ _ = undefined
  {-# INLINE gbtraverse #-}

instance GTraversableB f g U1 U1 where
  gbtraverse _ = pure
  {-# INLINE gbtraverse #-}

instance (GTraversableB f g l l', GTraversableB f g r r') => GTraversableB f g (l :*: r) (l' :*: r') where
  gbtraverse h (l :*: r) = (:*:) <$> gbtraverse h l <*> gbtraverse h r
  {-# INLINE gbtraverse #-}

instance (GTraversableB f g l l', GTraversableB f g r r') => GTraversableB f g (l :+: r) (l' :+: r') where
  gbtraverse h = \case
    L1 l -> L1 <$> gbtraverse h l
    R1 r -> R1 <$> gbtraverse h r
  {-# INLINE gbtraverse #-}


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance GTraversableB f g (Rec0 (F f a)) (Rec0 (G g a)) where
  gbtraverse h (K1 (Tag fa)) = K1 . Tag <$> h fa
  {-# INLINE gbtraverse #-}

instance TraversableB b => GTraversableB f g (Rec0 (b (F f))) (Rec0 (b (G g))) where
  gbtraverse h (K1 bf)
    = K1 <$> btraverse (fmap Tag . h . unTag) bf
  {-# INLINE gbtraverse #-}

instance (Traversable h, TraversableB b) => GTraversableB f g (K1 R (h (b (F f)))) (K1 R (h (b (G g)))) where
  gbtraverse h (K1 hbf)
    = K1 <$> traverse (btraverse (fmap Tag . h . unTag)) hbf
  {-# INLINE gbtraverse #-}


instance {-# OVERLAPPABLE #-} bf ~ bg => GTraversableB f g (Rec0 bf) (Rec0 bg) where
  gbtraverse _ = pure
  {-# INLINE gbtraverse #-}




-- We roll our own State/efficient-Writer monad, not to add dependencies

newtype St s a
  = St (s -> (a, s))

runSt :: s -> St s a -> (a, s)
runSt s (St f)
  = f s

instance Functor (St s) where
  fmap f (St g)
    = St $ (\(a, s') -> (f a, s')) . g
  {-# INLINE fmap #-}

instance Applicative (St s) where
  pure
    = St . (,)
  {-# INLINE pure #-}

  St l <*> St r
    = St $ \s ->
        let (f, s')  = l s
            (x, s'') = r s'
        in (f x, s'')
  {-# INLINE (<*>) #-}

type Wr = St

execWr :: Monoid w => Wr w a -> w
execWr
  = snd . runSt mempty

tell :: Monoid w => w -> Wr w ()
tell w
  = St (\s -> ((), s `mappend` w))
