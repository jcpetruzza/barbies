-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Barbie.Internal.Functor
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

import Data.Barbie.Internal.Deprecated.Generics
import Data.Barbie.Internal.Deprecated.Tags (F,G)

import Data.Barbie.Internal.Functor (FunctorB(..))
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
    :: ( Applicative t, CanDeriveGenericInstance b)
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


-- | Intuivively, the requirements to have @'TraversableB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'TraversableB' instance.
--
--     * Recursive usages of @B f@ are allowed to appear as argument to a
--       'Traversable' (e.g. @'Maybe' (B f)')
type CanDeriveGenericInstance b
  = ( Generic (b (Target F))
    , Generic (b (Target G))
    , GTraversableB (Rep (b (Target F)))
    , Rep (b (Target G)) ~ Repl (Target F) (Target G) (Rep (b (Target F)))
    )

-- | Default implementation of 'btraverse' based on 'Generic'.
gbtraverseDefault
  :: ( Applicative t, CanDeriveGenericInstance b)
  => (forall a . f a -> t (g a))
  -> b f -> t (b g)
gbtraverseDefault f b
  = unsafeUntargetBarbie @G . to <$> gbtraverse f (from (unsafeTargetBarbie @F b))



class GTraversableB b where
  gbtraverse
    :: Applicative t
    => (forall a . f a -> t (g a))
    -> b x -> t (Repl (Target F) (Target G) b x)

-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GTraversableB x => GTraversableB (M1 i c x) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (M1 x) = M1 <$> gbtraverse f x

instance GTraversableB V1 where
  {-# INLINE gbtraverse #-}
  gbtraverse _ _ = undefined

instance GTraversableB U1 where
  {-# INLINE gbtraverse #-}
  gbtraverse _ u1 = pure u1

instance (GTraversableB l, GTraversableB r) => GTraversableB (l :*: r) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (l :*: r)
    = (:*:) <$> gbtraverse f l <*> gbtraverse f r

instance (GTraversableB l, GTraversableB r) => GTraversableB (l :+: r) where
  {-# INLINE gbtraverse #-}
  gbtraverse f = \case
    L1 l -> L1 <$> gbtraverse f l
    R1 r -> R1 <$> gbtraverse f r


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GTraversableB (K1 R (Target (W F) a)) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (K1 fa)
    = K1 . unsafeTarget @(W G) <$> f (unsafeUntarget @(W F) fa)

instance {-# OVERLAPPING #-} GTraversableB (K1 R (Target F a)) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (K1 fa)
    = K1 . unsafeTarget @G <$> f (unsafeUntarget @F fa)

instance {-# OVERLAPPING #-} TraversableB b => GTraversableB (K1 R (b (Target F))) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (K1 bf)
    = K1 <$> btraverse (fmap (unsafeTarget @G) . f . unsafeUntarget @F) bf

instance {-# OVERLAPPING #-}
  ( Traversable h
  , TraversableB b
  , Repl (Target F) (Target G) (K1 R (h (b (Target F)))) -- shouldn't be
      ~ (K1 R (h (b (Target G))))  -- necessary but ghc chokes otherwise
  )
  => GTraversableB (K1 R (h (b (Target F)))) where
  {-# INLINE gbtraverse #-}
  gbtraverse f (K1 hbf)
    = K1 <$> traverse (fmap (unsafeTargetBarbie @G) . btraverse f . unsafeUntargetBarbie @F) hbf


instance (K1 i c) ~ Repl (Target F) (Target G) (K1 i c) => GTraversableB (K1 i c) where
  {-# INLINE gbtraverse #-}
  gbtraverse _ k1 = pure k1




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
