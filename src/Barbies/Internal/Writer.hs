module Barbies.Internal.Writer
  ( Wr
  , execWr
  , tell
  ) where

-- ---------------------------------------------------------------------
-- We roll our own State/efficient-Writer monad, not to add dependencies
-- ---------------------------------------------------------------------

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

instance Monad (St s) where
  return = pure
  {-# INLINE return #-}

  St action >>= f
    = St $ \s ->
        let
          (a, s') = action s
          St go  = f a
        in
          go s'
  {-# INLINE (>>=) #-}

type Wr = St

execWr :: Monoid w => Wr w a -> w
execWr
  = snd . runSt mempty

tell :: Monoid w => w -> Wr w ()
tell w
  = St (\s -> ((), seq s s `mappend` w))
