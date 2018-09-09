{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Data.Barbie.Internal.Wear
  ( Bare, Wear
  , NotBare
  )

where


import Data.Barbie.Internal.Generics(Target, W)

-- | The 'Wear' type-function allows one to define a Barbie-type as
--
-- @
-- data B f
--   = B { f1 :: 'Wear' f 'Int'
--       , f2 :: 'Wear' f 'Bool'
--       }
-- @
--
-- This way, one can use 'Bare' as a phantom that denotes no functor
-- around the typw:
--
--
-- @
-- B { f1 :: 5, f2 = 'True' } :: B 'Bare'
-- @
type family Wear f a where
  Wear Bare a = a
  Wear (Target f) a = Target (W f) a
  Wear f    a = f a


-- | 'Bare' is the only type such that @'Wear' 'Bare' a ~ a'@.
data Bare a

-- | 'NotBare' has one universal instance that makes @'NotBare' f a@
--   equivalent to @'Wear' f a ~ f a@. This will hold every time
--   `f` is not `Bare`.
class Wear f a ~ f a => NotBare f a where
instance Wear f a ~ f a => NotBare f a
