{-# LANGUAGE TypeFamilies       #-}
module Data.Barbie.Internal.Wear
  ( Bare, Wear
  )

where


import Data.Barbie.Internal.Generics(Target, W)

-- | The 'Wear' type-function allows one to define a type as
--
-- @
-- data B f
--   = B { f1 :: Wear f Int
--       , f2 :: Wear f Bool
--       }
-- @
--
-- The advantage being that on can then retrieve a "naked"
-- value by doing:
--
-- @
-- B { f1 :: 5, f2 = True } :: B Bare
-- #
type family Wear f a where
  Wear Bare a = a
  Wear (Target f) a = Target (W f) a
  Wear f    a = f a


-- | 'Bare' is the only type such that 'Wear Bare a ~ a'.
data Bare a
