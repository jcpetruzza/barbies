{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies.Internal.Wear
  ( Wear, Bare, Covered, WearTwo
  )

where

import GHC.TypeLits (ErrorMessage (..), TypeError)
import Data.Generics.GenericN (Param)

data Bare
data Covered

-- | The 'Wear' type-function allows one to define a Barbie-type as
--
-- @
-- data B t f
--   = B { f1 :: 'Wear' t f 'Int'
--       , f2 :: 'Wear' t f 'Bool'
--       }
-- @
--
-- This gives rise to two rather different types:
--
--   * @B 'Covered' f@ is a normal Barbie-type, in the sense that
--     @f1 :: B 'Covered' f -> f 'Int'@, etc.
--
--   * @B 'Bare' f@, on the other hand, is a normal record with
--     no functor around the type:
--
-- @
-- B { f1 :: 5, f2 = 'True' } :: B 'Bare' f
-- @
type family Wear t f a where
  Wear Bare        f a = a
  Wear Covered     f a = f a
  Wear (Param _ t) f a = Wear t f a
  Wear t       _ _ = TypeError (     'Text "`Wear` should only be used with "
                               ':<>: 'Text "`Bare` or `Covered`."
                               ':$$: 'Text "`" ':<>: 'ShowType t ':<>: 'Text "`"
                               ':<>: 'Text " is not allowed in this context."
                               )

-- | Like the `Wear` family, but with two wrappers @f@ and @g@ instead of one.
-- This is useful if you have a data-type where @f@ is parametric but @g@ is
-- not, consider this:
--
-- @
-- data T t f =
--   T { f1 :: 'Wear'    t f [Bool]
--     , f2 :: 'Wear'    t f (Sum Int)
--     , f3 :: 'WearTwo' t f Sum Int
--     , f4 :: 'WearTwo' t f Max Int
--     }
-- @
--
-- with @x :: T Covered Option@ we would have
--
-- @
-- f1 x :: IO (Option [Bool])
-- f2 x :: IO (Option (Sum Int))
-- f3 x :: IO (Option (Sum Int))
-- f4 x :: IO (Option (Max Int))
-- @
--
-- and with @y :: T Bare Identity@ we would have
--
-- @
-- f1 y :: Int
-- f2 y :: Sum Int
-- f3 y :: Int
-- f4 y :: Int
-- @
--
-- Note how @(Option (Sum Int))@ (or @Max@) has a nice Semigroup instance that
-- we can use to merge two (covered) barbies,
-- while `WearTwo` removes the wrapper for the bare barbie.
type family WearTwo t f g a where
  WearTwo Bare        f g a = a
  WearTwo Covered     f g a = f (g a)
  WearTwo (Param _ t) f g a = WearTwo t f g a
  WearTwo t           _ _ _ =
    TypeError (     'Text "`WearTwo` should only be used with "
              ':<>: 'Text "`Bare` or `Covered`."
              ':$$: 'Text "`" ':<>: 'ShowType t ':<>: 'Text "`"
              ':<>: 'Text " is not allowed in this context."
              )
