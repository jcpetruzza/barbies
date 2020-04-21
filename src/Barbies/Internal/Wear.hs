{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies.Internal.Wear
  ( Wear, Bare, Covered
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
  Wear Bare    f a = a
  Wear Covered f a = f a
  Wear (Param _ t) f a = Wear t f a
  Wear t       _ _ = TypeError (     'Text "`Wear` should only be used with "
                               ':<>: 'Text "`Bare` or `Covered`."
                               ':$$: 'Text "`" ':<>: 'ShowType t ':<>: 'Text "`"
                               ':<>: 'Text " is not allowed in this context."
                               )
