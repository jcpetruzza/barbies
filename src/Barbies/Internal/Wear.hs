{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies.Internal.Wear
  ( Wear, Bare, Covered, Thin
  )

where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Data.Generics.GenericN (Param)

data Bare
data Covered

-- | 'Thin' is a marker type whose purpose is to influence the behaviour
-- of the `Wear` type family. It can not be instantiated.
--
-- @Thin f@ adds an @f@ wrapper to a field iff the barbie is covered.
-- This allows /field-specific/ wrappers that get removed when stripping
-- (and added when covering) automatically by the Generic-derived instances.
-- @f@ /must/ be a plain newtype wrapper, i.e. it requires that
-- @Coercible a (f a)@ holds.
--
-- As an example and showcase, consider this barbie type:
--
-- @
-- data T t f =
--   T { f1 :: 'Wear' t f Int
--     , f2 :: 'Wear' t f (Sum Int)
--     , f3 :: 'Wear' t f (Maybe Int)
--     , f4 :: 'Wear' t f (Thin Sum Int)
--     -- , f5 :: 'Wear' t f (Thin Maybe Int)
--     -- not allowed, because 'Maybe' is no newtype
--     }
-- @
--
-- with @x :: T Covered IO@ we would have
--
-- @
-- f1 x :: IO Int
-- f2 x :: IO (Sum Int)
-- f3 x :: IO (Maybe Int)
-- f4 x :: IO (Sum Int)
-- @
--
-- and with @y :: T Bare Identity@ we would have
--
-- @
-- f1 y :: Int
-- f2 y :: Sum Int
-- f3 y :: Maybe Int
-- f4 y :: Int           -- Thin removes the Sum wrapper when bare
-- @
data Thin (f :: Type -> Type) (a :: Type)

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
  Wear Bare    f (Thin g a) = a
  Wear Bare    f a          = a
  Wear Covered f (Thin g a) = f (g a)
  Wear Covered f a          = f a
  Wear (Param _ t) f a = Wear t f a
  Wear t       _ _ = TypeError (     'Text "`Wear` should only be used with "
                               ':<>: 'Text "`Bare` or `Covered`."
                               ':$$: 'Text "`" ':<>: 'ShowType t ':<>: 'Text "`"
                               ':<>: 'Text " is not allowed in this context."
                               )
