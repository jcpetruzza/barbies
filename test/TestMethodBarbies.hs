{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TestMethodBarbies
  ( MethodRecord1(..)
  , MethodRecord2(..)
  , MethodRecord3(..)
  , MixedRecord(..)
  )

where

import Data.Functor.Barbie
import Data.Kind (Type)
import GHC.Generics
import Data.Typeable


----------------------------------------------------
-- Method Barbies (records-of-functions)
----------------------------------------------------

-- | A simple method record with one method field
data MethodRecord1 (f :: Type -> Type)
  = MethodRecord1 { mr1_method :: Int -> f Int }
  deriving (Generic, Typeable)

instance FunctorB MethodRecord1
instance ApplicativeB MethodRecord1
instance ConstraintsB MethodRecord1


-- | A method record with two method fields
data MethodRecord2 (f :: Type -> Type)
  = MethodRecord2
      { mr2_method1 :: Int -> f Int
      , mr2_method2 :: Bool -> f Bool
      }
  deriving (Generic, Typeable)

instance FunctorB MethodRecord2
instance ApplicativeB MethodRecord2
instance ConstraintsB MethodRecord2


-- | A record mixing regular fields and method fields
data MixedRecord (f :: Type -> Type)
  = MixedRecord
      { mixed_regular :: f Int
      , mixed_method  :: Int -> f Int
      }
  deriving (Generic, Typeable)

instance FunctorB MixedRecord
instance ApplicativeB MixedRecord
instance ConstraintsB MixedRecord


-- | A method record with methods of varying arities (2, 3, and 4)
data MethodRecord3 (f :: Type -> Type)
  = MethodRecord3
      { mr3_method1 :: Int -> Bool -> f Int           -- arity 2
      , mr3_method2 :: Int -> Bool -> Char -> f Bool  -- arity 3
      , mr3_method3 :: Int -> Bool -> Char -> Double -> f Double  -- arity 4
      }
  deriving (Generic, Typeable)

instance FunctorB MethodRecord3
instance ApplicativeB MethodRecord3
instance ConstraintsB MethodRecord3
