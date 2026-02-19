{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TestMethodBarbies
  ( MethodRecord1(..)
  , MethodRecord2(..)
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
