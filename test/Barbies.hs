{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# LANGUAGE UndecidableInstances  #-}
module Barbies
  ( Void

  , Record0(..)
  , Record1(..)
  , Record3(..)

  , Ignore1(..)

  , Sum3(..)
  )

where

import Data.Barbie
import Data.Typeable
import GHC.Generics
import Test.Tasty.QuickCheck

---------------------------------------------------
-- Trivial Barbies
---------------------------------------------------

data Void (f :: * -> *)
  deriving(Generic, Typeable, FunctorB, TraversableB, ProductB)

instance Eq   (Void f) where (==) v = case v of
instance Show (Void f) where showsPrec _ v = case v of


----------------------------------------------------
-- Product Barbies
----------------------------------------------------

data Record0 (f :: * -> *)
  = Record0
  deriving(Generic, Typeable, Eq, Show, FunctorB, TraversableB, ProductB)

instance Arbitrary (Record0 f) where arbitrary = pure Record0


data Record1 f
  = Record1 { rec1_f1 :: f Int }
  deriving(Generic, Typeable, FunctorB, TraversableB, ProductB)

deriving instance Show (f Int) => Show (Record1 f)
deriving instance Eq   (f Int) => Eq   (Record1 f)

instance Arbitrary (f Int) => Arbitrary (Record1 f) where
  arbitrary = Record1 <$> arbitrary

data Record3 f
  = Record3
      { rec3_f1 :: f Int
      , rec3_f2 :: f Bool
      , rec3_f3 :: f Char
      }
  deriving(Generic, Typeable, FunctorB, TraversableB, ProductB)

deriving instance (Show (f Int), Show (f Bool), Show (f Char)) => Show (Record3 f)
deriving instance (Eq   (f Int), Eq   (f Bool), Eq   (f Char)) => Eq   (Record3 f)

instance (Arbitrary (f Int), Arbitrary (f Bool), Arbitrary (f Char)) => Arbitrary (Record3 f) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary


-----------------------------------------------------
-- Bad products
-----------------------------------------------------

data Ignore1 (f :: * -> *)
  = Ignore1 { ign1_f1 :: Int }
  deriving(Generic, Typeable, Eq, Show, FunctorB, TraversableB)

instance Arbitrary (Ignore1 f) where arbitrary = Ignore1 <$> arbitrary


-----------------------------------------------------
-- Sums
-----------------------------------------------------

data Sum3 f
  = Sum3_0
  | Sum3_1 (f Int)
  | Sum3_2 (f Int) (f Bool)
  deriving(Generic, Typeable, FunctorB, TraversableB)

deriving instance (Show (f Int), Show (f Bool)) => Show (Sum3 f)
deriving instance (Eq   (f Int), Eq   (f Bool)) => Eq   (Sum3 f)

instance (Arbitrary (f Int), Arbitrary (f Bool)) => Arbitrary (Sum3 f) where
  arbitrary
    = oneof
        [ pure Sum3_0
        , Sum3_1 <$> arbitrary
        , Sum3_2 <$> arbitrary <*> arbitrary
        ]
