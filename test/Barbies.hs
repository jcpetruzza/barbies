{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies
  ( Void

  , Record0(..)
  , Record1(..)
  , Record3(..)

  , Record1S(..)
  , Record3S(..)

  , Ignore1(..)

  , Sum3(..)

  , CompositeRecord(..)
  , SumRec(..)
  , InfRec(..)

  , NestedF(..)
  )

where

import Data.Barbie

import Data.Typeable
import GHC.Generics
import Test.Tasty.QuickCheck

----------------------------------------------------
-- Product Barbies
----------------------------------------------------

data Record0 (f :: * -> *)
  = Record0
  deriving
    ( Generic, Typeable
    , Eq, Show
    )

instance FunctorB Record0
instance TraversableB Record0
instance ProductB Record0
instance ConstraintsB Record0
instance ProofB Record0

instance Arbitrary (Record0 f) where arbitrary = pure Record0


data Record1 f
  = Record1 { rec1_f1 :: f Int }
  deriving (Generic, Typeable)


instance FunctorB Record1
instance TraversableB Record1
instance ProductB Record1
instance ConstraintsB Record1
instance ProofB Record1

deriving instance AllB (ClassF Show f) Record1 => Show (Record1 f)
deriving instance AllB (ClassF Eq   f) Record1 => Eq   (Record1 f)

instance AllB (ClassF Arbitrary f) Record1 => Arbitrary (Record1 f) where
  arbitrary = Record1 <$> arbitrary


data Record1S f
  = Record1S { rec1s_f1 :: !(f Int) }
  deriving (Generic, Typeable)


instance FunctorB Record1S
instance TraversableB Record1S
instance ProductB Record1S
instance ConstraintsB Record1S
instance ProofB Record1S

deriving instance AllB (ClassF Show f) Record1S => Show (Record1S f)
deriving instance AllB (ClassF Eq   f) Record1S => Eq   (Record1S f)

instance AllB (ClassF Arbitrary f) Record1S => Arbitrary (Record1S f) where
  arbitrary = Record1S <$> arbitrary


data Record3 f
  = Record3
      { rec3_f1 :: f Int
      , rec3_f2 :: f Bool
      , rec3_f3 :: f Char
      }
  deriving (Generic, Typeable)


instance FunctorB Record3
instance TraversableB Record3
instance ProductB Record3
instance ConstraintsB Record3
instance ProofB Record3

deriving instance AllB (ClassF Show f) Record3 => Show (Record3 f)
deriving instance AllB (ClassF Eq   f) Record3 => Eq   (Record3 f)

instance AllB (ClassF Arbitrary f) Record3 => Arbitrary (Record3 f) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary

data Record3S f
  = Record3S
      { rec3s_f1 :: !(f Int)
      , rec3s_f2 :: !(f Bool)
      , rec3s_f3 :: !(f Char)
      }
  deriving (Generic, Typeable)


instance FunctorB Record3S
instance TraversableB Record3S
instance ProductB Record3S
instance ConstraintsB Record3S
instance ProofB Record3S

deriving instance AllB (ClassF Show f) Record3S => Show (Record3S f)
deriving instance AllB (ClassF Eq   f) Record3S => Eq   (Record3S f)

instance AllB (ClassF Arbitrary f) Record3S => Arbitrary (Record3S f) where
  arbitrary = Record3S <$> arbitrary <*> arbitrary <*> arbitrary


-----------------------------------------------------
-- Bad products
-----------------------------------------------------

data Ignore1 (f :: * -> *)
  = Ignore1 { ign1_f1 :: Int }
  deriving (Generic, Typeable, Eq, Show)

instance FunctorB Ignore1
instance TraversableB Ignore1
instance ConstraintsB Ignore1

instance Arbitrary (Ignore1 f) where arbitrary = Ignore1 <$> arbitrary


-----------------------------------------------------
-- Sums
-----------------------------------------------------

data Sum3 f
  = Sum3_0
  | Sum3_1 (f Int)
  | Sum3_2 (f Int) (f Bool)
  deriving (Generic, Typeable)

instance FunctorB Sum3
instance TraversableB Sum3
instance ConstraintsB Sum3

deriving instance AllB (ClassF Show f) Sum3 => Show (Sum3 f)
deriving instance AllB (ClassF Eq   f) Sum3 => Eq   (Sum3 f)

instance AllB (ClassF Arbitrary f) Sum3 => Arbitrary (Sum3 f) where
  arbitrary
    = oneof
        [ pure Sum3_0
        , Sum3_1 <$> arbitrary
        , Sum3_2 <$> arbitrary <*> arbitrary
        ]

-----------------------------------------------------
-- Composite and recursive
-----------------------------------------------------

data CompositeRecord f
  = CompositeRecord
      { crec_f1 :: f Int
      , crec_F2 :: f Bool
      , crec_f3 :: Record3 f
      , crec_f4 :: Record1 f
      }
  deriving (Generic, Typeable)

instance FunctorB CompositeRecord
instance TraversableB CompositeRecord
instance ProductB CompositeRecord
instance ConstraintsB CompositeRecord
instance ProofB CompositeRecord

deriving instance AllB (ClassF Show f) CompositeRecord => Show (CompositeRecord f)
deriving instance AllB (ClassF Eq   f) CompositeRecord => Eq   (CompositeRecord f)

instance AllB (ClassF Arbitrary f) CompositeRecord => Arbitrary (CompositeRecord f) where
  arbitrary
    = CompositeRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data SumRec f
  = SumRec_0
  | SumRec_1 (f Int)
  | SumRec_2 (f Int) (SumRec f)
  deriving (Generic, Typeable)

instance FunctorB SumRec
instance TraversableB SumRec
instance ConstraintsB SumRec

deriving instance AllB (ClassF Show f) SumRec => Show (SumRec f)
deriving instance AllB (ClassF Eq   f) SumRec => Eq   (SumRec f)

instance AllB (ClassF Arbitrary f) SumRec => Arbitrary (SumRec f) where
  arbitrary
    = oneof
        [ pure SumRec_0
        , SumRec_1 <$> arbitrary
        , SumRec_2 <$> arbitrary <*> arbitrary
        ]

data InfRec f
  = InfRec { ir_1 :: f Int, ir_2 :: InfRec f }
  deriving (Generic, Typeable)

instance FunctorB InfRec
instance TraversableB InfRec
instance ProductB InfRec
instance ConstraintsB InfRec
instance ProofB InfRec

deriving instance AllB (ClassF Show f) InfRec => Show (InfRec f)
deriving instance AllB (ClassF Eq   f) InfRec => Eq   (InfRec f)

-----------------------------------------------------
-- Nested under functors
-----------------------------------------------------

data NestedF f
  = NestedF
      { npf_1 :: f Int
      , npf_2 :: [Record3 f]
      , npf_3 :: Maybe (Sum3 f)
      , npf_4 :: Maybe (NestedF f)
      }
  deriving (Generic, Typeable)

instance FunctorB NestedF
instance TraversableB NestedF

deriving instance (Show (f Int), Show (Record3 f), Show (Sum3 f)) => Show (NestedF f)
deriving instance (Eq   (f Int), Eq   (Record3 f), Eq   (Sum3 f)) => Eq   (NestedF f)

instance (Arbitrary (f Int), AllB (ClassF Arbitrary f) Record3, AllB (ClassF Arbitrary f) Sum3) => Arbitrary (NestedF f) where
  arbitrary = NestedF <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
