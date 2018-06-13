{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Barbies
  ( Void

  , Record0(..)
  , Record1(..)
  , Record3(..)

  , Record1W(..)
  , Record3W(..)

  , Ignore1(..)

  , Sum3(..)
  , Sum3W(..)

  , CompositeRecord(..)
  , CompositeRecordW(..)
  , SumRec(..)
  , SumRecW(..)
  , InfRec(..)
  , InfRecW(..)

  , NestedF(..)
  , NestedFW(..)
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
  deriving (Generic, Typeable)

instance Eq   (Void f) where (==) v = case v of
instance Show (Void f) where showsPrec _ v = case v of

instance FunctorB Void
instance TraversableB Void
instance ConstraintsB Void
instance BareB Void

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
instance BareB Record0

instance Arbitrary (Record0 f) where arbitrary = pure Record0


data Record1 f
  = Record1 { rec1_f1 :: f Int }
  deriving (Generic, Typeable)


instance FunctorB Record1
instance TraversableB Record1
instance ProductB Record1
instance ConstraintsB Record1
instance ProofB Record1

deriving instance ConstraintsOf Show f Record1 => Show (Record1 f)
deriving instance ConstraintsOf Eq   f Record1 => Eq   (Record1 f)

instance ConstraintsOf Arbitrary f Record1 => Arbitrary (Record1 f) where
  arbitrary = Record1 <$> arbitrary


data Record1W f
  = Record1W { rec1w_f1 :: Wear f Int }
  deriving (Generic, Typeable)


instance FunctorB Record1W
instance TraversableB Record1W
instance ProductB Record1W
instance ConstraintsB Record1W
instance ProofB Record1W
instance BareB Record1W


deriving instance ConstraintsOf Show f Record1W => Show (Record1W f)
deriving instance ConstraintsOf Eq   f Record1W => Eq   (Record1W f)

instance ConstraintsOf Arbitrary f Record1W => Arbitrary (Record1W f) where
  arbitrary = Record1W <$> arbitrary



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

deriving instance ConstraintsOf Show f Record3 => Show (Record3 f)
deriving instance ConstraintsOf Eq   f Record3 => Eq   (Record3 f)

instance ConstraintsOf Arbitrary f Record3 => Arbitrary (Record3 f) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary


data Record3W f
  = Record3W
      { rec3w_f1 :: Wear f Int
      , rec3w_f2 :: Wear f Bool
      , rec3w_f3 :: Wear f Char
      }
  deriving (Generic, Typeable)


instance FunctorB Record3W
instance TraversableB Record3W
instance ProductB Record3W
instance ConstraintsB Record3W
instance ProofB Record3W

instance BareB Record3W

deriving instance ConstraintsOf Show f Record3W => Show (Record3W f)
deriving instance ConstraintsOf Eq   f Record3W => Eq   (Record3W f)

instance ConstraintsOf Arbitrary f Record3W => Arbitrary (Record3W f) where
  arbitrary = Record3W <$> arbitrary <*> arbitrary <*> arbitrary


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

deriving instance ConstraintsOf Show f Sum3 => Show (Sum3 f)
deriving instance ConstraintsOf Eq   f Sum3 => Eq   (Sum3 f)

instance ConstraintsOf Arbitrary f Sum3 => Arbitrary (Sum3 f) where
  arbitrary
    = oneof
        [ pure Sum3_0
        , Sum3_1 <$> arbitrary
        , Sum3_2 <$> arbitrary <*> arbitrary
        ]

data Sum3W f
  = Sum3W_0
  | Sum3W_1 (Wear f Int)
  | Sum3W_2 (Wear f Int) (Wear f Bool)
  deriving (Generic, Typeable)

instance FunctorB Sum3W
instance TraversableB Sum3W
instance ConstraintsB Sum3W
instance BareB Sum3W

deriving instance ConstraintsOf Show f Sum3W => Show (Sum3W f)
deriving instance ConstraintsOf Eq   f Sum3W => Eq   (Sum3W f)

instance ConstraintsOf Arbitrary f Sum3W => Arbitrary (Sum3W f) where
  arbitrary
    = oneof
        [ pure Sum3W_0
        , Sum3W_1 <$> arbitrary
        , Sum3W_2 <$> arbitrary <*> arbitrary
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

deriving instance ConstraintsOf Show f CompositeRecord => Show (CompositeRecord f)
deriving instance ConstraintsOf Eq   f CompositeRecord => Eq   (CompositeRecord f)

instance ConstraintsOf Arbitrary f CompositeRecord => Arbitrary (CompositeRecord f) where
  arbitrary
    = CompositeRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data CompositeRecordW f
  = CompositeRecordW
      { crecw_f1 :: Wear f Int
      , crecw_F2 :: Wear f Bool
      , crecw_f3 :: Record3W f
      , crecw_f4 :: Record1W f
      }
  deriving (Generic, Typeable)

instance FunctorB CompositeRecordW
instance TraversableB CompositeRecordW
instance ProductB CompositeRecordW
instance ConstraintsB CompositeRecordW
instance ProofB CompositeRecordW
instance BareB CompositeRecordW

deriving instance ConstraintsOf Show f CompositeRecordW => Show (CompositeRecordW f)
deriving instance ConstraintsOf Eq   f CompositeRecordW => Eq   (CompositeRecordW f)

instance ConstraintsOf Arbitrary f CompositeRecordW => Arbitrary (CompositeRecordW f) where
  arbitrary
    = CompositeRecordW <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data SumRec f
  = SumRec_0
  | SumRec_1 (f Int)
  | SumRec_2 (f Int) (SumRec f)
  deriving (Generic, Typeable)

instance FunctorB SumRec
instance TraversableB SumRec
instance ConstraintsB SumRec

deriving instance ConstraintsOf Show f SumRec => Show (SumRec f)
deriving instance ConstraintsOf Eq   f SumRec => Eq   (SumRec f)

instance ConstraintsOf Arbitrary f SumRec => Arbitrary (SumRec f) where
  arbitrary
    = oneof
        [ pure SumRec_0
        , SumRec_1 <$> arbitrary
        , SumRec_2 <$> arbitrary <*> arbitrary
        ]

data SumRecW f
  = SumRecW_0
  | SumRecW_1 (Wear f Int)
  | SumRecW_2 (Wear f Int) (SumRecW f)
  deriving (Generic, Typeable)

instance FunctorB SumRecW
instance TraversableB SumRecW
instance ConstraintsB SumRecW
instance BareB SumRecW

deriving instance ConstraintsOf Show f SumRecW => Show (SumRecW f)
deriving instance ConstraintsOf Eq   f SumRecW => Eq   (SumRecW f)

instance ConstraintsOf Arbitrary f SumRecW => Arbitrary (SumRecW f) where
  arbitrary
    = oneof
        [ pure SumRecW_0
        , SumRecW_1 <$> arbitrary
        , SumRecW_2 <$> arbitrary <*> arbitrary
        ]


data InfRec f
  = InfRec { ir_1 :: f Int, ir_2 :: InfRec f }
  deriving (Generic, Typeable)

instance FunctorB InfRec
instance TraversableB InfRec
instance ProductB InfRec
instance ConstraintsB InfRec
instance ProofB InfRec

deriving instance ConstraintsOf Show f InfRec => Show (InfRec f)
deriving instance ConstraintsOf Eq   f InfRec => Eq   (InfRec f)

data InfRecW f
  = InfRecW { irw_1 :: Wear f Int, irw_2 :: InfRecW f }
  deriving (Generic, Typeable)


instance FunctorB InfRecW
instance TraversableB InfRecW
instance ProductB InfRecW
instance ConstraintsB InfRecW
instance ProofB InfRecW
instance BareB InfRecW

deriving instance ConstraintsOf Show f InfRecW => Show (InfRecW f)
deriving instance ConstraintsOf Eq   f InfRecW => Eq   (InfRecW f)


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

instance (Arbitrary (f Int), ConstraintsOf Arbitrary f Record3, ConstraintsOf Arbitrary f Sum3) => Arbitrary (NestedF f) where
  arbitrary = NestedF <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data NestedFW f
  = NestedFW
      { npfw_1 :: Wear f Int
      , npfw_2 :: [Record3W f]
      , npfw_3 :: Maybe (Sum3W f)
      , npfw_4 :: Maybe (NestedFW f)
      }
  deriving (Generic, Typeable)



instance FunctorB NestedFW
instance TraversableB NestedFW
instance BareB NestedFW
-- instance  ConstraintsB NetedFW

deriving instance (Wear f Int ~ f Int, Show (f Int), Show (Record3W f), Show (Sum3W f)) => Show (NestedFW f)
deriving instance (Wear f Int ~ f Int, Eq   (f Int), Eq   (Record3W f), Eq   (Sum3W f)) => Eq   (NestedFW f)

instance (Wear f Int ~ f Int, Wear f Bool ~ f Bool, Wear f Char ~ f Char, Arbitrary (f Int), Arbitrary (f Bool), Arbitrary (f Char)) => Arbitrary (NestedFW f) where
  arbitrary = NestedFW <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
