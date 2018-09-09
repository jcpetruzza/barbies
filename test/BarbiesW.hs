{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module BarbiesW
  ( Record1W(..)
  , Record3W(..)

  , Record1WS(..)
  , Record3WS(..)

  , Sum3W(..)

  , CompositeRecordW(..)
  , SumRecW(..)
  , InfRecW(..)

  , NestedFW(..)
  )

where

import Data.Barbie

import Data.Typeable
import GHC.Generics
import Test.Tasty.QuickCheck

----------------------------------------------------
-- Product Barbies
----------------------------------------------------

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


data Record1WS f
  = Record1WS { rec1ws_f1 :: !(Wear f Int) }
  deriving (Generic, Typeable)


instance FunctorB Record1WS
instance TraversableB Record1WS
instance ProductB Record1WS
instance ConstraintsB Record1WS
instance ProofB Record1WS
instance BareB Record1WS


deriving instance ConstraintsOf Show f Record1WS => Show (Record1WS f)
deriving instance ConstraintsOf Eq   f Record1WS => Eq   (Record1WS f)

instance ConstraintsOf Arbitrary f Record1WS => Arbitrary (Record1WS f) where
  arbitrary = Record1WS <$> arbitrary

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


data Record3WS f
  = Record3WS
      { rec3ws_f1 :: !(Wear f Int)
      , rec3ws_f2 :: !(Wear f Bool)
      , rec3ws_f3 :: !(Wear f Char)
      }
  deriving (Generic, Typeable)


instance FunctorB Record3WS
instance TraversableB Record3WS
instance ProductB Record3WS
instance ConstraintsB Record3WS
instance ProofB Record3WS

instance BareB Record3WS

deriving instance ConstraintsOf Show f Record3WS => Show (Record3WS f)
deriving instance ConstraintsOf Eq   f Record3WS => Eq   (Record3WS f)

instance ConstraintsOf Arbitrary f Record3WS => Arbitrary (Record3WS f) where
  arbitrary = Record3WS <$> arbitrary <*> arbitrary <*> arbitrary


----------------------------------------------------
-- Sum Barbies
----------------------------------------------------

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
-- instance ConstraintsB NestedFW

deriving instance (Wear f Int ~ f Int, Show (f Int), Show (Record3W f), Show (Sum3W f)) => Show (NestedFW f)
deriving instance (Wear f Int ~ f Int, Eq   (f Int), Eq   (Record3W f), Eq   (Sum3W f)) => Eq   (NestedFW f)

instance (Wear f Int ~ f Int, Wear f Bool ~ f Bool, Wear f Char ~ f Char, Arbitrary (f Int), Arbitrary (f Bool), Arbitrary (f Char)) => Arbitrary (NestedFW f) where
  arbitrary = NestedFW <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
