{-# LANGUAGE DeriveAnyClass       #-}
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
-- import Test.Tasty.QuickCheck

----------------------------------------------------
-- Product Barbies
----------------------------------------------------

data Record1W t f
  = Record1W { rec1w_f1 :: Wear t f Int }
  deriving (Generic, Typeable)


-- instance FunctorB Record1W
-- instance TraversableB Record1W
-- instance ProductB Record1W
-- instance ConstraintsB Record1W
-- instance ProofB Record1W
-- instance BareB Record1W


-- deriving instance ConstraintsOf Show f Record1W => Show (Record1W t f)
-- deriving instance ConstraintsOf Eq   f Record1W => Eq   (Record1W t f)

-- instance ConstraintsOf Arbitrary f Record1W => Arbitrary (Record1W t f) where
--   arbitrary = Record1W <$> arbitrary


data Record1WS t f
  = Record1WS { rec1ws_f1 :: !(Wear t f Int) }
  deriving (Generic, Typeable)


-- instance FunctorB Record1WS
-- instance TraversableB Record1WS
-- instance ProductB Record1WS
-- instance ConstraintsB Record1WS
-- instance ProofB Record1WS
-- instance BareB Record1WS


-- deriving instance ConstraintsOf Show f Record1WS => Show (Record1WS t f)
-- deriving instance ConstraintsOf Eq   f Record1WS => Eq   (Record1WS t f)

-- instance ConstraintsOf Arbitrary f Record1WS => Arbitrary (Record1WS t f) where
--   arbitrary = Record1WS <$> arbitrary

data Record3W t f
  = Record3W
      { rec3w_f1 :: Wear t f Int
      , rec3w_f2 :: Wear t f Bool
      , rec3w_f3 :: Wear t f Char
      }
  deriving (Generic, Typeable)


-- instance FunctorB Record3W
-- instance TraversableB Record3W
-- instance ProductB Record3W
-- instance ConstraintsB Record3W
-- instance ProofB Record3W

-- instance BareB Record3W

-- deriving instance ConstraintsOf Show f Record3W => Show (Record3W t f)
-- deriving instance ConstraintsOf Eq   f Record3W => Eq   (Record3W t f)

-- instance ConstraintsOf Arbitrary f Record3W => Arbitrary (Record3W t f) where
--   arbitrary = Record3W <$> arbitrary <*> arbitrary <*> arbitrary


data Record3WS t f
  = Record3WS
      { rec3ws_f1 :: !(Wear t f Int)
      , rec3ws_f2 :: !(Wear t f Bool)
      , rec3ws_f3 :: !(Wear t f Char)
      }
  deriving (Generic, Typeable)


-- instance FunctorB Record3WS
-- instance TraversableB Record3WS
-- instance ProductB Record3WS
-- instance ConstraintsB Record3WS
-- instance ProofB Record3WS

-- instance BareB Record3WS

-- deriving instance ConstraintsOf Show f Record3WS => Show (Record3WS t f)
-- deriving instance ConstraintsOf Eq   f Record3WS => Eq   (Record3WS t f)

-- instance ConstraintsOf Arbitrary f Record3WS => Arbitrary (Record3WS t f) where
--   arbitrary = Record3WS <$> arbitrary <*> arbitrary <*> arbitrary


----------------------------------------------------
-- Sum Barbies
----------------------------------------------------

data Sum3W t f
  = Sum3W_0
  | Sum3W_1 (Wear t f Int)
  | Sum3W_2 (Wear t f Int) (Wear t f Bool)
  deriving (Generic, Typeable)

-- instance FunctorB Sum3W
-- instance TraversableB Sum3W
-- instance ConstraintsB Sum3W
-- instance BareB Sum3W

-- deriving instance ConstraintsOf Show f Sum3W => Show (Sum3W t f)
-- deriving instance ConstraintsOf Eq   f Sum3W => Eq   (Sum3W t f)

-- instance ConstraintsOf Arbitrary f Sum3W => Arbitrary (Sum3W t f) where
--   arbitrary
--     = oneof
--         [ pure Sum3W_0
--         , Sum3W_1 <$> arbitrary
--         , Sum3W_2 <$> arbitrary <*> arbitrary
--         ]


-----------------------------------------------------
-- Composite and recursive
-----------------------------------------------------


data CompositeRecordW t f
  = CompositeRecordW
      { crecw_f1 :: Wear t f Int
      , crecw_F2 :: Wear t f Bool
      , crecw_f3 :: Record3W t f
      , crecw_f4 :: Record1W t f
      }
  deriving (Generic, Typeable)

-- instance FunctorB CompositeRecordW
-- instance TraversableB CompositeRecordW
-- instance ProductB CompositeRecordW
-- instance ConstraintsB CompositeRecordW
-- instance ProofB CompositeRecordW
-- instance BareB CompositeRecordW

-- deriving instance ConstraintsOf Show f CompositeRecordW => Show (CompositeRecordW t f)
-- deriving instance ConstraintsOf Eq   f CompositeRecordW => Eq   (CompositeRecordW t f)

-- instance ConstraintsOf Arbitrary f CompositeRecordW => Arbitrary (CompositeRecordW t f) where
--   arbitrary
--     = CompositeRecordW <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data SumRecW t f
  = SumRecW_0
  | SumRecW_1 (Wear t f Int)
  | SumRecW_2 (Wear t f Int) (SumRecW t f)
  deriving (Generic, Typeable)

-- instance FunctorB SumRecW
-- instance TraversableB SumRecW
-- instance ConstraintsB SumRecW
-- instance BareB SumRecW

-- deriving instance ConstraintsOf Show f SumRecW => Show (SumRecW t f)
-- deriving instance ConstraintsOf Eq   f SumRecW => Eq   (SumRecW t f)

-- instance ConstraintsOf Arbitrary f SumRecW => Arbitrary (SumRecW t f) where
--   arbitrary
--     = oneof
--         [ pure SumRecW_0
--         , SumRecW_1 <$> arbitrary
--         , SumRecW_2 <$> arbitrary <*> arbitrary
--         ]

data InfRecW t f
  = InfRecW { irw_1 :: Wear t f Int, irw_2 :: InfRecW t f }
  deriving (Generic, Typeable)


-- instance FunctorB InfRecW
-- instance TraversableB InfRecW
-- instance ProductB InfRecW
-- instance ConstraintsB InfRecW
-- instance ProofB InfRecW
-- instance BareB InfRecW

-- deriving instance ConstraintsOf Show f InfRecW => Show (InfRecW t f)
-- deriving instance ConstraintsOf Eq   f InfRecW => Eq   (InfRecW t f)

-----------------------------------------------------
-- Nested under functors
-----------------------------------------------------

data NestedFW t f
  = NestedFW
      { npfw_1 :: Wear t f Int
      , npfw_2 :: [Record3W t f]
      , npfw_3 :: Maybe (Sum3W t f)
      , npfw_4 :: Maybe (NestedFW t f)
      }
  deriving (Generic, Typeable)



-- instance FunctorB NestedFW
-- instance TraversableB NestedFW
-- instance BareB NestedFW
-- -- instance ConstraintsB NestedFW

-- deriving instance (Wear t f Int ~ f Int, Show (f Int), Show (Record3W t f), Show (Sum3W t f)) => Show (NestedFW t f)
-- deriving instance (Wear t f Int ~ f Int, Eq   (f Int), Eq   (Record3W t f), Eq   (Sum3W t f)) => Eq   (NestedFW t f)

-- instance (Wear t f Int ~ f Int, Wear t f Bool ~ f Bool, Wear t f Char ~ f Char, Arbitrary (f Int), Arbitrary (f Bool), Arbitrary (f Char)) => Arbitrary (NestedFW t f) where
--   arbitrary = NestedFW <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
