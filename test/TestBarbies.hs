{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module TestBarbies
  ( Barbies.Void

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
  , Nested2F(..)

  , HKB(..)
  )

where

import qualified Barbies
import Data.Functor.Barbie

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
instance ApplicativeB Record0
instance ConstraintsB Record0

instance Arbitrary (Record0 f) where arbitrary = pure Record0


data Record1 f
  = Record1 { rec1_f1 :: f Int }
  deriving (Generic, Typeable)


instance FunctorB Record1
instance TraversableB Record1
instance ApplicativeB Record1
instance ConstraintsB Record1

deriving instance AllBF Show f Record1 => Show (Record1 f)
deriving instance AllBF Eq   f Record1 => Eq   (Record1 f)

instance AllBF Arbitrary f Record1 => Arbitrary (Record1 f) where
  arbitrary = Record1 <$> arbitrary


data Record1S f
  = Record1S { rec1s_f1 :: !(f Int) }
  deriving (Generic, Typeable)


instance FunctorB Record1S
instance TraversableB Record1S
instance ApplicativeB Record1S
instance ConstraintsB Record1S

deriving instance AllBF Show f Record1S => Show (Record1S f)
deriving instance AllBF Eq   f Record1S => Eq   (Record1S f)

instance AllBF Arbitrary f Record1S => Arbitrary (Record1S f) where
  arbitrary = Record1S <$> arbitrary


data Record3 f
  = Record3
      { rec3_f1 :: f Int
      , rec3_f2 :: f Bool
      , rec3_f3 :: f Char
      , rec3_m1 :: Maybe ()
      }
  deriving (Generic, Typeable)


instance FunctorB Record3
instance TraversableB Record3
instance ApplicativeB Record3
instance ConstraintsB Record3

deriving instance AllBF Show f Record3 => Show (Record3 f)
deriving instance AllBF Eq   f Record3 => Eq   (Record3 f)

instance AllBF Arbitrary f Record3 => Arbitrary (Record3 f) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data Record3S f
  = Record3S
      { rec3s_f1 :: !(f Int)
      , rec3s_f2 :: !(f Bool)
      , rec3s_f3 :: !(f Char)
      }
  deriving (Generic, Typeable)


instance FunctorB Record3S
instance TraversableB Record3S
instance ApplicativeB Record3S
instance ConstraintsB Record3S

deriving instance AllBF Show f Record3S => Show (Record3S f)
deriving instance AllBF Eq   f Record3S => Eq   (Record3S f)

instance AllBF Arbitrary f Record3S => Arbitrary (Record3S f) where
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

deriving instance AllBF Show f Sum3 => Show (Sum3 f)
deriving instance AllBF Eq   f Sum3 => Eq   (Sum3 f)

instance AllBF Arbitrary f Sum3 => Arbitrary (Sum3 f) where
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
instance ApplicativeB CompositeRecord
instance ConstraintsB CompositeRecord

deriving instance AllBF Show f CompositeRecord => Show (CompositeRecord f)
deriving instance AllBF Eq   f CompositeRecord => Eq   (CompositeRecord f)

instance AllBF Arbitrary f CompositeRecord => Arbitrary (CompositeRecord f) where
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

deriving instance AllBF Show f SumRec => Show (SumRec f)
deriving instance AllBF Eq   f SumRec => Eq   (SumRec f)

instance AllBF Arbitrary f SumRec => Arbitrary (SumRec f) where
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
instance ApplicativeB InfRec
instance ConstraintsB InfRec

deriving instance AllBF Show f InfRec => Show (InfRec f)
deriving instance AllBF Eq   f InfRec => Eq   (InfRec f)

-----------------------------------------------------
-- Nested under functors
-----------------------------------------------------

data NestedF f
  = NestedF
      { npf_1 :: f Int
      , npf_2 :: [Record3 f]
      , npf_4 :: Maybe (NestedF f)
      }
  deriving (Generic, Typeable)

instance FunctorB NestedF
instance TraversableB NestedF
instance ApplicativeB NestedF

deriving instance (Show (f Int), Show (Record3 f)) => Show (NestedF f)
deriving instance (Eq   (f Int), Eq   (Record3 f)) => Eq   (NestedF f)

instance (Arbitrary (f Int), AllBF Arbitrary f Record3) => Arbitrary (NestedF f) where
  arbitrary
    = scale (`div` 2) $
        NestedF <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary


data Nested2F f
  = Nested2F
    { np2f_1 :: f Int
    , np2f_2 :: [Maybe (Nested2F f)]
    }
  deriving (Generic, Typeable)

instance FunctorB Nested2F
instance TraversableB Nested2F
instance ApplicativeB Nested2F

deriving instance Show (f Int) => Show (Nested2F f)
deriving instance Eq (f Int) => Eq (Nested2F f)

instance Arbitrary (f Int) => Arbitrary (Nested2F f) where
  arbitrary = scale (`div` 2) $ Nested2F <$> arbitrary <*> scale (`div` 2) arbitrary

-----------------------------------------------------
-- Parametric barbies
-----------------------------------------------------

data ParB b (f :: * -> *)
  = ParB (b f)
  deriving (Generic, Typeable)

instance FunctorB b => FunctorB (ParB b)
instance TraversableB b => TraversableB (ParB b)
instance ApplicativeB b => ApplicativeB (ParB b)
instance ConstraintsB b => ConstraintsB (ParB b)

data ParBH h b (f :: * -> *)
  = ParBH (h (b f))
  deriving (Generic, Typeable)

instance (Functor h, FunctorB b) => FunctorB (ParBH h b)
instance (Traversable h, TraversableB b) => TraversableB (ParBH h b)
instance (Applicative h, ApplicativeB b) => ApplicativeB (ParBH h b)

data ParX a f
  = ParX (f a) a
  deriving (Generic, Typeable)

instance FunctorB (ParX a)
instance TraversableB (ParX a)
instance Monoid a => ApplicativeB (ParX a)
instance ConstraintsB (ParX a)


-----------------------------------------------------
-- Higher-kinded barbies
-----------------------------------------------------

data HKB b
  = HKB
      { hkb1 :: b Maybe
      , khb2 :: b ([])
      }
  deriving (Generic, Typeable)

instance FunctorB HKB
instance TraversableB HKB
instance ApplicativeB HKB
instance ConstraintsB HKB
