{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module TestBiBarbies
  (
    Record0(..)
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

  , ParX(..)
  , HKB(..)
  )

where

import qualified Barbies
import Data.Functor.Indexed

import Data.Typeable
import GHC.Generics
import Test.Tasty.QuickCheck

instance Arbitrary (b r l) => Arbitrary (Barbies.Flip b l r) where
  arbitrary = Barbies.Flip <$> arbitrary

----------------------------------------------------
-- Product Barbies
----------------------------------------------------

data Record0 (f :: kl -> *) (x :: kr)
  = Record0
  deriving
    ( Generic, Typeable
    , Eq, Show
    )

instance FunctorI Record0
instance ApplicativeI Record0
instance TraversableI Record0
instance ConstraintsI Record0

instance Arbitrary (Record0 f g) where arbitrary = pure Record0


data Record1 f (x :: kr)
  = Record1 { rec1_f1 :: f Int }
  deriving (Generic, Typeable)


instance FunctorI Record1
instance ApplicativeI Record1
instance TraversableI Record1
instance ConstraintsI Record1

deriving instance AllIF Show f Record1 => Show (Record1 f x)
deriving instance AllIF Eq   f Record1 => Eq   (Record1 f x)

instance AllIF Arbitrary f Record1 => Arbitrary (Record1 f g) where
  arbitrary = Record1 <$> arbitrary


data Record1S f (x :: kr)
  = Record1S { rec1s_f1 :: !(f Int) }
  deriving (Generic, Typeable)


instance FunctorI Record1S
instance ApplicativeI Record1S
instance TraversableI Record1S
instance ConstraintsI Record1S

deriving instance AllIF Show f Record1S => Show (Record1S f x)
deriving instance AllIF Eq   f Record1S => Eq   (Record1S f x)

instance AllIF Arbitrary f Record1S => Arbitrary (Record1S f x) where
  arbitrary = Record1S <$> arbitrary


data Record3 f x
  = Record3
      { rec3_f1 :: f Int
      , rec3_f2 :: f Bool
      , rec3_f3 :: f Char
      , rec3_m1 :: Maybe ()
      }
  deriving (Generic, Typeable)


instance FunctorI Record3
instance ApplicativeI Record3
instance TraversableI Record3
instance ConstraintsI Record3

deriving instance AllIF Show f Record3 => Show (Record3 f x)
deriving instance AllIF Eq   f Record3 => Eq   (Record3 f x)

instance AllIF Arbitrary f Record3 => Arbitrary (Record3 f x) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Record3S f x
  = Record3S
      { rec3s_f1 :: !(f Int)
      , rec3s_f2 :: !(f Bool)
      , rec3s_f3 :: !(f Char)
      }
  deriving (Generic, Typeable)


instance FunctorI Record3S
instance ApplicativeI Record3S
instance TraversableI Record3S
instance ConstraintsI Record3S

deriving instance AllIF Show f Record3S => Show (Record3S f x)
deriving instance AllIF Eq   f Record3S => Eq   (Record3S f x)

instance AllIF Arbitrary f Record3S => Arbitrary (Record3S f x) where
  arbitrary = Record3S <$> arbitrary <*> arbitrary <*> arbitrary

-----------------------------------------------------
-- Bad products
-----------------------------------------------------

data Ignore1 (f :: * -> *) (x :: kx)
  = Ignore1 { ign1_f1 :: Int }
  deriving (Generic, Typeable, Eq, Show)

instance FunctorI Ignore1
instance TraversableI Ignore1
instance ConstraintsI Ignore1

instance Arbitrary (Ignore1 f x) where arbitrary = Ignore1 <$> arbitrary


-----------------------------------------------------
-- Sums
-----------------------------------------------------

data Sum3 f x
  = Sum3_0
  | Sum3_1 (f Int)
  | Sum3_2 (f Int) (f Bool)
  deriving (Generic, Typeable)

instance FunctorI Sum3
instance TraversableI Sum3
instance ConstraintsI Sum3

deriving instance AllIF Show f Sum3 => Show (Sum3 f x)
deriving instance AllIF Eq   f Sum3 => Eq   (Sum3 f x)

instance AllIF Arbitrary f Sum3 => Arbitrary (Sum3 f x) where
  arbitrary
    = oneof
        [ pure Sum3_0
        , Sum3_1 <$> arbitrary
        , Sum3_2 <$> arbitrary <*> arbitrary
        ]

-----------------------------------------------------
-- Composite and recursive
-----------------------------------------------------

data CompositeRecord f x
  = CompositeRecord
      { crec_f1 :: f Int
      , crec_F2 :: f Bool
      , crec_f3 :: Record3 f x
      , crec_f4 :: Record1 f x
      }
  deriving (Generic, Typeable)

instance FunctorI CompositeRecord
instance ApplicativeI CompositeRecord
instance TraversableI CompositeRecord
instance ConstraintsI CompositeRecord

deriving instance AllIF Show f CompositeRecord => Show (CompositeRecord f x)
deriving instance AllIF Eq   f CompositeRecord => Eq   (CompositeRecord f x)

instance AllIF Arbitrary f CompositeRecord => Arbitrary (CompositeRecord f x) where
  arbitrary
    = CompositeRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data SumRec f x
  = SumRec_0
  | SumRec_1 (f Int)
  | SumRec_2 (f Int) (SumRec f x)
  deriving (Generic, Typeable)

instance FunctorI SumRec
instance TraversableI SumRec
instance ConstraintsI SumRec

deriving instance AllIF Show f SumRec => Show (SumRec f x)
deriving instance AllIF Eq   f SumRec => Eq   (SumRec f x)

instance AllIF Arbitrary f SumRec => Arbitrary (SumRec f x) where
  arbitrary
    = oneof
        [ pure SumRec_0
        , SumRec_1 <$> arbitrary
        , SumRec_2 <$> arbitrary <*> arbitrary
        ]

data InfRec f x
  = InfRec { ir_1 :: f Int, ir_2 :: InfRec f x }
  deriving (Generic, Typeable)

instance FunctorI InfRec
instance ApplicativeI InfRec
instance TraversableI InfRec
instance ConstraintsI InfRec

deriving instance AllIF Show f InfRec => Show (InfRec f x)
deriving instance AllIF Eq   f InfRec => Eq   (InfRec f x)

-----------------------------------------------------
-- Nested under functors
-----------------------------------------------------

data NestedF f x
  = NestedF
      { npf_1 :: f Int
      , npf_2 :: [Record3 f x]
      , npf_3 :: Maybe (NestedF f x)
      }
  deriving (Generic, Typeable)

instance FunctorI NestedF
instance ApplicativeI NestedF
instance TraversableI NestedF

deriving instance (Show (f Int), Show (Record3 f x)) => Show (NestedF f x)
deriving instance (Eq   (f Int), Eq   (Record3 f x)) => Eq   (NestedF f x)

instance (Arbitrary (f Int), AllIF Arbitrary f Record3, AllIF Arbitrary f Sum3) => Arbitrary (NestedF f x) where
  arbitrary
    = scale (`div` 2) $
        NestedF <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary


data Nested2F f x
  = Nested2F
    { np2f_1 :: f Int
    , np2f_2 :: [Maybe (Nested2F f x)]
    }
  deriving (Generic, Typeable)

instance FunctorI Nested2F
instance TraversableI Nested2F
instance ApplicativeI Nested2F

deriving instance Show (f Int) => Show (Nested2F f x)
deriving instance Eq (f Int) => Eq (Nested2F f x)

instance Arbitrary (f Int) => Arbitrary (Nested2F f x) where
  arbitrary = scale (`div` 2) $ Nested2F <$> arbitrary <*> scale (`div` 2) arbitrary


-----------------------------------------------------
-- Parametric barbies
-----------------------------------------------------

data ParB b (f :: k -> *) (x :: kx)
  = ParB (b f x)
  deriving (Generic, Typeable)

instance FunctorI b => FunctorI (ParB b)
instance ApplicativeI b => ApplicativeI (ParB b)
instance TraversableI b => TraversableI (ParB b)
instance ConstraintsI b => ConstraintsI (ParB b)

data ParBH h b (f :: k -> *) (x :: kx)
  = ParBH (h (b f x))
  deriving (Generic, Typeable)

instance (Functor h, FunctorI b) => FunctorI (ParBH h b)
instance (Applicative h, ApplicativeI b) => ApplicativeI (ParBH h b)
instance (Traversable h, TraversableI b) => TraversableI (ParBH h b)

data ParX a f x
  = ParX (f a) a
  deriving (Generic, Typeable)

instance FunctorI (ParX a)
instance Monoid a => ApplicativeI (ParX a)
instance TraversableI (ParX a)
instance ConstraintsI (ParX a)

deriving instance (Show a, Show (f a)) => Show (ParX a f x)
deriving instance (Eq a, Eq (f a)) => Eq (ParX a f x)

instance (Arbitrary a, Arbitrary (f a)) => Arbitrary (ParX a f x) where
  arbitrary
    = ParX <$> arbitrary <*> arbitrary

-----------------------------------------------------
-- Higher-kinded barbies
-----------------------------------------------------

data HKB b x
  = HKB
      { hkb1 :: b Maybe
      , khb2 :: b ([])
      }
  deriving (Generic, Typeable)

instance FunctorI HKB
instance ApplicativeI HKB
instance TraversableI HKB
instance ConstraintsI HKB
