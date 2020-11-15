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

  , NestedB(..)
  , MixedBT(..)
  )

where

import Barbies
import Data.Distributive
import qualified TestBarbies

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

instance FunctorT Record0
instance DistributiveT Record0
instance ApplicativeT Record0
instance TraversableT Record0
instance ConstraintsT Record0

instance Arbitrary (Record0 f g) where arbitrary = pure Record0


data Record1 f (x :: kr)
  = Record1 { rec1_f1 :: f Int }
  deriving (Generic, Typeable)


instance FunctorT Record1
instance DistributiveT Record1
instance ApplicativeT Record1
instance TraversableT Record1
instance ConstraintsT Record1

deriving instance AllTF Show f Record1 => Show (Record1 f x)
deriving instance AllTF Eq   f Record1 => Eq   (Record1 f x)

instance AllTF Arbitrary f Record1 => Arbitrary (Record1 f g) where
  arbitrary = Record1 <$> arbitrary


data Record1S f (x :: kr)
  = Record1S { rec1s_f1 :: !(f Int) }
  deriving (Generic, Typeable)


instance FunctorT Record1S
instance DistributiveT Record1S
instance ApplicativeT Record1S
instance TraversableT Record1S
instance ConstraintsT Record1S

deriving instance AllTF Show f Record1S => Show (Record1S f x)
deriving instance AllTF Eq   f Record1S => Eq   (Record1S f x)

instance AllTF Arbitrary f Record1S => Arbitrary (Record1S f x) where
  arbitrary = Record1S <$> arbitrary


data Record3 f x
  = Record3
      { rec3_f1 :: f Int
      , rec3_f2 :: f Bool
      , rec3_f3 :: f Char
      , rec3_m1 :: Maybe ()
      }
  deriving (Generic, Typeable)


instance FunctorT Record3
instance ApplicativeT Record3
instance TraversableT Record3
instance ConstraintsT Record3

deriving instance AllTF Show f Record3 => Show (Record3 f x)
deriving instance AllTF Eq   f Record3 => Eq   (Record3 f x)

instance AllTF Arbitrary f Record3 => Arbitrary (Record3 f x) where
  arbitrary = Record3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data Record3S f x
  = Record3S
      { rec3s_f1 :: !(f Int)
      , rec3s_f2 :: !(f Bool)
      , rec3s_f3 :: !(f Char)
      }
  deriving (Generic, Typeable)


instance FunctorT Record3S
instance DistributiveT Record3S
instance ApplicativeT Record3S
instance TraversableT Record3S
instance ConstraintsT Record3S

deriving instance AllTF Show f Record3S => Show (Record3S f x)
deriving instance AllTF Eq   f Record3S => Eq   (Record3S f x)

instance AllTF Arbitrary f Record3S => Arbitrary (Record3S f x) where
  arbitrary = Record3S <$> arbitrary <*> arbitrary <*> arbitrary

-----------------------------------------------------
-- Bad products
-----------------------------------------------------

data Ignore1 (f :: * -> *) (x :: kx)
  = Ignore1 { ign1_f1 :: Int }
  deriving (Generic, Typeable, Eq, Show)

instance FunctorT Ignore1
instance TraversableT Ignore1
instance ConstraintsT Ignore1

instance Arbitrary (Ignore1 f x) where arbitrary = Ignore1 <$> arbitrary


-----------------------------------------------------
-- Sums
-----------------------------------------------------

data Sum3 f x
  = Sum3_0
  | Sum3_1 (f Int)
  | Sum3_2 (f Int) (f Bool)
  deriving (Generic, Typeable)

instance FunctorT Sum3
instance TraversableT Sum3
instance ConstraintsT Sum3

deriving instance AllTF Show f Sum3 => Show (Sum3 f x)
deriving instance AllTF Eq   f Sum3 => Eq   (Sum3 f x)

instance AllTF Arbitrary f Sum3 => Arbitrary (Sum3 f x) where
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

instance FunctorT CompositeRecord
instance ApplicativeT CompositeRecord
instance TraversableT CompositeRecord
instance ConstraintsT CompositeRecord

deriving instance AllTF Show f CompositeRecord => Show (CompositeRecord f x)
deriving instance AllTF Eq   f CompositeRecord => Eq   (CompositeRecord f x)

instance AllTF Arbitrary f CompositeRecord => Arbitrary (CompositeRecord f x) where
  arbitrary
    = CompositeRecord <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


data SumRec f x
  = SumRec_0
  | SumRec_1 (f Int)
  | SumRec_2 (f Int) (SumRec f x)
  deriving (Generic, Typeable)

instance FunctorT SumRec
instance TraversableT SumRec
instance ConstraintsT SumRec

deriving instance AllTF Show f SumRec => Show (SumRec f x)
deriving instance AllTF Eq   f SumRec => Eq   (SumRec f x)

instance AllTF Arbitrary f SumRec => Arbitrary (SumRec f x) where
  arbitrary
    = oneof
        [ pure SumRec_0
        , SumRec_1 <$> arbitrary
        , SumRec_2 <$> arbitrary <*> arbitrary
        ]

data InfRec f x
  = InfRec { ir_1 :: f Int, ir_2 :: InfRec f x }
  deriving (Generic, Typeable)

instance FunctorT InfRec
instance ApplicativeT InfRec
instance TraversableT InfRec
instance ConstraintsT InfRec

deriving instance AllTF Show f InfRec => Show (InfRec f x)
deriving instance AllTF Eq   f InfRec => Eq   (InfRec f x)

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

instance FunctorT NestedF
instance ApplicativeT NestedF
instance TraversableT NestedF

deriving instance (Show (f Int), Show (Record3 f x)) => Show (NestedF f x)
deriving instance (Eq   (f Int), Eq   (Record3 f x)) => Eq   (NestedF f x)

instance (Arbitrary (f Int), AllTF Arbitrary f Record3, AllTF Arbitrary f Sum3) => Arbitrary (NestedF f x) where
  arbitrary
    = scale (`div` 2) $
        NestedF <$> arbitrary <*> scale (`div` 2) arbitrary <*> arbitrary


data Nested2F f x
  = Nested2F
    { np2f_1 :: f Int
    , np2f_2 :: [Maybe (Nested2F f x)]
    }
  deriving (Generic, Typeable)

instance FunctorT Nested2F
instance TraversableT Nested2F
instance ApplicativeT Nested2F

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

instance FunctorT b => FunctorT (ParB b)
instance DistributiveT b => DistributiveT (ParB b)
instance ApplicativeT b => ApplicativeT (ParB b)
instance TraversableT b => TraversableT (ParB b)
instance ConstraintsT b => ConstraintsT (ParB b)

data ParBH h b (f :: k -> *) (x :: kx)
  = ParBH (h (b f x))
  deriving (Generic, Typeable)

instance (Functor h, FunctorT b) => FunctorT (ParBH h b)
instance (Distributive h, DistributiveT b) => DistributiveT (ParBH h b)
instance (Applicative h, ApplicativeT b) => ApplicativeT (ParBH h b)
instance (Traversable h, TraversableT b) => TraversableT (ParBH h b)

data ParX a f x
  = ParX (f a) a
  deriving (Generic, Typeable)

instance FunctorT (ParX a)
instance Monoid a => ApplicativeT (ParX a)
instance TraversableT (ParX a)
instance ConstraintsT (ParX a)

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

instance FunctorT HKB
instance ApplicativeT HKB
instance TraversableT HKB
instance ConstraintsT HKB



-----------------------------------------------------
-- Actual bi-barbies
-----------------------------------------------------

type Record1' = TestBarbies.Record1

data NestedB f g
  = NestedB
      { nb_1 :: g Int
      , nb_2 :: f (g Bool)
      , nb_3 :: f (Record1' g)
      , nb_4 :: Record1' g
      }
  deriving (Generic, Typeable)

instance FunctorT NestedB
instance TraversableT NestedB
instance Functor f => FunctorB (NestedB f)
instance Distributive f => DistributiveB (NestedB f)
instance Applicative f => ApplicativeB (NestedB f)
instance Traversable f => TraversableB (NestedB f)


deriving instance (Show (f (g Bool)), AllBF Show g Record1', Show (f (Record1' g))) => Show (NestedB f g)
deriving instance (Eq (f (g Bool)), AllBF Eq g Record1', Eq (f (Record1' g))) => Eq (NestedB f g)


instance (Arbitrary (f (g Bool)), AllBF Arbitrary g Record1', Arbitrary (f (Record1' g))) => Arbitrary (NestedB f g) where
  arbitrary
    = NestedB <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data MixedBT f g
  = MixedBT
    { mx_1 :: f Int
    , mx_2 :: g Bool
    }
  deriving (Generic, Typeable)

instance FunctorT MixedBT
instance TraversableT MixedBT
instance ConstraintsT MixedBT

instance FunctorB (MixedBT f)
instance (Monoid (f Int)) => ApplicativeB (MixedBT f)
instance TraversableB (MixedBT f)
instance ConstraintsB (MixedBT f)

deriving instance (AllBF Show g (MixedBT f), AllTF Show f MixedBT) => Show (MixedBT f g)
deriving instance (AllBF Eq g (MixedBT f), AllTF Eq f MixedBT) => Eq (MixedBT f g)

instance (AllBF Arbitrary g (MixedBT f), AllTF Arbitrary f MixedBT) => Arbitrary (MixedBT f g) where
  arbitrary = MixedBT <$> arbitrary <*> arbitrary
