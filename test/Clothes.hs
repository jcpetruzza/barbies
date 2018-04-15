{-# LANGUAGE DeriveDataTypeable #-}
module Clothes

where

import Data.Typeable
import Test.Tasty.QuickCheck

data F a = F a deriving(Eq, Show, Typeable)
data G a = G a deriving(Eq, Show, Typeable)
data H a = H a deriving(Eq, Show, Typeable)
data I a = I a deriving(Eq, Show, Typeable)

fg :: F a -> G a
fg (F a) = G a

gh :: G a -> H a
gh (G a) = H a

hi :: H a -> I a
hi (H a) = I a

instance Arbitrary a => Arbitrary (F a) where arbitrary = F <$> arbitrary
instance Arbitrary a => Arbitrary (G a) where arbitrary = G <$> arbitrary
instance Arbitrary a => Arbitrary (H a) where arbitrary = H <$> arbitrary
instance Arbitrary a => Arbitrary (I a) where arbitrary = I <$> arbitrary

instance CoArbitrary a => CoArbitrary (F a) where coarbitrary (F a) = coarbitrary a
instance CoArbitrary a => CoArbitrary (G a) where coarbitrary (G a) = coarbitrary a
instance CoArbitrary a => CoArbitrary (H a) where coarbitrary (H a) = coarbitrary a
instance CoArbitrary a => CoArbitrary (I a) where coarbitrary (I a) = coarbitrary a
