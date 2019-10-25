{-# LANGUAGE AllowAmbiguousTypes #-}
module Legacy.Spec.Constraints
  ( lawAddDictPrj
  , lawDictsEquivPrj
  )

where

import Legacy.Clothes(F)
import Data.Barbie(bmap, ConstraintsB(..), AllBF, ProductBC(..))
import Data.Barbie.Constraints(ClassF, Dict)

import Data.Functor.Product (Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))


lawAddDictPrj
  :: forall b
  . ( ConstraintsB b, AllBF Show F b
    , Eq (b F)
    , Show (b F)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawAddDictPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap second (baddDicts b :: b (Dict (ClassF Show F) `Product` F)) === b
  where
    second (Pair _ b) = b


lawDictsEquivPrj
  :: forall b
  . ( ProductBC b, AllBF Show F b
    , Eq (b (Dict (ClassF Show F)))
    , Show (b F), Show (b (Dict (ClassF Show F)))
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawDictsEquivPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap first (baddDicts b :: b (Dict (ClassF Show F) `Product` F)) === bdicts
  where
    first (Pair a _) = a
