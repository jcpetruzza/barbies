{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Product ( laws, uniqLaws )

where

import Clothes(F, G)

import Data.Barbie(FunctorB(..), ProductB(..))

import Data.Functor.Product(Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))


laws
  :: forall b
  . ( ProductB b
    , Eq (b F), Eq (b G)
    , Show (b F), Show (b G)
    , Arbitrary (b F), Arbitrary (b G)
    , Typeable b
    )
  => TestTree
laws
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \l r ->
      bmap first  (bprod l r) == (l :: b F) &&
      bmap second (bprod l r) == (r :: b G)
  where
    first  (Pair a _) = a
    second (Pair _ b) = b

uniqLaws
  :: forall b
  . ( ProductB b
    , Eq (b Maybe)
    , Show (b F), Show (b Maybe)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
uniqLaws
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap (const Nothing) (b :: b F) === buniq Nothing
