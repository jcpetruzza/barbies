{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Applicative
 ( productLaws, uniqLaws )

where

import Clothes(F, G)

import Data.Functor.Barbie(FunctorB(..), ApplicativeB(..))

import Data.Functor.Product(Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))


-- We only derive ApplicativeB for products, so we test the product
-- laws, which are stronger than the applicative ones.
productLaws
  :: forall b
  . ( ApplicativeB b
    , Eq (b F), Eq (b G)
    , Show (b F), Show (b G)
    , Arbitrary (b F), Arbitrary (b G)
    , Typeable b
    )
  => TestTree
productLaws
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \l r ->
      bmap first  (bprod l r) == (l :: b F) &&
      bmap second (bprod l r) == (r :: b G)
  where
    first  (Pair a _) = a
    second (Pair _ b) = b

-- `bpure` is uniquely determined in products
uniqLaws
  :: forall b
  . ( ApplicativeB b
    , Eq (b Maybe)
    , Show (b F), Show (b Maybe)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
uniqLaws
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap (const Nothing) (b :: b F) === bpure Nothing
