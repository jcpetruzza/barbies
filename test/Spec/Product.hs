{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Product
  (
    properties
  )

where

import Barbies
import Clothes

import Data.Barbie

import Data.Functor.Product(Product(Pair))
import Data.Typeable

import Test.Tasty
import Test.Tasty.QuickCheck

properties :: TestTree
properties
  = testGroup "Product Laws"
      [ testGroup "Product"
          [ productLaws @Record0
          , productLaws @Record1
          , productLaws @Record3
          ]

      , testGroup "Unique"
          [ uniqLaws @Record0
          , uniqLaws @Record1
          , uniqLaws @Record3
          ]
      ]

productLaws
  :: forall b
  . ( ProductB b
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

uniqLaws
  :: forall b
  . ( ProductB b
    , Eq (b Maybe), Show (b F)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
uniqLaws
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap (const Nothing) (b :: b F) == buniq Nothing
