{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Bare ( laws )

where

import Data.Barbie (BareB(..), Bare)
import Data.Functor.Identity

import Data.Typeable (Typeable, typeRep, Proxy(..))

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))

laws
  :: forall b
  . ( BareB b
    , Eq (b Bare), Eq (b Identity)
    , Show (b Bare), Show (b Identity)
    , Arbitrary (b Bare), Arbitrary (b Identity)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bstrip . bcover = id" $ \b ->
          bstrip (bcover b) === (b :: b Bare)

      , testProperty "bcover . bstrip = id" $ \b ->
          bcover (bstrip b) === (b :: b Identity)
      ]
