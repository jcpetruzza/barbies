{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Distributive ( laws )

where

import Data.Functor.Identity (Identity(..))

import Data.Functor.Barbie (FunctorB(..), DistributiveB(..), bshape)

import Data.Typeable (Typeable, typeRep, Proxy(..))

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))

laws
  :: forall b
  . ( DistributiveB b
    , Eq (b Identity)
    , Show (b Identity)
    , Arbitrary (b Identity)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bmap (Identity . ($ b)) shape = b" $ \b ->
          bmap (\fd -> Identity (fd b)) bshape === (b :: b Identity)
      ]
