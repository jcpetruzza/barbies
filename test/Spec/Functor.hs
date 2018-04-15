{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Functor
  (
    properties
  )

where

import Barbies
import Clothes

import Data.Barbie

import Data.Typeable

import Test.Tasty
import Test.Tasty.QuickCheck

properties :: TestTree
properties
  = testGroup "Functor Laws"
      [ functorLaws @Record0
      , functorLaws @Record1
      , functorLaws @Record3

      , functorLaws @Ignore1

      , functorLaws @Sum3
      ]

functorLaws
  :: forall b
  . (FunctorB b, Eq (b F), Eq (b H), Show (b F), Arbitrary (b F), Typeable b)
  => TestTree
functorLaws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bmap id = id" $ \b ->
          bmap id b == (b :: b F)

      , testProperty "bmap (f . g) = bmap f . bmap g)" $ \b ->
          bmap (gh . fg) b == (bmap gh . bmap fg) (b :: b F)
      ]
