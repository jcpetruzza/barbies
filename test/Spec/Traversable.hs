{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Traversable
  (
    properties
  )

where

import Barbies
import Clothes

import Data.Barbie

import Data.Functor.Identity
import Data.Functor.Compose
import Data.Maybe (maybeToList)
import Data.Typeable

import Test.Tasty
import Test.Tasty.QuickCheck

properties :: TestTree
properties
  = testGroup "Traversable Laws"
      [ traversableLaws @Record0
      , traversableLaws @Record1
      , traversableLaws @Record3

      , traversableLaws @Ignore1

      , traversableLaws @Sum3
      ]

traversableLaws
  :: forall b
  . (TraversableB b, Eq (b F), Eq (b G), Eq (b H), Show (b F), Arbitrary (b F), Typeable b)
  => TestTree
traversableLaws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [testProperty "naturality" $ \b ->
         let f = Just . fg
             t = maybeToList
         in (t . btraverse f) (b :: b F) == btraverse (t . f) (b :: b F)

      , testProperty "identity" $ \b ->
          btraverse Identity b == Identity (b :: b F)

      , testProperty "composition" $ \b ->
          let f x = Just (fg x)
              g x = [gh x]
          in btraverse (Compose . fmap g . f) b ==
               (Compose . fmap (btraverse g) . btraverse f) (b :: b F)
      ]

