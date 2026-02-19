{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Spec.Method
  ( methodTests
  )

where

import Barbies.Constraints (Dict (..))
import Data.Functor.Barbie (FunctorB(..), ApplicativeB(..), ConstraintsB(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(Pair))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import TestMethodBarbies


-- | Tests for method record support
methodTests :: TestTree
methodTests = testGroup "Method Records"
    [ testGroup "FunctorB"
        [ testCase "bmap id on MethodRecord1" $ do
            let record = MethodRecord1 { mr1_method = \n -> Identity n }
                mapped = bmap id record
            runIdentity (mr1_method mapped 5) @?= 5

        , testCase "bmap id on MethodRecord2" $ do
            let record = MethodRecord2
                  { mr2_method1 = \n -> Identity n
                  , mr2_method2 = \b -> Identity b
                  }
                mapped = bmap id record
            runIdentity (mr2_method1 mapped 42) @?= 42
            runIdentity (mr2_method2 mapped True) @?= True

        , testCase "bmap id on MixedRecord" $ do
            let record = MixedRecord
                  { mixed_regular = Identity 10
                  , mixed_method  = \n -> Identity n
                  }
                mapped = bmap id record
            runIdentity (mixed_regular mapped) @?= 10
            runIdentity (mixed_method mapped 5) @?= 5

        , testCase "bmap (natural transformation) on MethodRecord1" $ do
            -- Use a simple natural transformation: Identity -> []
            let record = MethodRecord1 { mr1_method = \n -> Identity n }
                listify :: forall a. Identity a -> [] a
                listify (Identity x) = [x]
                mapped = bmap listify record
            mr1_method mapped 5 @?= [5]

        , testCase "bmap composition law (MethodRecord1)" $ do
            let record = MethodRecord1 { mr1_method = \n -> Identity n }
                -- Use polymorphic transformations
                f :: forall a. Identity a -> [a]
                f (Identity x) = [x]
                g :: forall a. [a] -> Maybe a
                g [] = Nothing
                g (x:_) = Just x
                lhs = bmap (g . f) record
                rhs = (bmap g . bmap f) record
            mr1_method lhs 5 @?= mr1_method rhs 5
        ]

    , testGroup "ApplicativeB"
        [ testCase "bpure on MethodRecord1" $ do
            let record :: MethodRecord1 Maybe
                record = bpure Nothing
            mr1_method record 999 @?= Nothing

        , testCase "bpure on MixedRecord" $ do
            let record :: MixedRecord Maybe
                record = bpure Nothing
            mixed_regular record @?= Nothing
            mixed_method record 999 @?= Nothing

        , testCase "bprod on MethodRecord1" $ do
            let r1 = MethodRecord1 { mr1_method = \n -> Identity n }
                r2 = MethodRecord1 { mr1_method = \n -> Identity (n * 2) }
                prod = r1 `bprod` r2
            case mr1_method prod 5 of
              Pair (Identity a) (Identity b) -> do
                a @?= 5
                b @?= 10

        , testCase "bprod on MixedRecord" $ do
            let r1 = MixedRecord
                  { mixed_regular = Identity 10
                  , mixed_method  = \n -> Identity n
                  }
                r2 = MixedRecord
                  { mixed_regular = Identity 20
                  , mixed_method  = \n -> Identity (n * 2)
                  }
                prod = r1 `bprod` r2
            case mixed_regular prod of
              Pair (Identity a) (Identity b) -> do
                a @?= 10
                b @?= 20
            case mixed_method prod 5 of
              Pair (Identity a) (Identity b) -> do
                a @?= 5
                b @?= 10

        , testCase "bprod left identity" $ do
            let r = MethodRecord1 { mr1_method = \n -> Identity n }
                lhs = bmap (\(Pair _ b) -> b) (bpure Nothing `bprod` r)
            mr1_method lhs 42 @?= mr1_method r 42
        ]

    , testGroup "ConstraintsB"
        [ testCase "baddDicts on MethodRecord1" $ do
            let record = MethodRecord1 { mr1_method = \n -> Identity n }
                withDicts = baddDicts record :: MethodRecord1 (Product (Dict Show) Identity)
            case mr1_method withDicts 42 of
              Pair Dict (Identity v) -> v @?= 42

        , testCase "baddDicts on MixedRecord" $ do
            let record = MixedRecord
                  { mixed_regular = Identity (10 :: Int)
                  , mixed_method  = \n -> Identity n
                  }
                withDicts = baddDicts record :: MixedRecord (Product (Dict Show) Identity)
            case mixed_regular withDicts of
              Pair Dict (Identity v) -> v @?= 10
            case mixed_method withDicts 42 of
              Pair Dict (Identity v) -> v @?= 42

        , testCase "baddDicts projection law (MethodRecord1)" $ do
            let record = MethodRecord1 { mr1_method = \n -> Identity n }
                withDicts = baddDicts record :: MethodRecord1 (Product (Dict Show) Identity)
                projected = bmap (\(Pair _ b) -> b) withDicts
            runIdentity (mr1_method projected 5) @?= runIdentity (mr1_method record 5)
        ]
    ]
