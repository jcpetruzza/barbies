import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Spec.Bare as Bare
import qualified Spec.Constraints as Constraints
import qualified Spec.Functor as Functor
import qualified Spec.Applicative as Applicative
import qualified Spec.Traversable as Traversable
import qualified Spec.Wrapper as Wrapper

import TestBarbies
import TestBarbiesW

import Barbies.Bare(Covered)
import Data.Functor.Barbie(bfoldMap, bmapC, btraverseC, bpureC)
import Data.Functor.Const    (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Monoid           (Sum (..))

main :: IO ()
main
  = defaultMain $
      testGroup "Tests"
        [ testGroup "Functor Laws"
            [ Functor.laws @Record0
            , Functor.laws @Record1
            , Functor.laws @Record3

            , Functor.laws @Record1S
            , Functor.laws @Record3S

            , Functor.laws @(Record1W Covered)
            , Functor.laws @(Record3W Covered)

            , Functor.laws @(Record1WS Covered)
            , Functor.laws @(Record3WS Covered)

            , Functor.laws @Ignore1

            , Functor.laws @Sum3
            , Functor.laws @SumRec

            , Functor.laws @(Sum3W Covered)
            , Functor.laws @(SumRecW Covered)

            , Functor.laws @CompositeRecord
            , Functor.laws @NestedF

            , Functor.laws @(CompositeRecordW Covered)
            ]

        , testGroup "Traversable Laws"
            [ Traversable.laws @Record0
            , Traversable.laws @Record1
            , Traversable.laws @Record3

            , Traversable.laws @Record1S
            , Traversable.laws @Record3S

            , Traversable.laws @(Record1W Covered)
            , Traversable.laws @(Record3W Covered)

            , Traversable.laws @(Record1WS Covered)
            , Traversable.laws @(Record3WS Covered)

            , Traversable.laws @Ignore1

            , Traversable.laws @Sum3
            , Traversable.laws @SumRec

            , Traversable.laws @(Sum3W Covered)
            , Traversable.laws @(SumRecW Covered)

            , Traversable.laws @CompositeRecord
            , Traversable.laws @NestedF

            , Traversable.laws @(CompositeRecordW Covered)
            , Traversable.laws @(NestedFW Covered)
            ]

        , testGroup "Applicative laws"
            [ Applicative.laws @Record0
            , Applicative.laws @Record1
            , Applicative.laws @Record3
            , Applicative.laws @CompositeRecord
            , Applicative.laws @NestedF

            , Applicative.laws @Record1S
            , Applicative.laws @Record3S

            , Applicative.laws @(Record1W Covered)
            , Applicative.laws @(Record3W Covered)
            , Applicative.laws @(CompositeRecordW Covered)
            , Applicative.laws @(NestedFW Covered)

            , Applicative.laws @(Record1WS Covered)
            , Applicative.laws @(Record3WS Covered)
            ]

        , testGroup "adDict projection"
            [ Constraints.lawAddDictPrj @Record0
            , Constraints.lawAddDictPrj @Record1
            , Constraints.lawAddDictPrj @Record3

            , Constraints.lawAddDictPrj @Record1S
            , Constraints.lawAddDictPrj @Record3S

            , Constraints.lawAddDictPrj @(Record1W Covered)
            , Constraints.lawAddDictPrj @(Record3W Covered)

            , Constraints.lawAddDictPrj @(Record1WS Covered)
            , Constraints.lawAddDictPrj @(Record3WS Covered)

            , Constraints.lawAddDictPrj @Ignore1

            , Constraints.lawAddDictPrj @Sum3
            , Constraints.lawAddDictPrj @SumRec

            , Constraints.lawAddDictPrj @(Sum3W Covered)
            , Constraints.lawAddDictPrj @(SumRecW Covered)

            , Constraints.lawAddDictPrj @CompositeRecord
            , Constraints.lawAddDictPrj @(CompositeRecordW Covered)
            ]

        , testGroup "Bare laws"
            [ Bare.laws @Record1W
            , Bare.laws @Record3W
            , Bare.laws @Record1WS
            , Bare.laws @Record3WS
            , Bare.laws @Sum3W
            , Bare.laws @SumRecW
            , Bare.laws @NestedFW
            ]

        , testGroup "Generic wrapper"
            [ Wrapper.lawsMonoid @Record1
            , Wrapper.lawsMonoid @(Record1W Covered)

            , Wrapper.lawsMonoid @Record1S
            , Wrapper.lawsMonoid @(Record1WS Covered)

            , Wrapper.lawsMonoid @Record3
            , Wrapper.lawsMonoid @(Record3W Covered)

            , Wrapper.lawsMonoid @Record3S
            , Wrapper.lawsMonoid @(Record3WS Covered)
            ]

        , testGroup "bfoldMap"
            [ testCase "Record3" $ do
                let b = Record3 (Const "tic") (Const "tac") (Const "toe") Nothing
                bfoldMap getConst b @?= "tictactoe"
            ]
        , testGroup
          "bmapC"
          [ testCase "Record1" $
                bmapC @Num (fmap (+1)) (Record1 (Identity 0))
                    @?= Record1 (Identity 1)
          ]
        , testGroup
          "btraverseC"
          [ testCase "Record1" $
                btraverseC @Num (\inner -> (Sum @Int 1, fmap (+ 1) inner)) (Record1 (Identity 0))
                    @?= (Sum 1, Record1 (Identity 1))
          ]
        , testGroup
          "bpureC"
          [ testCase "Record1" $
                bpureC @Num (Identity (fromIntegral (42 :: Int)))
                    @?= Record1 (Identity 42)
          ]
        ]
