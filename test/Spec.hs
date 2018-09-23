import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Spec.Bare as Bare
import qualified Spec.Constraints as Constraints
import qualified Spec.Functor as Functor
import qualified Spec.Product as Product
import qualified Spec.Traversable as Traversable
import qualified Spec.Wrapper as Wrapper

import Barbies
import BarbiesW

import Data.Barbie (bfoldMap)
import Data.Barbie.Bare(Covered)
import Data.Functor.Const(Const(..))

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
            ]

        , testGroup "Product Laws"
            [ Product.laws @Record0
            , Product.laws @Record1
            , Product.laws @Record3
            , Product.laws @CompositeRecord

            , Product.laws @Record1S
            , Product.laws @Record3S

            , Product.laws @(Record1W Covered)
            , Product.laws @(Record3W Covered)
            , Product.laws @(CompositeRecordW Covered)

            , Product.laws @(Record1WS Covered)
            , Product.laws @(Record3WS Covered)
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            , Product.uniqLaws @CompositeRecord

            , Product.uniqLaws @Record1S
            , Product.uniqLaws @Record3S

            , Product.uniqLaws @(Record1W Covered)
            , Product.uniqLaws @(Record3W Covered)
            , Product.uniqLaws @(CompositeRecordW Covered)

            , Product.uniqLaws @(Record1WS Covered)
            , Product.uniqLaws @(Record3WS Covered)
            ]

        , testGroup "adjProof projection"
            [ Constraints.lawAdjProofPrj @Record0
            , Constraints.lawAdjProofPrj @Record1
            , Constraints.lawAdjProofPrj @Record3

            , Constraints.lawAdjProofPrj @Record1S
            , Constraints.lawAdjProofPrj @Record3S

            , Constraints.lawAdjProofPrj @(Record1W Covered)
            , Constraints.lawAdjProofPrj @(Record3W Covered)

            , Constraints.lawAdjProofPrj @(Record1WS Covered)
            , Constraints.lawAdjProofPrj @(Record3WS Covered)

            , Constraints.lawAdjProofPrj @Ignore1

            , Constraints.lawAdjProofPrj @Sum3
            , Constraints.lawAdjProofPrj @SumRec

            , Constraints.lawAdjProofPrj @(Sum3W Covered)
            , Constraints.lawAdjProofPrj @(SumRecW Covered)

            , Constraints.lawAdjProofPrj @CompositeRecord
            , Constraints.lawAdjProofPrj @(CompositeRecordW Covered)
            ]

        , testGroup "bproof projection"
            [ Constraints.lawProofEquivPrj @Record0
            , Constraints.lawProofEquivPrj @Record1
            , Constraints.lawProofEquivPrj @Record3
            , Constraints.lawProofEquivPrj @CompositeRecord

            , Constraints.lawProofEquivPrj @Record1S
            , Constraints.lawProofEquivPrj @Record3S

            , Constraints.lawProofEquivPrj @(Record1W Covered)
            , Constraints.lawProofEquivPrj @(Record3W Covered)
            , Constraints.lawProofEquivPrj @(CompositeRecordW Covered)

            , Constraints.lawProofEquivPrj @(Record1WS Covered)
            , Constraints.lawProofEquivPrj @(Record3WS Covered)
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
                let b = Record3 (Const "tic") (Const "tac") (Const "toe")
                bfoldMap getConst b @?= "tictactoe"
            ]
        ]
