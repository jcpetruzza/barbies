import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Spec.Bare as Bare
import qualified Spec.Constraints as Constraints
import qualified Spec.Functor as Functor
import qualified Spec.Product as Product
import qualified Spec.Traversable as Traversable
import qualified Spec.Wrapper as Wrapper

import Barbies
import qualified BarbiesWDeprecated as Deprecated

import Data.Barbie (bfoldMap)
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

            , Functor.laws @Deprecated.Record1W
            , Functor.laws @Deprecated.Record3W

            , Functor.laws @Deprecated.Record1WS
            , Functor.laws @Deprecated.Record3WS

            , Functor.laws @Ignore1

            , Functor.laws @Sum3
            , Functor.laws @SumRec

            , Functor.laws @Deprecated.Sum3W
            , Functor.laws @Deprecated.SumRecW

            , Functor.laws @CompositeRecord
            , Functor.laws @NestedF

            , Functor.laws @Deprecated.CompositeRecordW
            ]

        , testGroup "Traversable Laws"
            [ Traversable.laws @Record0
            , Traversable.laws @Record1
            , Traversable.laws @Record3

            , Traversable.laws @Record1S
            , Traversable.laws @Record3S

            , Traversable.laws @Deprecated.Record1W
            , Traversable.laws @Deprecated.Record3W

            , Traversable.laws @Deprecated.Record1WS
            , Traversable.laws @Deprecated.Record3WS

            , Traversable.laws @Ignore1

            , Traversable.laws @Sum3
            , Traversable.laws @SumRec

            , Traversable.laws @Deprecated.Sum3W
            , Traversable.laws @Deprecated.SumRecW

            , Traversable.laws @CompositeRecord
            , Traversable.laws @NestedF

            , Traversable.laws @Deprecated.CompositeRecordW
            ]

        , testGroup "Product Laws"
            [ Product.laws @Record0
            , Product.laws @Record1
            , Product.laws @Record3
            , Product.laws @CompositeRecord

            , Product.laws @Record1S
            , Product.laws @Record3S

            , Product.laws @Deprecated.Record1W
            , Product.laws @Deprecated.Record3W
            , Product.laws @Deprecated.CompositeRecordW

            , Product.laws @Deprecated.Record1WS
            , Product.laws @Deprecated.Record3WS
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            , Product.uniqLaws @CompositeRecord

            , Product.uniqLaws @Record1S
            , Product.uniqLaws @Record3S

            , Product.uniqLaws @Deprecated.Record1W
            , Product.uniqLaws @Deprecated.Record3W
            , Product.uniqLaws @Deprecated.CompositeRecordW

            , Product.uniqLaws @Deprecated.Record1WS
            , Product.uniqLaws @Deprecated.Record3WS
            ]

        , testGroup "adjProof projection"
            [ Constraints.lawAdjProofPrj @Record0
            , Constraints.lawAdjProofPrj @Record1
            , Constraints.lawAdjProofPrj @Record3

            , Constraints.lawAdjProofPrj @Record1S
            , Constraints.lawAdjProofPrj @Record3S

            , Constraints.lawAdjProofPrj @Deprecated.Record1W
            , Constraints.lawAdjProofPrj @Deprecated.Record3W

            , Constraints.lawAdjProofPrj @Deprecated.Record1WS
            , Constraints.lawAdjProofPrj @Deprecated.Record3WS

            , Constraints.lawAdjProofPrj @Ignore1

            , Constraints.lawAdjProofPrj @Sum3
            , Constraints.lawAdjProofPrj @SumRec

            , Constraints.lawAdjProofPrj @Deprecated.Sum3W
            , Constraints.lawAdjProofPrj @Deprecated.SumRecW

            , Constraints.lawAdjProofPrj @CompositeRecord
            , Constraints.lawAdjProofPrj @Deprecated.CompositeRecordW
            ]

        , testGroup "bproof projection"
            [ Constraints.lawProofEquivPrj @Record0
            , Constraints.lawProofEquivPrj @Record1
            , Constraints.lawProofEquivPrj @Record3
            , Constraints.lawProofEquivPrj @CompositeRecord

            , Constraints.lawProofEquivPrj @Record1S
            , Constraints.lawProofEquivPrj @Record3S

            , Constraints.lawProofEquivPrj @Deprecated.Record1W
            , Constraints.lawProofEquivPrj @Deprecated.Record3W
            , Constraints.lawProofEquivPrj @Deprecated.CompositeRecordW

            , Constraints.lawProofEquivPrj @Deprecated.Record1WS
            , Constraints.lawProofEquivPrj @Deprecated.Record3WS
            ]

        , testGroup "Bare laws"
            [ Bare.laws @Deprecated.Record1W
            , Bare.laws @Deprecated.Record3W
            , Bare.laws @Deprecated.Record1WS
            , Bare.laws @Deprecated.Record3WS
            , Bare.laws @Deprecated.Sum3W
            , Bare.laws @Deprecated.SumRecW
            , Bare.laws @Deprecated.NestedFW
            ]

        , testGroup "Generic wrapper"
            [ Wrapper.lawsMonoid @Record1
            , Wrapper.lawsMonoid @Deprecated.Record1W

            , Wrapper.lawsMonoid @Record1S
            , Wrapper.lawsMonoid @Deprecated.Record1WS

            , Wrapper.lawsMonoid @Record3
            , Wrapper.lawsMonoid @Deprecated.Record3W

            , Wrapper.lawsMonoid @Record3S
            , Wrapper.lawsMonoid @Deprecated.Record3WS
            ]

        , testGroup "bfoldMap"
            [ testCase "Record3" $ do
                let b = Record3 (Const "tic") (Const "tac") (Const "toe")
                bfoldMap getConst b @?= "tictactoe"
            ]
        ]
