{-# LANGUAGE TypeApplications    #-}
import Test.Tasty (defaultMain, testGroup)

import qualified Spec.Bare as Bare
import qualified Spec.Constraints as Constraints
import qualified Spec.Functor as Functor
import qualified Spec.Product as Product
import qualified Spec.Traversable as Traversable
import qualified Spec.Wrapper as Wrapper


import Barbies

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

            , Functor.laws @Record1W
            , Functor.laws @Record3W

            , Functor.laws @Record1WS
            , Functor.laws @Record3WS

            , Functor.laws @Ignore1

            , Functor.laws @Sum3
            , Functor.laws @SumRec

            , Functor.laws @Sum3W
            , Functor.laws @SumRecW

            , Functor.laws @CompositeRecord
            , Functor.laws @NestedF

            , Functor.laws @CompositeRecordW
            ]

        , testGroup "Traversable Laws"
            [ Traversable.laws @Record0
            , Traversable.laws @Record1
            , Traversable.laws @Record3

            , Traversable.laws @Record1S
            , Traversable.laws @Record3S

            , Traversable.laws @Record1W
            , Traversable.laws @Record3W

            , Traversable.laws @Record1WS
            , Traversable.laws @Record3WS

            , Traversable.laws @Ignore1

            , Traversable.laws @Sum3
            , Traversable.laws @SumRec

            , Traversable.laws @Sum3W
            , Traversable.laws @SumRecW

            , Traversable.laws @CompositeRecord
            , Traversable.laws @NestedF

            , Traversable.laws @CompositeRecordW
            ]

        , testGroup "Product Laws"
            [ Product.laws @Record0
            , Product.laws @Record1
            , Product.laws @Record3
            , Product.laws @CompositeRecord

            , Product.laws @Record1S
            , Product.laws @Record3S

            , Product.laws @Record1W
            , Product.laws @Record3W
            , Product.laws @CompositeRecordW

            , Product.laws @Record1WS
            , Product.laws @Record3WS
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            , Product.uniqLaws @CompositeRecord

            , Product.uniqLaws @Record1S
            , Product.uniqLaws @Record3S

            , Product.uniqLaws @Record1W
            , Product.uniqLaws @Record3W
            , Product.uniqLaws @CompositeRecordW

            , Product.uniqLaws @Record1WS
            , Product.uniqLaws @Record3WS
            ]

        , testGroup "adjProof projection"
            [ Constraints.lawAdjProofPrj @Record0
            , Constraints.lawAdjProofPrj @Record1
            , Constraints.lawAdjProofPrj @Record3

            , Constraints.lawAdjProofPrj @Record1S
            , Constraints.lawAdjProofPrj @Record3S

            , Constraints.lawAdjProofPrj @Record1W
            , Constraints.lawAdjProofPrj @Record3W

            , Constraints.lawAdjProofPrj @Record1WS
            , Constraints.lawAdjProofPrj @Record3WS

            , Constraints.lawAdjProofPrj @Ignore1

            , Constraints.lawAdjProofPrj @Sum3
            , Constraints.lawAdjProofPrj @SumRec

            , Constraints.lawAdjProofPrj @Sum3W
            , Constraints.lawAdjProofPrj @SumRecW

            , Constraints.lawAdjProofPrj @CompositeRecord
            , Constraints.lawAdjProofPrj @CompositeRecordW
            ]

        , testGroup "bproof projection"
            [ Constraints.lawProofEquivPrj @Record0
            , Constraints.lawProofEquivPrj @Record1
            , Constraints.lawProofEquivPrj @Record3
            , Constraints.lawProofEquivPrj @CompositeRecord

            , Constraints.lawProofEquivPrj @Record1S
            , Constraints.lawProofEquivPrj @Record3S

            , Constraints.lawProofEquivPrj @Record1W
            , Constraints.lawProofEquivPrj @Record3W
            , Constraints.lawProofEquivPrj @CompositeRecordW

            , Constraints.lawProofEquivPrj @Record1WS
            , Constraints.lawProofEquivPrj @Record3WS
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
            , Wrapper.lawsMonoid @Record1W

            , Wrapper.lawsMonoid @Record1S
            , Wrapper.lawsMonoid @Record1WS

            , Wrapper.lawsMonoid @Record3
            , Wrapper.lawsMonoid @Record3W

            , Wrapper.lawsMonoid @Record3S
            , Wrapper.lawsMonoid @Record3WS
            ]
        ]
