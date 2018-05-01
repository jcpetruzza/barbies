{-# LANGUAGE TypeApplications    #-}
import Test.Tasty (defaultMain, testGroup)

import qualified Spec.Bare as Bare
import qualified Spec.Constraints as Constraints
import qualified Spec.Functor as Functor
import qualified Spec.Product as Product
import qualified Spec.Traversable as Traversable


import Barbies

main :: IO ()
main
  = defaultMain $
      testGroup "Tests"
        [ testGroup "Functor Laws"
            [ Functor.laws @Record0
            , Functor.laws @Record1
            , Functor.laws @Record3

            , Functor.laws @Record1W
            , Functor.laws @Record3W

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

            , Traversable.laws @Record1W
            , Traversable.laws @Record3W

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

            , Product.laws @Record1W
            , Product.laws @Record3W
            , Product.laws @CompositeRecordW
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            , Product.uniqLaws @CompositeRecord

            , Product.uniqLaws @Record1W
            , Product.uniqLaws @Record3W
            , Product.uniqLaws @CompositeRecordW
            ]

        , testGroup "adjProof projection"
            [ Constraints.lawAdjProofPrj @Record0
            , Constraints.lawAdjProofPrj @Record1
            , Constraints.lawAdjProofPrj @Record3



            , Constraints.lawAdjProofPrj @Ignore1

            , Constraints.lawAdjProofPrj @Sum3
            , Constraints.lawAdjProofPrj @SumRec


            , Constraints.lawAdjProofPrj @CompositeRecord
            ]

        , testGroup "bproof projection"
            [ Constraints.lawProofEquivPrj @Record0
            , Constraints.lawProofEquivPrj @Record1
            , Constraints.lawProofEquivPrj @Record3
            , Constraints.lawProofEquivPrj @CompositeRecord

            ]

        , testGroup "Bare laws"
            [ Bare.laws @Record1W
            , Bare.laws @Record3W
            , Bare.laws @Sum3W
            , Bare.laws @SumRecW
            , Bare.laws @NestedFW
            ]
        ]
