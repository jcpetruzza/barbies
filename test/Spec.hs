{-# LANGUAGE TypeApplications    #-}
import Test.Tasty (defaultMain, testGroup)

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

            , Functor.laws @Ignore1

            , Functor.laws @Sum3
            , Functor.laws @SumRec

            , Functor.laws @CompositeRecord
            , Functor.laws @NestedF
            ]

        , testGroup "Traversable Laws"
            [ Traversable.laws @Record0
            , Traversable.laws @Record1
            , Traversable.laws @Record3

            , Traversable.laws @Ignore1

            , Traversable.laws @Sum3
            , Traversable.laws @SumRec

            , Traversable.laws @CompositeRecord
            , Traversable.laws @NestedF
            ]

        , testGroup "Product Laws"
            [ Product.laws @Record0
            , Product.laws @Record1
            , Product.laws @Record3
            , Product.laws @CompositeRecord
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            , Product.uniqLaws @CompositeRecord
            ]
        ]


