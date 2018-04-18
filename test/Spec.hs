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
            ]

        , testGroup "Traversable Laws"
            [ Traversable.laws @Record0
            , Traversable.laws @Record1
            , Traversable.laws @Record3

            , Traversable.laws @Ignore1

            , Traversable.laws @Sum3
            ]

        , testGroup "Product Laws"
            [ Product.laws @Record0
            , Product.laws @Record1
            , Product.laws @Record3
            ]

        , testGroup "Uniq Laws"
            [ Product.uniqLaws @Record0
            , Product.uniqLaws @Record1
            , Product.uniqLaws @Record3
            ]
        ]


