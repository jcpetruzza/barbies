import Test.Tasty (defaultMain, testGroup)

import qualified Spec.Functor as Functor
import qualified Spec.Traversable as Traversable

main :: IO ()
main
  = defaultMain $
      testGroup "Tests"
        [ Functor.properties
        , Traversable.properties
        ]


