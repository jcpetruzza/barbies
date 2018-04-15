import Test.Tasty (defaultMain)
import Spec.Functor as Functor

main :: IO ()
main
  = defaultMain
      Functor.properties


