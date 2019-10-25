{-# LANGUAGE AllowAmbiguousTypes #-}
module Legacy.Spec.Functor ( laws )

where

import Legacy.Clothes (F, H, FG(..), GH(..), NatTransf(..))

import Data.Barbie (FunctorB(..))

import Data.Typeable (Typeable, typeRep, Proxy(..))

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))

laws
  :: forall b
  . ( FunctorB b
    , Eq (b F), Eq (b H)
    , Show (b F), Show (b H)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bmap id = id" $ \b ->
          bmap id b === (b :: b F)

      , testProperty "bmap (f . g) = bmap f . bmap g)" $
          \b (GH (NatTransf f)) (FG (NatTransf g)) ->
            bmap (f . g) b === (bmap f . bmap g) (b :: b F)
      ]
