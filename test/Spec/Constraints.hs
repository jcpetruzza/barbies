{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Constraints ( lawAdjProofPrj, lawProofEquivPrj )

where

import Clothes(F)
import Data.Barbie(bmap, ConstraintsB(..), ProofB(..))
import Data.Barbie.Constraints(DictOf)

import Data.Functor.Product (Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))


lawAdjProofPrj
  :: forall b
  . ( ConstraintsB b, ConstraintsOf Show F b
    , Eq (b F)
    , Show (b F)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawAdjProofPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap second (adjProof b :: b (Product (DictOf Show F) F)) === b
  where
    second (Pair _ b) = b


lawProofEquivPrj
  :: forall b
  . ( ProofB b, ConstraintsOf Show F b
    , Eq (b (DictOf Show F))
    , Show (b F), Show (b (DictOf Show F))
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawProofEquivPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap first (adjProof b :: b (Product (DictOf Show F) F)) === bproof
  where
    first (Pair a _) = a
