{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Applicative
 ( laws
 )

where

import Clothes(F(..), G, H, I, FG(..), HI(..), NatTransf(..))

import Data.Functor.Barbie(FunctorB(..), ApplicativeB(..))

import Data.Functor.Product(Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree, testGroup)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))

laws
  :: forall b
  .  ( ApplicativeB b
     , Eq (b F), Eq (b (G `Product` I)), Eq (b ((F `Product` G) `Product` H))
     , Show (b F), Show (b G), Show (b H)
     , Show (b (G `Product` I)), Show (b ((F `Product` G) `Product` H))
     , Arbitrary (b F), Arbitrary (b G), Arbitrary (b H)
     , Typeable b
     )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy @b)))
      [ testProperty "naturality of bprod" $
          \(FG (NatTransf f)) (HI (NatTransf g)) l r ->
            let
              lhs, rhs :: b F -> b H -> b (G `Product` I)
              lhs u v = bmap (\(Pair a b) -> Pair (f a) (g b)) (u `bprod` v)
              rhs u v = bmap f u `bprod` bmap g v
            in
              lhs l r === rhs l r

      , testProperty "left identity" $ \u ->
          bmap (\(Pair _ b) -> b) (bpure (F []) `bprod` u) === (u :: b F)

      , testProperty "left identity" $ \u ->
          bmap (\(Pair a _) -> a) (u `bprod` bpure (F [])) === (u :: b F)

      , testProperty "associativity" $ \u v w ->
          let
            assocPair (Pair a (Pair b c))
              = Pair (Pair a b) c

            lhs, rhs :: b ((F `Product` G) `Product` H)
            lhs = bmap assocPair (u `bprod` (v `bprod` w))
            rhs = (u `bprod` v) `bprod` w
          in
            lhs === rhs
      ]
