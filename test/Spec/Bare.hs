{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Spec.Bare ( laws )

where

import Data.Barbie (BareB(..))
import Data.Functor.Identity

import Data.Typeable (Typeable, typeRep, Proxy(..))

import Test.Tasty(testGroup, TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))

laws
  :: forall b
  . ( BareB b
    , Eq (b Identity) , Show (b Identity) , Arbitrary (b Identity)
    -- , Show (b Bare), Eq (b Bare), Arbitrary (b Bare)
    , Typeable b
    )
  => TestTree
laws
  = testGroup (show (typeRep (Proxy :: Proxy b)))
      [ testProperty "bcover . bstrip = id" $ \b ->
          bcover (bstrip b) === (b :: b Identity)

      -- TODO: FIXME
      -- , testProperty "bstrip . bcover = id" $ \b ->
      --     bstrip (bcover b) === (b :: b Bare)
      ]
