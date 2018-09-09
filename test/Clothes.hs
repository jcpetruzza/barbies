{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Clothes

where

import Prelude hiding ((.), id)

import Control.Category
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import Data.Typeable

import Test.Tasty.QuickCheck

data UnitF a = UnitF deriving(Eq, Show, Typeable)

data F a = F [a]
  deriving(Eq, Show, Typeable)

data G a = NoG | G1 a | Gn [a]
  deriving(Eq, Show, Typeable)

data H a = NoH1 | NoH2 | H1 [a] | H2 [a] | H3 [a]
  deriving(Eq, Show, Typeable)

data I a = NoI1 | NoI2 | NoI3 | I1 a | I2 (a,a)
  deriving(Eq, Show, Typeable)


instance Arbitrary a => Arbitrary (F a) where
  arbitrary = F <$> arbitrary

instance Arbitrary a => Arbitrary (G a) where
  arbitrary = oneof
    [ pure NoG
    , G1 <$> arbitrary
    , Gn <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (H a) where
  arbitrary = oneof
    [ pure NoH1
    , pure NoH2
    , H1 <$> arbitrary
    , H2 <$> arbitrary
    , H3 <$> arbitrary
    ]

instance Arbitrary a => Arbitrary (I a) where
  arbitrary = oneof
    [ pure NoI1
    , pure NoI2
    , pure NoI3
    , I1 <$> arbitrary
    , I2 <$> arbitrary
    ]

newtype NatTransf f g
  = NatTransf {applyNat :: (forall a . f a -> g a)}


instance Category NatTransf where
  id    = NatTransf id
  f . g = NatTransf (applyNat f . applyNat g)

point :: (forall a . a -> f a) -> NatTransf Identity f
point mkPoint
  = NatTransf (\(Identity a) -> mkPoint a)

unit :: (forall a . f a) -> NatTransf UnitF f
unit u
  = NatTransf (\UnitF -> u)

headF :: NatTransf NE.NonEmpty Identity
headF
  = NatTransf (\(a NE.:| _) -> Identity a)

terminal :: NatTransf f UnitF
terminal
  = NatTransf (const UnitF)


instance (ArbitraryF f, ArbitraryF g) => Arbitrary (NatTransf f g) where
  arbitrary
    = do fromList <- arbitraryf
         pure (fromList . flattenf)


class ArbitraryF f where
  arbitraryf :: Gen (NatTransf [] f)
  flattenf   :: NatTransf f []


instance ArbitraryF F where
  arbitraryf
    = pure $ NatTransf F

  flattenf
    = NatTransf (\(F as) -> as)


instance ArbitraryF G where
  arbitraryf
    = mkArbitraryf
        [unit NoG]
        [point G1 , point (Gn . pure)]
        [NatTransf (Gn . NE.toList)]

  flattenf
    = NatTransf $ \case
        NoG   -> []
        G1 a  -> [a]
        Gn as -> as


instance ArbitraryF H where
  arbitraryf
    = mkArbitraryf
        [unit NoH1, unit NoH2]
        [point (H1 . pure), point (H2 . pure)]
        [ NatTransf (H1 . NE.toList)
        , NatTransf (H2 . NE.toList)
        , NatTransf (H2 . NE.toList)
        ]

  flattenf
    = NatTransf $ \case
        NoH1  -> []
        NoH2  -> []
        H1 as -> as
        H2 as -> as
        H3 as -> as

instance ArbitraryF I where
  arbitraryf
    = mkArbitraryf
        [unit NoI1, unit NoI2, unit NoI3]
        [point I1, NatTransf (\(Identity a) -> I2 (a, a))]
        [ NatTransf mkI2 ]
    where
      mkI2 = \case
        a NE.:| []    -> I2 (a, a)
        a NE.:| (b:_) -> I2 (a, b)

  flattenf
    = NatTransf $ \case
        NoI1     -> []
        NoI2     -> []
        NoI3     -> []
        I1 a     -> [a]
        I2 (a,b) -> [a,b]

mkArbitraryf
  :: [NatTransf UnitF f]
  -> [NatTransf Identity f]
  -> [NatTransf NE.NonEmpty f]
  -> Gen (NatTransf [] f)
mkArbitraryf us is ls
  = do let nullary = us
           unary   = is ++ map (. terminal) nullary
           nary    = ls ++ map (. headF) unary
       build <$> elements nullary <*> elements unary <*> elements nary
  where
    build u i l
      = NatTransf $ \case
          []   -> applyNat u UnitF
          [a]  -> applyNat i (Identity a)
          a:as -> applyNat l (a NE.:| as)

newtype FG
  = FG (NatTransf F G)
  deriving (Arbitrary)

newtype GH
  = GH (NatTransf G H)
  deriving (Arbitrary)

newtype HI
  = HI (NatTransf H I)
  deriving (Arbitrary)

instance Show FG
  where show _ = "<natural-transformation :: F -> G>"

instance Show GH
  where show _ = "<natural-transformation :: G -> H>"

instance Show HI
  where show _ = "<natural-transformation :: H -> I>"
