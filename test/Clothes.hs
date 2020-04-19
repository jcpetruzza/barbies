{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
module Clothes

where

import Prelude hiding ((.), id)

import Control.Category
import Data.Functor.Classes (Eq1(..), Show1(..), liftShowsPrec2, showsUnaryWith)
import Data.Functor.Identity
import qualified Data.List.NonEmpty as NE
import Data.Typeable

import Test.Tasty.QuickCheck

data UnitF a = UnitF deriving(Eq, Show, Typeable)

data F a = F [a]
  deriving(Eq, Show, Typeable, Functor)

instance Eq1 F where
  liftEq eq (F as) (F bs) = liftEq eq as bs

instance Show1 F where
  liftShowsPrec sp sl d (F as)
    = showsUnaryWith (liftShowsPrec sp sl) "F" d as

data G a = NoG | G1 a | Gn [a]
  deriving(Eq, Show, Typeable, Functor)

instance Eq1 G where
  liftEq _  NoG     NoG     = True
  liftEq _  NoG     _       = False
  liftEq eq (G1 a)  (G1 b)  = a `eq` b
  liftEq _  (G1 _)  _       = False
  liftEq eq (Gn as) (Gn bs) = liftEq eq as bs
  liftEq _  (Gn _ ) _       = False

instance Show1 G where
  liftShowsPrec sp sl d = \case
    NoG   -> showString "NoG"
    G1 a  -> showsUnaryWith sp "G1" d a
    Gn as -> showsUnaryWith (liftShowsPrec sp sl) "Gn" d as

data H a = NoH1 | NoH2 | H1 [a] | H2 [a] | H3 [a]
  deriving(Eq, Show, Typeable, Functor)

instance Show1 H where
  liftShowsPrec sp sl d = \case
    NoH1  -> showString "NoH1"
    NoH2  -> showString "NoH2"
    H1 as -> showsUnaryWith (liftShowsPrec sp sl) "H1" d as
    H2 as -> showsUnaryWith (liftShowsPrec sp sl) "H2" d as
    H3 as -> showsUnaryWith (liftShowsPrec sp sl) "H3" d as

instance Eq1 H where
  liftEq _  NoH1    NoH1    = True
  liftEq _  NoH1    _       = False
  liftEq _  NoH2    NoH2    = True
  liftEq _  NoH2    _       = False
  liftEq eq (H1 as) (H1 bs) = liftEq eq as bs
  liftEq _  (H1 _ ) _       = False
  liftEq eq (H2 as) (H2 bs) = liftEq eq as bs
  liftEq _  (H2 _ ) _       = False
  liftEq eq (H3 as) (H3 bs) = liftEq eq as bs
  liftEq _  (H3 _ ) _       = False

data I a = NoI1 | NoI2 | NoI3 | I1 a | I2 (a,a)
  deriving(Eq, Show, Typeable)

instance Show1 I where
  liftShowsPrec sp sl d = \case
    NoI1  -> showString "NoI1"
    NoI2  -> showString "NoI2"
    NoI3  -> showString "NoI3"
    I1 a  -> showsUnaryWith sp "I1" d a
    I2 aa -> showsUnaryWith (liftShowsPrec2 sp sl sp sl) "I2" d aa

instance Eq1 I where
  liftEq _  NoI1        NoI1      = True
  liftEq _  NoI1        _         = False
  liftEq _  NoI2        NoI2      = True
  liftEq _  NoI2        _         = False
  liftEq _  NoI3        NoI3      = True
  liftEq _  NoI3        _         = False
  liftEq eq (I1 a)      (I1 b)    = a `eq` b
  liftEq _  (I1 _ )     _         = False
  liftEq eq (I2 (a,b)) (I2 (c,d)) = (a `eq` c) && (b `eq` d)
  liftEq _  (I2 _ )    _          = False


instance Arbitrary a => Arbitrary (F a) where
  arbitrary
    = scale (`div` 2) $
        F <$> arbitrary

instance Arbitrary a => Arbitrary (G a) where
  arbitrary
    = scale (`div` 2) $
        oneof
          [ pure NoG
          , G1 <$> arbitrary
          , Gn <$> arbitrary
          ]

instance Arbitrary a => Arbitrary (H a) where
  arbitrary
    = scale (`div` 2) $
        oneof
          [ pure NoH1
          , pure NoH2
          , H1 <$> arbitrary
          , H2 <$> arbitrary
          , H3 <$> arbitrary
          ]

instance Arbitrary a => Arbitrary (I a) where
  arbitrary
    = scale (`div` 2) $
        oneof
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
    = scale (`div` 2) $
        do fromList <- arbitraryf
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
