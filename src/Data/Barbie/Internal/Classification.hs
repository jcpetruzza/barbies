{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Barbie.Internal.Classification
  ( BarbieType(..)
  , GClassifyBarbie
  , ClassifyBarbie
  )

where

import Data.Barbie.Internal.Generics(Target, RecUsage(..), NonRec(..), RecRep, W)
import Data.Barbie.Internal.Tags(F)

import GHC.Generics

data BarbieType
  = NoBarbie      -- ^ The parameter is never used.
  | WearBarbie    -- ^ The parameter is used, and always under a 'Wear'.
  | NonWearBarbie -- ^ The parameter is used, never under a 'Wear'.
  | MixedBarbie   -- ^ THe parameter is used, sometimes under a 'Wear', somtimes not.

type family MergeBarbieType l r where
  MergeBarbieType 'NoBarbie r = r
  MergeBarbieType l 'NoBarbie = l

  MergeBarbieType 'MixedBarbie _ = 'MixedBarbie
  MergeBarbieType _ 'MixedBarbie = 'MixedBarbie

  MergeBarbieType x x = x
  MergeBarbieType _l _r = 'MixedBarbie

type family GClassifyBarbie rep where
  GClassifyBarbie (M1 _i _c x) = GClassifyBarbie x
  GClassifyBarbie V1 = 'NoBarbie
  GClassifyBarbie U1 = 'NoBarbie
  GClassifyBarbie (l :*: r) = MergeBarbieType (GClassifyBarbie l) (GClassifyBarbie r)
  GClassifyBarbie (l :+: r) = MergeBarbieType (GClassifyBarbie l) (GClassifyBarbie r)
  GClassifyBarbie (K1 R (NonRec (Target (W F) a))) = 'WearBarbie
  GClassifyBarbie (K1 R (NonRec (Target F a))) = 'NonWearBarbie
  GClassifyBarbie (K1 R (NonRec (b (Target F)))) = GClassifyBarbie (Rep (b (Target F)))
  GClassifyBarbie (K1 R (RecUsage (b (Target F)))) = 'NoBarbie -- break recursion
  GClassifyBarbie (K1 _i _c) = 'NoBarbie

type ClassifyBarbie b = GClassifyBarbie (RecRep (b (Target F)))
