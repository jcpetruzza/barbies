{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.Barbie.Internal.Tag
  ( Tag(..)
  , CoercibleTag(..)
  ) where

import Data.Barbie.Internal.Classification

import Data.Coerce
import Unsafe.Coerce(unsafeCoerce)

newtype Tag (t:: k) f a = Tag {unTag :: f a}

class GCoercibleTag bt b tag f where
  gcoerceTag   :: b f -> b (tag f)
  gcoerceUntag :: b (tag f) -> b f

instance GCoercibleTag 'WearBarbie b (Tag t) f where
  gcoerceTag   = unsafeCoerce
  gcoerceUntag = unsafeCoerce

instance GCoercibleTag 'MixedBarbie b (Tag t) f where
  gcoerceTag   = unsafeCoerce
  gcoerceUntag = unsafeCoerce

instance (Coercible (b (Tag t f)) (b f), Coercible (b f) (b (Tag t f)))
  => GCoercibleTag 'NonWearBarbie b (Tag t) f where
  gcoerceTag   = coerce
  gcoerceUntag = coerce

instance (Coercible (b (Tag t f)) (b f), Coercible (b f) (b (Tag t f)))
  => GCoercibleTag 'NoBarbie b (Tag t) f where
  gcoerceTag   = coerce
  gcoerceUntag = coerce


class CoercibleTag tag b f where
  coerceTag   :: b f -> b (tag f)
  coerceUntag :: b (tag f) -> b f

instance (GCoercibleTag (ClassifyBarbie b) b (Tag t) f) => CoercibleTag (Tag t) b f where
  coerceTag   = gcoerceTag   @(ClassifyBarbie b)
  coerceUntag = gcoerceUntag @(ClassifyBarbie b)
