module Data.Barbie.Bare
  {-# DEPRECATED "Use Barbies.Bare" #-}
  ( -- * Bare values
    Barbies.Bare.Wear
  , Barbies.Bare.Bare
  , Barbies.Bare.Covered

    -- * Covering and stripping
  , Barbies.Bare.BareB(bstrip, bcover)
  , Barbies.Bare.bstripFrom
  , Barbies.Bare.bcoverWith
  ) where

import qualified Barbies.Bare
