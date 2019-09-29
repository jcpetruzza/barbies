module Data.Barbie.Constraints
  {-# DEPRECATED "Use Data.Functor.Barbie or Barbie.Constraints" #-}
  ( -- * Instance dictionaries
    Dict(..)
  , requiringDict

    -- * Retrieving dictionaries
  , ConstraintsB(..)
  , ProductBC(..)
  , bmapC
  , btraverseC

  , AllBF
  , ClassF
  , ClassFG
  )

where

import Barbies.Internal.Constraints
import Barbies.Internal.Dicts
import Data.Barbie.Internal.ProductC
