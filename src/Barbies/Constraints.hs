-----------------------------------------------------------------------------
-- |
-- Module:  Barbies.Constraints
--
-- Support for operating on Barbie-types with constrained functions.
----------------------------------------------------------------------------
module Barbies.Constraints
  ( -- * Instance dictionaries
    Dict(..)
  , requiringDict

    -- * Getting constraints
  , AllBF
  , ClassF
  , ClassFG

    -- * Helpers
  , type (&)
  )

where

import Barbies.Internal.ConstraintsB
import Barbies.Internal.Dicts
