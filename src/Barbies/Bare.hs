-----------------------------------------------------------------------------
-- |
-- Module      :  Barbies.Bare
--
-- Sometimes one needs a type like
--  @Barbie 'Data.Functor.Identity.Identity'@ and it may feel like
-- a second-class record type, where one needs to
-- unpack values in each field. For those cases, we can leverage on
-- closed type-families:
--
-- @
-- data 'Bare'
-- data 'Covered'
--
-- type family 'Wear' t f a where
--   'Wear' 'Bare'    f a = a
--   'Wear' 'Covered' f a = f a
--
-- data SignUpForm t f
--   = SignUpForm'
--       { username  :: 'Wear' t f 'String',
--       , password  :: 'Wear' t f 'String'
--       , mailingOk :: 'Wear' t f 'Bool'
--       }
--  instance 'FunctorB' (SignUpForm 'Covered')
--  instance 'TraversableB' (SignUpForm 'Covered')
--  ...,
--  instance 'BareB' SignUpForm
--
-- type SignUpRaw  = SignUpForm 'Maybe'
-- type SignUpData = SignUpForm 'Bare'
--
-- formData = SignUpForm "jbond" "shaken007" False :: SignUpData
-- @
----------------------------------------------------------------------------
module Barbies.Bare
  ( -- * Bare values
    Wear
  , Bare
  , Covered

    -- * Covering and stripping
  , BareB(bstrip, bcover)
  , bstripFrom
  , bcoverWith

  ) where

import Barbies.Internal.Bare
  ( Wear, Bare, Covered
  , BareB(..)
  , bstripFrom, bcoverWith
  )
