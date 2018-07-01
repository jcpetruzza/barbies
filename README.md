# barbies [![Build Status](https://travis-ci.org/jcpetruzza/barbies.svg?branch=master)](https://travis-ci.org/jcpetruzza/barbies)

Types that are parametric on unary type-constructors that control
their shapes are like Barbies that can wear different clothes
to become a different doll. This is a common Haskell-idiom. E.g.,

```haskell

data Barbie f
  = Barbie
      { name :: f String
      , age  :: f Int
      }

b1 :: Barbie Last       -- Barbie with a monoid structure
b2 :: Barbie (Const a)  -- container Barbie
b3 :: Barbie Identity   -- Barbie's new clothes

```

This package provides basic classes and abstractions to work with these types and easily transform them.
