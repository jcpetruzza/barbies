# barbies [![Build Status](https://travis-ci.org/jcpetruzza/barbies.svg?branch=master)](https://travis-ci.org/jcpetruzza/barbies)

Types that are parametric on unary type-constructors that control
their shapes are like Barbies that can wear different clothes
to become a different doll. This is a common Haskell-idiom. E.g.,

```haskell

data Person f
  = Person
      { name :: f String
      , age  :: f Int
      }

b1 :: Person Last       -- Barbie with a monoid structure
b2 :: Person (Const a)  -- container Barbie
b3 :: Person Identity   -- Barbie's new clothes

```

This package provides basic classes and abstractions to work with these types and easily transform them.
See the [docs](https://hackage.haskell.org/package/barbies/docs/Barbies.html) to learn more.

## Related packages

  - [barbies-th](https://hackage.haskell.org/package/barbies-th): Use Template Haskell to
    derive barbie-types from declarations that look like normal types.
  - [higgledy](https://hackage.haskell.org/package/higgledy): Use Generics to give a barbie-type interface
    to a normal type.
  - [harg](https://hackage.haskell.org/package/harg): Program-configuration (from command-line arguments,
     environment variables, configuration files, etc) via barbie-types
