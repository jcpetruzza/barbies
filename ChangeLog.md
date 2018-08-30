# Changelog for barbies

## 0.1.4.0
  - Add btraverse_
  - Add the trivial Void and Unit barbies

## 0.1.3.1
  - Fix issue on Barbie-types with strictness annotations.

## 0.1.3.0
  - Use both `Monoid` and `Semigroup` as constraints for the `Monoid` instance,
    so that this works with ghc 8.0 to 8.4 (Fraser Murray)

## 0.1.2.0
  - Use `Monoid` and not `Semigroup` as constraints for the `Monoid` instance

## 0.1.1.0
  - Add `instance Semigroup (Barbie b)` to go along the `Monoid` instance

## 0.1.0.1
  - Works under GHC 8.0.2, but notice one needs to use empty instance
    declarations, because ghc chokes on `deriving` clauses.


## 0.1.0.0 - Initial release
