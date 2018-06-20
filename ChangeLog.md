# Changelog for barbies

## 0.1.2.0
  - Use 'Monoid' and not 'Semigroup' as constraints for the 'Monoid' instance

## 0.1.1.0
  - Add `instance Semigroup (Barbie b)` to go along the `Monoid` instance

## 0.1.0.1
  - Works under GHC 8.0.2, but notice one needs to use empty instance
    declarations, because ghc chokes on `deriving` clauses.


## 0.1.0.0 - Initial release
