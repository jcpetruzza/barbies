# Changelog for barbies

## 0.2.0.0
  - Replace `ConstraintsOf` in `ConstraintsB` by `AllB`, which allows
    constraints to be given on `a` instead of on `f a`. The `ClassF`
    class lets us specify constraints on `f a` by doing `AllB (ClassF c f) b`.
    `ConstraintsOf` becomes a type alias. Credit goes to Csongor Kiss.
  - Add `bsequence'`, a frequent specialisation of `bsequence`.
  - Add `bfoldMap`.
  - Add `buniqC` and `bmempty`.
  - Improve the instance derivation mechanism. For barbie-types without `Wear`,
    the code should be indistinguishible from hand-written instances
    (not currently verified).

## 0.1.4.0
  - Add `btraverse_`
  - Add the trivial `Void` and `Unit` barbies

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
