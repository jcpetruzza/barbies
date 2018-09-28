# Changelog for barbies

## 0.2.0.0
  - Replace `ConstraintsOf` in `ConstraintsB` by `AllB`, which allows
    constraints to be given on `a` instead of on `f a`. The `ClassF`
    class lets us specify constraints on `f a` by doing `AllB (ClassF c f) b`.
    `ConstraintsOf` becomes then a type alias. Credit goes to Csongor Kiss.
  - `ConstraintsOf` is ultimately deprecated in favour of `AllBF`, which
    is shorter and more consistent with `AllB`.
  - Changed the way `Wear` works: now wear-types need to have an extra
    type parameter that controls whether they are `Bare` or `Covered`. This
    let us remove all the "magic" that was involved, in the sense that
    one couldn't have instances of `FunctorB`, etc, for wear-types wihtout
    using `unsafeCoerce` (this was true also for handwritten instances).
  - Add `bsequence'`, a frequent specialisation of `bsequence`.
  - Add `bfoldMap`.
  - Add `buniqC` and `bmempty`.
  - Improve the internal instance derivation mechanism. We no longer
    need `unsafeCoerce` and the code should be in general indistinguishible
    from hand-written instances (not currently verified).
  - Fix support for barbie-types that have additional type parameters (#5).

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
