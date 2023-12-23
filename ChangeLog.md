# Changelog for barbies

## 2.1.0.0
  - Remove the deprecated interface from 1.x version
  - Add flipped-argument versions of `traverse` functions (Jack Kelly).
    By analogy to `Data.Traversable.for` in `base` these are similarly-named:
    - `Data.Barbie.bfor`
    - `Data.Barbie.bfor_`
    - `Data.Barbie.bforC`
    - `Data.Functor.Transformer.tfor`
    - `Data.Functor.Transformer.tforC`
    - `Data.Functor.Transformer.tfor_`
    - `Barbies.Bi.btfor1`

## 2.0.5.0
  - Add helper class Barbies.Constraints.(&) (#46)

## 2.0.4.0
  - Add FunctorT and DistributiveT instances for AccumT

## 2.0.3.1
  - Compatibility changes for GHC 9.2 (Dan Dart)

## 2.0.3.0
  - Add Barbies.Bi.bttraverse_
  - Add Barbies.Bi.btfoldMap
  - Fix failure to derive Constraints{B,T} for proper
    bi-barbies.
  - Builds with ghc 9 (Fumiaki Kinoshita)

## 2.0.2.0
  - Add `Barbies.Bare.WearTwo` type family to support having _field-specific_
    newtype wrappers that get applied only to the covered barbie (Lennart
    Spitzner).

## 2.0.1.0
  - Add the `DistributiveB` class (Gergő Érdi).

## 2.0.0.0
  - Builds with ghc 8.8, but drops support for ghc 8.0 and 8.2
  - Fix failure to derive `TraversableB` and `ConstraintsB` when using a type
    parameter not under the functor argument.
  - Fix failure to derive instances for types with arguments of kind `k -> Type`.
  - Fix failure to derive instances where functor arg is applied under a functor.
  - Derive instances for nested barbies occurring under two functors (Matthew Peddie).
  - Add `foldMapC` and `bzipWithxC` (Matthew Peddie).
  - Create a `Barbies` module, to contain wrappers, basic docs, etc.
    `Data.Functor.Barbie` contains only functor-related stuff.
  - Replace `ProductB` by `ApplicativeB`, with more lax laws. Now we can derive
    more instances than before, since arbitrary monoids are allowed as fields
    of the record.
  - Add `Data.Functor.Transformer`, operations for bi-barbies, including support for nesting.
  - Add a `ErrorContainer` wrapper, similar to `Container` but for `Either e`.
  - Remove `ProductBC`, since `bdicts` can now be defined in terms of `ApplicativeB`
    and `ConstraintsB`.
  - Remove functions deprecated on release 1.0
  - Deprecate `Data.Functor.Prod`, `(/*)` and `(/*/)`.
  - Deprecate `Data.Barbie`, in favor of `Data.Functor.Barbie`.
  - Deprecate `Data.Barbie.Bare`, in favor of `Barbies.Bare`.
  - Deprecate `Data.Barbie.Constraints`, in favor of `Barbies.Constraints`.

## 1.1.3.0
  - `Wear` will raise a `TypeError` instead of getting
    stuck (Alex Peitsinis).

## 1.1.2.1
  - Uploaded 1.1.2.0 was broken (missing `btraverseC`)

## 1.1.2.0
  - Add `traverseC` (Ole Krüger).
  - Fix typo in ProductB laws (thanks to Ben Radford).

## 1.1.1.0
  - Add `bmapC` (Chris Penner).

## 1.1.0.0
  - Make all classes poly-kinded (#7): a barbie can now be any type
    parameterised by a type `(k -> Type)`. In particular, a (higher-kinded)
    barbie is a type parameterised by a barbie. Thanks to Ole Krüger.

  - Add instances for functor transformers: `Proxy`, `Const`, `Product`, `Sum`
    and `Compose` (Ole Krüger).

## 1.0.0.0
  - Replaced `ConstraintsOf` in `ConstraintsB` by `AllB`, which allows
    constraints to be given on `a` instead of on `f a`. The `ClassF`
    class lets us specify constraints on `f a` by doing `AllB (ClassF c f) b`.
    `ConstraintsOf` becomes then a type alias. Credit goes to Csongor Kiss.

  - `ConstraintsOf` was ultimately deprecated in favour of `AllBF`, which
    is shorter and more consistent with `AllB`.

  - Renamed `ConstraintsB(adjProof)` to `ConstraintsB(baddDicts)`.

  - Renamed `ProofB(bproof)` to `ProductBC(bdicts)`.

  - Changed the way `Wear` works: now wear-types need to have an extra
    type parameter that controls whether they are `Bare` or `Covered`. This
    let us remove all the "magic" that was involved, in the sense that
    one couldn't have instances of `FunctorB`, etc, for wear-types wihtout
    using `unsafeCoerce` (this was true also for handwritten instances).

  - Added `bsequence'`, a frequent specialisation of `bsequence`.

  - Added `bfoldMap`.

  - Added `buniqC` and `bmempty`.

  - Improved the internal instance derivation mechanism. We no longer
    need `unsafeCoerce` and the code should be in general indistinguishible
    from hand-written instances (not currently verified).

  - Fixed support for barbie-types that have additional type parameters (#5).

## 0.1.4.0
  - Added `btraverse_`

  - Added the trivial `Void` and `Unit` barbies

## 0.1.3.1
  - Fixed issue on Barbie-types with strictness annotations.

## 0.1.3.0
  - Use both `Monoid` and `Semigroup` as constraints for the `Monoid` instance,
    so that this works with ghc 8.0 to 8.4 (Fraser Murray)

## 0.1.2.0
  - Use `Monoid` and not `Semigroup` as constraints for the `Monoid` instance

## 0.1.1.0
  - Added `instance Semigroup (Barbie b)` to go along the `Monoid` instance

## 0.1.0.1
  - Works under GHC 8.0.2, but notice one needs to use empty instance
    declarations, because ghc chokes on `deriving` clauses.

## 0.1.0.0 - Initial release
