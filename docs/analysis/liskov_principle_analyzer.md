# Odin: Detection Of Liskov Substitution Principle Violation

This is the 5th defect type supported by Odin. The analysis relies on derivation of logical properties functionality and data structures utilized in the 3rd defect type (unjustified assumption).

## Defect description
The definition of the Liskov substitution principle is as follows:
> The principle defines that objects of a superclass shall be replaceable with objects of its subclasses without breaking the application.

We focus on one specific constraint that Liskov substitution principle enforces:

> A subtype is not substitutable for its super type if it strengthens its operations' preconditions, or weakens its operations' post conditions.

## Examples
```
[] > base
  [self x] > f
    seq > @
      assert (x.less 9)
      x.add 1
[] > derived
  base > @
  [self x] > f
    seq > @
      assert (x.greater 9)
      x.sub 1
```

Here the redefinition of method `f` in the `derived` object changes the original input domain of argument `x` from (-inf, 9) to (9, inf). This is a strengthening of the preconditions on method `f`, which is a violation of the Liskov substitution principle.

```
[] > base
  [self x] > f
    seq > @
      assert (x.less 9)
      x.add 1
[] > derived
  base > @
  [self x] > f
    seq > @
      assert (x.less 20)
      x.sub 1
```
In this example the Liskov substitution principle is not violated, since method `f` in the `derived` object expands the input domain of the function.

## Brief Algorithm description
1. All child objects are collected.
2. The parents of found children are resolved.
3. Methods that are overridden in children or are affected by changes in child objects are identified.
4. Logical properties of the method and its every version in parents are derived.
5. The properties of the method in child are compared to every other version pair-wise via implication.
6. If the defect is detected - a similar message is produced:
> Method `a` of object `child` violates the Liskov substitution principle as compared to version in parent object `grandparent`