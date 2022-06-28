[![Maven Release](https://badgen.net/maven/v/metadata-url/https/repo1.maven.org/maven2/org/polystat/odin/odin_2.13/maven-metadata.xml)](https://oss.sonatype.org/content/repositories/releases/org/polystat/odin/odin_2.13/ )
[![CI](https://github.com/polystat/odin/actions/workflows/ci.yml/badge.svg)](https://github.com/polystat/odin/actions/workflows/ci.yml)

[![Hits-of-Code](https://hitsofcode.com/github/polystat/odin)](https://hitsofcode.com/view/github/polystat/odin)
![Lines of code](https://img.shields.io/tokei/lines/github/polystat/odin)

Odin (object dependency inspector) — a static analyzer for [EO programming language](https://github.com/cqfn/eo).

Odin is used in [polystat](https://github.com/polystat/polystat)
and [polystat-cli](https://github.com/nikololiahim/polystat-cli).

# Table of contents

- [Analysis](#analysis)
    - [Defect Descriptions](#defects)
        - [Mutual Recursion](#mutual-recursion-analyzer)
        - [Unjustified Assumptions](#unjustified-assumptions-in-subclasses-analyzer)
        - [State Access](#direct-access-to-the-base-class-state-analyzer)
        - [Liskov Principle](#liskov-substitution-principle-violation-analyzer)
    - [Limitations](#limitations)
- [Usage](#usage)
    - [Running](#sandbox)
    - [Testing](#tests)
- [Project structure](#project-structure)
- [Development](#development)

# Analysis

## Defects

(in-depth documentation for each defect can be found in the subsequent sections)

As of now, ODIN supports the following defect types:

#### 1. Unanticipated mutual recursion

> Mutual recursion caused by method redefinition during inheritance.

Sample input:

```
[] > a 
  b > @ 
  [self] > f 
    self > @ 
[] > b 
  [] > d 
    a > @
  [self] > g 
    self > @ 
[] > c 
  [] > e 
    a > @ 
    [self] > h 
      self.f self > @ 
[] > t 
  c.e > @ 
  [self] > f 
    self.h self > @ 
```

Sample output:

```
t: 
  t.h (was last redefined in "c.e") ->
  t.f ->
  t.h (was last redefined in "c.e")
```

#### 2. Unjustified assumptions about methods of superclasses

> Assumptions made in subclasses regarding method dependencies in superclasses.

Sample input:

```
[] > base
  [self x] > f
    seq > @
      assert (x.less 9)
      x.add 1
  [self x] > g
    seq > @
      assert ((self.f self (x.add 1)).less 10)
      22
[] > derived
  base > @
  [self x] > f
    seq > @
      assert (5.less x)
      x.sub 1
```

Sample output:

```
Method g is not referentially transparent
```

#### 3. Direct Access to the Base Class State

> Altering the state stored in the base class in undesirable ways.

Sample input:

```
[] > a
  memory > state
  [self new_state] > update_state
    self.state.write new_state > @
[] > b
  a > @
  [self new_state] > change_state_plus_two
    self.state.write (new_state.add 2) > @
```

Sample output:

```
Method 'change_state_plus_two' of object 'b' directly accesses state 'state' of base class 'a'
```

#### 4. Liskov Substitution Principle Violation

> Ability to use subclasses in place of superclasses.

Sample input:

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

Sample output:

```
Method f of object derived violates the Liskov substitution principle as compared to version in parent object base
```

### Mutual Recursion Analyzer

[(Back to TOC)](#table-of-contents)

This analyzer is capable of detecting mutual recursion caused by method redefinition during inheritance.

#### Identifiable patterns

Only the following patterns are recognized by the algorithm:

1. Object declarations:

```
[] > name
  {0 or 1 decoratee}
  {>= 0 method declarations}
  {>= 0 object declarations}
```

2. Decoratee

```
simple_name > @
or
an.attribute.of.some.object > @
```

3. Method declarations:

```
[self {>= 0 params}] > methodName
{>= 0 method calls} > @
```

4. Method calls:

```
self.methodName self {>= 0 params}
```

#### Known Shortcomings

##### 1. Some decorations are ignored.

The algorithm supports only two kinds of decorations: simple applications, like `fruit > @`, and attribute decorations,
like `fruits.citrus.orange > @`. **All the other patterns are ignored, thus an object containing such pattern is
considered to not have a `@` attribute at all!**

For example, in this program:

```
[] > baobab
   1.add 2 > @
   [self] > stall
```

the statement `1.add 2 > @` is ignored so, the algorithm 'sees' this program like so:

```
[] > baobab
  [self] > stall
```

##### 2. Unreachable code

The algorithm takes into consideration every possible method call that is present in the body, whether it is actually
called does not matter. For example, in method `zen`:

```
[self] > zen
  if. > @
    false
    self.no self
    self.yes self
```

The algorithm will consider both `no` and `yes` method calls, even though `no` is never called.

The same goes for unused local definitions. In EO, "method" is an object containing a special `@` (phi) attribute. When
the method is called:

```
self.method self
```

it is the object labelled by `@` that gets executed. Any other local bindings that are not used in the binding
associated with `@` are ignored. For example:

```
[self] > m
  fib 100000 > huge_computation!
  1.add 1 > @
```

Only `1.add 1` will be computed when method `m` is called. The `huge_computatuion` will never be performed (since it is
never called in the declaration of `@`).

That being said, if `huge_computation` is a method call, say:

```
[self] > m

  # a valid method call now!
  self.fib self 100000 > huge_computation!
  1.add 1 > @
```

The algorithm would still consider it as "called". In other words, it will be a part of the analyzer output if it is a
part of some mutual recursion chain.

Furthermore, the algorithm does not run any checks for the presence of `@` attribute. As a result, a method that would
produce a runtime error when called, like:

```
[self] > m
  self.fib self 100000 > huge_computation!
  
  # this is no longer `@`
  1.add 1 > two 
```

is still considered by the algorithm.

##### 3. Inheritance chains with cycles

Even though the following code is perfectly valid in EO:

```
[] > a
  b > @

[] > b
  a > @
```

This code (and the similar variations) will cause the analyzer to fail with `StackOverflowError`.

#### Algorithm description

1. Parse the source code to build its AST.

2. Create a tree of partial objects that preserves the object nestedness. Basically, collect all the information that
   can be collect within a single pass.

3. This tree is then refined to create a complete representation of the program, with method redefinition.

4. Traverse the callgraphs finding cycles that involve methods coming from multiple objects. For example, the
   callchain ***a.f -> a.g -> a.f*** will not be in the output, meanwhile the callchain  ***a.f -> b.g -> a.f*** will be
   a part of it.

#### Possible errors

The analyzer can fail with the following errors:

1. The analyzer encounters decoratee objects that are not defined.

```
[] > a
  b > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Parent (or decoratee) object "b" of object "a" is specified, but not defined in the program!` </span>.

2. The analyzer encounters calls of non-existent methods.

```
[] > a
  [self] > f
    self.non_existent self > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Method "non_existent" was called from the object "a", although it is not defined there!` </span>.

### Unjustified Assumptions In Subclasses Analyzer

[(Back to TOC)](#table-of-contents)

For this type of defect we detect whether the refactoring of superclasses by inlining can affect the functionality of
subclasses in an undesired way. The notion of unjustified assumption in this context refers to assumptions made in
subclasses regarding method dependencies in superclasses.

#### Brief algorithm description

The following stages and steps take place during program analysis:

##### Setting locators

0. The source code of the program is parsed to build its corresponding AST.
1. Identifiers without explicitly set locators get locators derived and set.

##### Inlining

2. Object hierarchy is analyzed to set apart methods that comply with specific criteria (can be found below) and are not
   redefined.
3. Every method in the resulting set is paired with its version in which all valid calls (criteria can be found below)
   to other methods of the containing object are inlined non-recursively.

##### Logic extraction

4. Logical properties (constraints on variables in the form of a logical expression) are extracted for both versions of
   the method in the pair.
5. The derived expression for the version before inlining is connected with the expression after inlining by the means
   of implication. Doing so allows us to discover whether the constraints on the variables have become weaker or
   stronger.
6. We deduce that the unjustified assumption takes place if the resulting logical expression is false, meaning that
   there exist some inputs such that they worked before the inlining, and stopped working after.

#### In-depth description of algorithms

##### Requirements for the methods to be chosen for analysis:

1. Its value is an object term with void self and attached @ attributes
2. There are no references to its @ attribute in any of its attached attributes.

##### Requirements for the call to be considered valid during inlining:

1. The called method belongs to the same object as the method containing the call.
2. The call the general form `self.method self ...`. Meaning that `self` is used as both the source for the method and
   is passed as the first argument.

##### Algorithm for setting locators:

0. Explicit locator chains of from (^.^.value) are resolved at parse-time and are converted to corresponding AST-nodes.
1. All predefined EO-keywords are stored in a special structure called 'context' and have locators point to the root of
   the program.
2. The first step of actual processing extends the existing context by adding all top-level objects.
3. Each top-level object is recursively explored while keeping track of encountered objects and their depth in the
   context.
4. The terms that refer to the definitions in the current scope are given 0-locators (\$), while terms contained in the
   context are given locators by subtracting their depth from current depth.

Before:

```
[] > obj
  [self] > method
    self > @
  [method] > shadowedMethod
    method > @
  [] > notShadowedMethod
    method > @

[] > notShadowedObj
  obj > @
  
[] > shadowedObj
  [obj] > method
    [] > innerMethod
      obj > @
    obj > @

[] > outer
  [] > self
    256 > magic
    [] > dummy
      [outer] > dummyMethod
        outer > @
      outer.self > @
    self "yahoo" > @
  [self] > method
    self.magic > @
```

After:

```
[] > obj
  [self] > method
    $.self > @
  [method] > shadowedMethod
    $.method > @
  [] > notShadowedMethod
    ^.method > @
[] > notShadowedObj
  ^.obj > @
[] > shadowedObj
  [obj] > method
    [] > innerMethod
      ^.obj > @
    $.obj > @
[] > outer
  [] > self
    256 > magic
    [] > dummy
      [outer] > dummyMethod
        $.outer > @
      ^.^.^.outer.self > @
    ^.self > @
      "yahoo"
  [self] > method
    $.self.magic > @
```

#### Inlining algorithm:

0. All objects are extracted from the AST
1. Every AST-object is converted into the 'Object' data structure
2. Each created 'Object' is processed as follows:
   2.1 All calls in the object are processed, while non-call binds are returned as is. 2.1.1 Replace occurrences of
   formal parameters in the method body with the argument values. 2.1.2 If the method being inlined contains local
   bindings other than `@` attribute, extract them into the `local-attrs` object and put this object adjacent to the
   call-site. 2.1.3. References to local attributes are replaced with their counterparts from the `local-attrs` object.
   2.1.4. The call is replaced with the value of the called method's `@` attribute.
3. If the conversion was successful, the modified Object is returned. Otherwise, the list of errors is returned.

Before:

```
[] > obj
    [self arg1 arg2 arg3] > average3
       arg1.add (arg2.add arg3) > sum
       3 > count
      sum.div count > average
       [] > @
         ^.sum > sum
         ^.count > count
         ^.average > average
     [self] > call-site
       self.average3 self 1 2 3 > @
```

After:

```
[] > obj
 [self arg1 arg2 arg3] > average3
      $.arg1.add > sum
         $.arg2.add
          $.arg3
       3 > count
      $.sum.div > average
         $.count
       [] > @
         ^.sum > sum
         ^.count > count
        ^.average > average
  [self] > call-site
      [] > local_average3
        1.add > sum
          2.add
            3
         3 > count
         $.sum.div > average
           $.count
       [] > @
         ^.local_average3.sum > sum
         ^.local_average3.count > count
         ^.local_average3.average > average
```

#### Algorithm for logical expression extraction:

0. The list of recognized terms and their corresponding expressions/properties:
   ![](https://i.imgur.com/PBFB7ac.png)
1. All methods that can be called from the target method are collected and stored.
2. Properties and values of each function in the set are derived according to the above rules. This is done to account
   for their potential calls. The resulting SMT-code has all properties and returned expressions of every functions
   listed at the beginning of top of the file as function definitions.
3. The target method and its version with inlined calls are recursively processed on field-by-field basis.
4. The acquired logical expressions are connected using the implication operation and assertion in the following
   manner: `assert(logic-before-inlining => logic-after-inlining)`.
5. The resulting formula is then complemented by ``(check-sat)`` and ``(get-model)`` commands and sent the SMT-solver
   for further processing.

Program:

```
[] > test
  [] > parent
    [self x] > f
      x.sub 5 > y1
      seq > @
        assert (0.less y1)
        x
    [self y] > g
      self.f self y > @
    [self z] > h
      z > @
  [] > child
    parent > @
    [self y] > f
      y > @
    [self z] > h
      self.g self z > @
```

Resulting SMT-code:

```clojure=
(define-fun value-of-before-f ((var-before-y Int)) Int var-before-y)
(define-fun properties-of-before-f ((var-before-y Int)) Bool (and true true))
(define-fun value-of-before-g ((var-before-y Int)) Int (value-of-before-f var-before-y))
(define-fun properties-of-before-g ((var-before-y Int)) Bool (and (properties-of-before-f var-before-y) true))
(define-fun value-of-before-h ((var-before-z Int)) Int (value-of-before-g var-before-z))
(define-fun properties-of-before-h ((var-before-z Int)) Bool (and (properties-of-before-g var-before-z) true))
(define-fun value-of-after-f ((var-after-y Int)) Int var-after-y)
(define-fun properties-of-after-f ((var-after-y Int)) Bool (and true true))
(define-fun value-of-after-g ((var-after-y Int)) Int var-after-y)
(define-fun properties-of-after-g ((var-after-y Int)) Bool (exists ((var-after-local_f Int)(var-after-local_f-y1 Int)) (and (and (and (and true true) (< 0 var-after-local_f-y1)) true) (and (and (and true true) (= var-after-local_f-y1 (- var-after-y 5))) true))))
(define-fun value-of-after-h ((var-after-z Int)) Int (value-of-after-f var-after-z))
(define-fun properties-of-after-h ((var-after-z Int)) Bool (and (properties-of-after-f var-after-z) true))
(assert (forall ((var-before-y Int)) (exists ((var-after-y Int)) (and (and true (= var-before-y var-after-y)) (=> (and (properties-of-before-f var-before-y) true) (exists ((var-after-local_f Int)(var-after-local_f-y1 Int)) (and (and (and (and true true) (< 0 var-after-local_f-y1)) true) (and (and (and true true) (= var-after-local_f-y1 (- var-after-y 5))) true))))))))

(check-sat)
(get-model)
```

##### Current Limitations:

1. Method pairs for inspection are formed by considering only direct descendants. For example, in case of an inheritance
   chain A -> B -> C only combinations (A,B) and (B,C) are examined. It would be ideal to examine all possible
   combinations. So, in the given example an additional pair (A,C) is to be examined.
2. Presence of methods with mutual or regular recursion causes the recursive calls to be interpreted only partially.
3. Current implementation makes it impossible to store boolean values in object fields. Only integer values are
   supported as of now.
4. No checks are performed on the contents of asserts in EO. As such, it is possible to assert an integer value, which
   would cause unanticipated behavior. For example, `assert (3.add 5)`.
5. Imported functions are not supported by the analyzer.
6. Methods without arguments sometimes cause the analyzer to crash.

#### Possible Errors

The error messages that the analyser can fail with at each stage are listed here.

#### Setting locators

##### 1. The analyzer encounters a reference to a non-existent object.

```
[] > a
  [self] > method
    b > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Could not set locator for non-existent object with name b` 
</span>

#### Inlining

##### 1. Decoratee is not a valid object.

```
1 < non-existent
[] > a
  non-existent > @
```

> <span style="color:red"> `java.lang.Exception: There is no such parent with name "^.non-existent"`
</span>

##### 2. Locators point too broadly.

```
[] > b
[] > a
  ^.^.b > @
```

> <span style="color:red"> `java.lang.Exception: Locator overshoot at ^.^.b `

</span>

##### 3. Attempt to call a method that does not exist.

```
[] > a
  [self] > f
    self.non-existent self > @
```

> <span style="color:red">`java.lang.Exception: Inliner: attempt to call non-existent method "non-existent"`
</span>

##### 4. Attempt to call a method with the wrong number of arguments.

```
[] > a
  [self a b] > f
    a.add b > @
  [self] > g
    self.f self 1 > @
  [self] > h
    self.f self 1 2 3 > @
```

> <span style="color:red"> `java.lang.Exception: 
Wrong number of arguments given for method f. Wrong number of arguments given for method f.
`

</span>

#### Logic extraction

##### 1. Objects with void attributes as local definitions of methods are not supported.

```
[self x] > f
  22 > local-def
  [void] > local-obj
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: object with void attributes are not supported yet!` </span>.

##### 2. Attempt to access attributes of applications.

```
  [] > parent
    [self x] > f
      (self.g 10).@ > res
      10 > @
    [self y] > g
      y.add 1 > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Cannot analyze dot of app: (self.g 10).@` </span>.

##### 3. Using a primitive that is not yet supported.

```
[self] > method
  some-unsupported-primitive 10 > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Unsupported 1-ary primitive some-unsupported-primitive` </span>.

```
[self x] > method
  x.some-unsupported-primitive 10 > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Unsupported 1-ary primitive .some-unsupported-primitive` </span>.

##### 4. Call to a method with no arguments.

```
  [] > parent
    [self x] > f
      (self.g 10).@ > res
      10 > @
    [self y] > g
      y.add 1 > @
```

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Methods with no arguments are not supported` </span>.

##### 5. Some SMT-related issue.

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: SMT solver failed with error: ??? </span>.`

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: SMT failed to parse the generated program with error: ???` </span>.

### Direct Access to the Base Class State Analyzer

[(Back to TOC)](#table-of-contents)

The fourth defect type suported by odin.

#### Original Problem Statement

```
An extension class should not access the state of its base 
class directly, but only through calling base class methods.
```

Given the class `C` with methods `m` and `n`:

```
C = class 
  x : int := 0; y : int := 0
  m => y := y + 1; x := y
  n => y := y + 2; x := y 
end 
```

And a potential modification `M` that affects method `n`:

```
M = modifier
  n => x:= x + 2
end
```

We will have the following: the modification `M`, if applied to class `C` might cause unnecessary confusion. The initial
implemntation maintains an implicit invariant `x=y` by using `x := y` at the last action of every method.

However, the redefinition of `n` in `M` causes the invariant to be broken in case of a calling the modified version
of `n` after `m`. This will cause `y` to be equal to 1, and `x` to be equal to 3.

Conversely, the sequence of calls `n;m` will cause a different kind of confusion. By looking at `C`, one could assume
that the method calls will make `x` equal to 3, whereas, in fact, `x` will be assigned only 1.

Thus, the best way to avoid such confusion is by only allowing changes to the variables defined in the base class to be
made via the corresponding methods of the base class.

#### EO Equivalnet of the Statement

In EO, base class state can be modelled with the use of
`memory` functionality for variables and `cage` functionality for objects.

So, having object `a` with state `state`:

```
[] > a
  memory > state
  [self new_state] > update_state
    seq > @
      self.state.write new_state
      self.state
```

The proper way to change the state in the subclass `b` would be:

```
[] > b
  a > @
  [self new_state] > change_state_plus_two
    new_state.add 2 > tmp
    seq > @
      self.update_state self tmp
      self.state
```

An **im**proper way to achieve the same functionality in subclass `bad`:

```
[] > bad
  a > @
  [self new_state] > change_state_plus_two
    seq > @
      self.state.write (new_state.add 2) 
      self.state
```

Access to state of any superclass is considered a bad practice. Examples can be found in functions `read_func`
and `bad_func`.

> NOTE: access to local state, such as `local_state` is not considered a defect.

```
[] > super_puper
  memory > omega_state

[] > super
  super_puper > @
  memory > super_state
  [self] > read_func
    self.omega_state.add 2 > @

[] > parent
  super > @
  memory > parent_state

[] > child
  parent > @
  memory > local_state
  [self] > bad_func
    seq > @
      self.omega_state.write 10
      self.super_state.write 10
      self.parent_state.write 10
      self.local_state.write 10
```

#### Brief description of the Devised algorithm

1. Build the `Tree` structure from the source code and resolve all parents.
2. Collect all existing subclasses.
3. Identify the state variables (ones that contain `memory` or `cage`) accessible by each target subclass.
4. If some method of the target subclass accesses a state variable present in its hierarchy - a message similar to the
   following is generated:
   `
   Method 'change_state_plus_two' of object 'b' directly accesses state 'state' of base class 'a'
   `

#### Implementation Highlights

1. Only variables that are accessed using the `self` object are considered. So, `self.state.write 10` is considered,
   while `state.write 10` is not considered, since it can potentially be a local variable.
2. The hierarchy where the defect takes place can be nested in objects.
3. Access to the state refers to actions that either read the state or call functions on it.

#### Possible limitations

1. Variables can be accessed without using `self`. The current implementation does not consider such cases.

### Liskov Substitution Principle Violation Analyzer

[(Back to TOC)](#table-of-contents)

The analysis relies on derivation of logical properties functionality and data structures utilized in the unjustified
assumption defect type.

#### Defect description

The definition of the Liskov substitution principle is as follows:
> The principle defines that objects of a superclass shall be replaceable with objects of its subclasses without breaking the application.

We focus on one specific constraint that Liskov substitution principle enforces:

> A subtype is not substitutable for its super type if it strengthens its operations' preconditions, or weakens its operations' post conditions.

#### Examples

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

Here the redefinition of method `f` in the `derived` object changes the original input domain of argument `x` from (
-inf, 9) to (9, inf). This is a strengthening of the preconditions on method `f`, which is a violation of the Liskov
substitution principle.

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

In this example the Liskov substitution principle is not violated, since method `f` in the `derived` object expands the
input domain of the function.

#### Brief Algorithm description

1. All child objects are collected.
2. The parents of found children are resolved.
3. Methods that are overridden in children or are affected by changes in child objects are identified.
4. Logical properties of the method and its every version in parents are derived.
5. The properties of the method in child are compared to every other version pair-wise via implication.
6. If the defect is detected - a similar message is produced:

> Method `a` of object `child` violates the Liskov substitution principle as compared to version in parent object `grandparent`

## Limitations

Some assumptions are made about EO programs and used EO constructs during analysis for all type of defects.
Additionally, some constructs and syntax are not supported intentionally.

### 1. No support for the following syntax:

- named arguments
```
distance.
  point
    5:x
    -3:y
  point:to
    13:x
    3.9:y
```
- multiline attribute access

```
dx.pow 2
.add
dy.pow 2
.sqrt > length
```
- regex, boolean and bytes data objects are not implemented
- identifiers cannot include arbitrary unicode
- atom objects (`[other] > add /int[?]`) are not implemented

### 2. Multi-file EO programs are only partially supported

While, the imports are recognised by the analyzer in the form of `+alias optional-alias imoprt` statements, the actual
code behind them is not used during analysis. More than that, the only instance when imports are considered is during
the setting-locators stage of the third defect analysis. Even then, the information is not used in any meaningful way.

# Usage

[(Back to TOC)](#table-of-contents)

This project is meant to be used as a module, and not a standalone application.

Nevertheless, it is still possible to test the available analyzers.

Running the project requires:

- [sbt](https://www.scala-sbt.org/1.x/docs/Setup.html)
- JDK 8+

## Provided API

The API exposed by ODIN can be found in the
[interop](interop) project.

## Sandbox

One can play around with the analyzer in the sandbox project by modifying the
[present](sandbox/src/main/scala/org/polystat/odin/sandbox/Sandbox.scala)
code snippets or adding new ones.

The sandbox can be run via:

```shell
sbt sandbox/run
```

File containing the `run` entrypoint can be found [here](sandbox/src/main/scala/org/polystat/odin/sandbox/Sandbox.scala)
.

## Tests

All existing tests can be run via:

```shell
sbt test 
```

## Scala REPL

The source code can be run in scala REPL via:

```shell
sbt console
```

# Project structure

[(Back to TOC)](#table-of-contents)

## `core` project

Contains the EO AST.

All other projects should depend on this one.

## `analysis` project

Contains the implementations of the analyzers odin can use.

## `backends` project

Contains backends for the static analyzer. Backend is something that produces representations the EO AST.

### `eolang` backend

Backend that can generate EO programs from an AST.

## `utils` project

Convenient tools that are used by other parts of the analyzer.

This project must not depend on any other project.

## `sandbox` project

Allows one to interactively run and manually test the analyzer. Facilitates the development and debug of the source code
and allows one to see intermediate results of the development.

Any other project must not depend on it.

For more details on the project organization see:

- [build.sbt](build.sbt) - main build configuration file
- [project/plugins.sbt](project/plugins.sbt) - lists sbt plugins
- [project/Dependendencies.scala](project/Dependendencies.scala) - lists dependencies and versions for the projects
- [project/Compiler.scala](project/Compiler.scala) - lists compiler flags and compiler plugins

## `parser` project

This submodule holds the source code for EO parser written in Scala using a library
called [`cats-parse`](https://github.com/typelevel/cats-parse). It should recognize any valid EO program and produce an
AST defined in the `core` module.

### Syntax Specification

When working on this parser, I tried to make it as close as possible
to [specifications from the paper (Section 2, Figure 1)](https://www.eolang.org/eolang-paper.pdf), although, due to
specifics of Tymur's AST and the `cats-parse` library, there were some diversions.

### Tests

Most EO programs that were used to test this parser (but not all) are available
in `parser/src/test/resources/eo_sources`. The tests themselves can be found in the form of ScalaTest unit tests
in `parser/src/test/scala/org/polystat/odin/parser/cats-parse`.

The parser has also been tested on the randomly-generated EO programs. The ability to randonly generate EO programs and
run the parser against them is provided by the [Scalacheck](https://github.com/typelevel/scalacheck) library.

### Motivation

The [existing EO parser implementation](https://github.com/cqfn/eo/tree/master/eo-parser) was not satisfactory for
several reasons:

- Its output is a bunch of XML files collectively known as XMIR. I have already
  had [some experience](https://github.com/polystat/eo2py/blob/main/eo2py-maven-plugin/src/main/resources/org.eolang.maven/pre/to-python.xsl)
  of working with this representation, and I'm not a fan of it.
- The existing parser is very restrictive when it comes to whitespace and comments. This implementation has lifted some
  of these constraints.

### Advantages:

- No need to know XML/XSLT to use it.
- Very easy to extend or modify.
- Directly usable in Scala and Java programs.
- Can produce arbitrary Java and Scala and classes.
- It has much better, highly customizable error-reporting abilities.
- Can potentially be extended to produce any kind of intermediate representation.

### Disadvantages

- The output of this parser can only be accessed from within Scala or Java programs. On the other hand, the AST can be
  serialized to produce very similar (if not the same) XML.
- Maintaining this parser requires knowing the specifics of Scala and `cats-parse`.

# Development

[(Back to TOC)](#table-of-contents)

`master` branch has the latest changes and must always be buildable. All the changes are to be done by creating a pull
request. Once the [CI](#ci) run has successfully finished and a reviewer has approved changes, the code can be manually
merged to the `master` branch.

## CI

When a pull request is sent to `master` or a branch with a pull request to `master` is pushed, the following checks will
run via GitHub Actions:

- Build — all projects in the repository are built to check that the code compiles
- Test — all tests are ran to check that new changes have not broken the existing functionality
- Lint — run scalafix. If it fails run `sbt scalafixAll` and fix issues that are not autofixable manually.

  [scalafix official documentation](https://scalacenter.github.io/scalafix/docs/users/installation.html#sbt) tells that
  SemanticDB compiler plugin with `-Yrangepos` flag adds overhead to the compilation, so it is recommended to create a
  local `.jvmopts` file with the following content:
  ```
    -Xss8m
    -Xms1G
    -Xmx8G
  ```
- Check code formatting — the code formatting will be checked via `scalafmt`. If it fails run `sbt scalafmtAll` and
  commit the updated formatting.

For more information, see `.github/workflows/ci.yml`.
