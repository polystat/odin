# Odin: Detection Of Unjustified Assumptions In Subclasses

For this type of defect we detect whether the refactoring of superclasses by inlining can affect the functionality of subclasses in an undesired way. The notion of unjustified assumption in this context refers to assumptions made in subclasses regarding method dependencies in superclasses.



## Brief algorithm description
The following stages and steps take place during program analysis:
#### Setting locators
0. The source code of the program is parsed to build its corresponding AST.
1. Identifiers without explicitly set locators get locators derived and set.
#### Inlining
2. Object hierarchy is analyzed to set apart methods that comply with specific criteria (can be found below) and are not redefined.
3. Every method in the resulting set is paired with its  version in which all valid calls (criteria can be found below) to other methods of the containing object are inlined non-recursively.
#### Logic extraction
4. Logical properties (constraints on variables in the form of a logical expression) are extracted for both versions of the method in the pair.
5. The derived expression for the version before inlining is connected with the expression after inlining by the means of implication. Doing so allows us to discover whether the constraints on the variables have become weaker or stronger.
6. We deduce that the unjustified assumption takes place if the resulting logical expression is false, meaning that there exist some inputs such that they worked before the inlining, and stopped working after.


## In-depth description of algorithms

### Requirements for the methods to be chosen for analysis:

1. Its value is an object term with void self and attached @ attributes
2. There are no references to its @ attribute in any of its attached attributes.

### Requirements for the call to be considered valid during inlining:

1. The called method belongs to the same object as the method containing the call.
2. The call the general form `self.method self ...`. Meaning that `self` is used as both the source for the method and is passed as the first argument.

### Algorithm for setting locators:
0. Explicit locator chains of from (^.^.value) are resolved at parse-time and are converted to corresponding AST-nodes.
1. All predefined EO-keywords are stored in a special structure called 'context' and have locators point to the root of the program.
2. The first step of actual processing extends the existing context by adding all top-level objects.
3. Each top-level object is recursively explored while keeping track of encountered objects and their depth in the context.
4. The terms that refer to the definitions in the current scope are given 0-locators (\$), while terms contained in the context are given locators by subtracting their depth from current depth.

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
### Inlining algorithm:
0. All objects are extracted from the AST
1. Every AST-object is converted into the 'Object' data structure
2. Each created 'Object' is processed as follows:
   2.1 All calls in the object are processed, while non-call binds are returned as is.
   2.1.1 Replace occurrences of formal parameters in the method body with the argument values.
   2.1.2 If the method being inlined contains local bindings other than `@` attribute, extract them into the `local-attrs` object and put this object adjacent to the call-site.
   2.1.3. References to local attributes are replaced with their counterparts from the `local-attrs` object.
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

### Algorithm for logical expression extraction:
0. The list of recognized terms and their corresponding expressions/properties:
   ![](https://i.imgur.com/PBFB7ac.png)
1. All methods that can be called from the target method are collected and stored.
2. Properties and values of each function in the set are derived according to the above rules. This is done to account for their potential calls. The resulting SMT-code has all properties and returned expressions of every functions listed at the beginning of top of the file as function definitions.
3. The target method and its version with inlined calls are recursively processed on field-by-field basis.
4. The acquired logical expressions are connected using the implication operation and assertion in the following manner: `assert(logic-before-inlining => logic-after-inlining)`.
5. The resulting formula is then complemented by ``(check-sat)`` and ``(get-model)`` commands and sent the SMT-solver for further processing.

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

## Current Limitations:
1. Method pairs for inspection are formed by considering only direct descendants. For example, in case of an inheritance chain A -> B -> C only combinations (A,B) and (B,C) are examined. It would be ideal to examine all possible combinations. So, in the given example an additional pair (A,C) is to be examined.
<!-- 2. Only NOT redefined methods are examined for the presence unjustified assumptions. This might not be the ideal approach? -->
2. Presence of methods with mutual recursion causes the program to be unable to detect defects of related to unjustified assumptions.
3. Current implementation makes it impossible to store boolean values in object fields. Only integer values are supported as of now.
4. No checks are performed on the contents of asserts in EO. As such, it is possible to assert an integer value, which would cause unanticipated behavior. For example, `assert (3.add 5)`.
5. Imported functions are not supported by the analyzer.
6. Methods without arguments are not yet supported.

## Possible Errors
The error messages that the analyser can fail with at each stage are listed here.

### Setting locators
#### 1. The analyzer encounters a reference to a non-existent object.
```
[] > a
  [self] > method
    b > @
```
> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Could not set locator for non-existent object with name b` 
</span>

### Inlining
#### 1. Decoratee is not a valid object.
```
1 < non-existent
[] > a
  non-existent > @
```
> <span style="color:red"> `java.lang.Exception: There is no such parent with name "^.non-existent"`
</span>

#### 2. Locators point too broadly.
```
[] > b
[] > a
  ^.^.b > @
```
> <span style="color:red"> `java.lang.Exception: Locator overshoot at ^.^.b `

</span>

#### 3. Attempt to call a method that does not exist.
```
[] > a
  [self] > f
    self.non-existent self > @
```
> <span style="color:red">`java.lang.Exception: Inliner: attempt to call non-existent method "non-existent"`
</span>

#### 4. Attempt to call a method with the wrong number of arguments.
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
Wrong number of arguments given for method f.
Wrong number of arguments given for method f.
`

</span>


### Logic extraction
#### 1. Objects with void attributes as local definitions of methods are not supported.
```
[self x] > f
  22 > local-def
  [void] > local-obj
```
> <span style="color:red"> `Exception in thread "main" java.lang.Exception: object with void attributes are not supported yet!` </span>.

#### 2. Attempt to access attributes of applications.
```
  [] > parent
    [self x] > f
      (self.g 10).@ > res
      10 > @
    [self y] > g
      y.add 1 > @
```
> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Cannot analyze dot of app: (self.g 10).@` </span>.
#### 3. Using a primitive that is not yet supported.
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
#### 4. Call to a method with no arguments.
```
  [] > parent
    [self x] > f
      (self.g 10).@ > res
      10 > @
    [self y] > g
      y.add 1 > @
```
> <span style="color:red"> `Exception in thread "main" java.lang.Exception: Methods with no arguments are not supported` </span>.
#### 5. Some SMT-related issue.
> <span style="color:red"> `Exception in thread "main" java.lang.Exception: SMT solver failed with error: ??? </span>.`

> <span style="color:red"> `Exception in thread "main" java.lang.Exception: SMT failed to parse the generated program with error: ???` </span>.
