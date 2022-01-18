# Odin: Mutual Recursion Analyzer

The analyzer is capable of detecting mutual recursion caused by method redefinition during inheritance.

## Identifiable patterns
Only the following patterns are recognized by the algorithm:

### 1. Object declarations:
```
[] > name
  {0 or 1 decoratee}
  {>= 0 method declarations}
  {>= 0 object declarations}
```

### 2. Decoratee
```
simple_name > @
or
an.attribute.of.some.object > @
```

### 3. Method declarations:
```
[self {>= 0 params}] > methodName
{>= 0 method calls} > @
```

### 4. Method calls:
```
self.methodName self {>= 0 params}
```



## Known Shortcomings

### 1. Some decorations are ignored.

The algorithm supports only two kinds of decorations: simple applications, like `fruit > @`, and attribute decorations, like `fruits.citrus.orange > @`. **All the other patterns are ignored, thus an object containing such pattern is considered to not have a `@` attribute at all!**

For example, in this program:
```
[] > baobab
   1.add 2 > @
   [self] > stall
```
the statement `1.add 2 > @` is ignored so,
the algorithm 'sees' this program like so:
```
[] > baobab
  [self] > stall
```

### 2. Unreachable code
The algorithm takes into consideration every possible method call that is present in the body, whether it is actually called does not matter. For example, in method `zen`:
```
[self] > zen
  if. > @
    false
    self.no self
    self.yes self
```
The algorithm will consider both `no` and `yes` method calls, even though `no` is never called.

The same goes for unused local definitions. In EO, "method" is an object containing a special `@` (phi) attribute. When the method is called:
```
self.method self
```

it is the object labelled by `@` that gets executed. Any other local bindings that are not used in the binding associated with `@` are ignored. For example:
```
[self] > m
  fib 100000 > huge_computation!
  1.add 1 > @
```

Only `1.add 1` will be computed when method `m` is called. The `huge_computatuion` will never be performed (since it is never called in the declaration of `@`).

That being said, if `huge_computation` is a method call, say:
```
[self] > m

  # a valid method call now!
  self.fib self 100000 > huge_computation!
  1.add 1 > @
```

The algorithm would still consider it as "called". In other words, it will be a part of the  analyzer output if it is a part of some mutual recursion chain.

Furthermore, the algorithm does not run any checks for the presence of `@` attribute. As a result, a method that would produce a runtime error when called, like:

```
[self] > m
  self.fib self 100000 > huge_computation!
  
  # this is no longer `@`
  1.add 1 > two 
```

is still considered by the algorithm.

### 3. Inheritance chains with cycles
Even though the following code is perfectly valid in EO:
```
[] > a
  b > @

[] > b
  a > @
```

This code (and the similar variations) will cause the analyzer to fail with `StackOverflowError`.

## Algorithm description
1. Parse the source code to build its AST.

2. Create a tree of partial objects that preserves the object nestedness. Basically, collect all the information that can be collect within a single pass.

3. This tree is then refined to create a complete representation of the program, with method redefinition.

4. Traverse the callgraphs finding cycles that involve methods coming from multiple objects. For example, the callchain ***a.f -> a.g -> a.f*** will not be in the output, meanwhile the callchain  ***a.f -> b.g -> a.f*** will be a part of it.


## Expected output

Example input:

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

Example output (prettified):
```
t: 
  t.h (was last redefined in "c.e") ->
  t.f ->
  t.h (was last redefined in "c.e")

```

## Possible errors
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