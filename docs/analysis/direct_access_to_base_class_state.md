# Direct Access to the Base Class State

The fourth defect type suported by odin.

## Original Problem Statement

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
We will have the following: the modification `M`, if applied to class `C` might cause unnecessary confusion. The initial implemntation maintains an implicit invariant `x=y` by using `x := y` at the last action of every method.

However, the redefinition of `n` in `M` causes the invariant to be broken in case of a calling the modified version of `n` after `m`. This will cause `y` to be equal to 1, and `x` to be equal to 3.

Conversely, the sequence of calls `n;m` will cause a different kind of confusion. By looking at `C`, one could assume that the  method calls will make `x` equal to 3, whereas, in fact, `x` will be assigned only 1.

Thus, the best way to avoid such confusion is by only allowing changes to the variables defined in the base class to be made via the corresponding methods of the base class.


## EO Equivalnet of the Statement
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
Access to state of any superclass is considered a bad practice.
Examples can be found in functions `read_func` and `bad_func`.

> NOTE: access to local state, such as `local_state` is not considered harmful.
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

## Brief description of the Devised algorithm
1. Build the `Tree` structure from the source code and resolve all parents.
2. Collect all existing subclasses.
3. Identify the state variables (ones that contain `memory` or `cage`) accessible by each target subclass.
4. If some method of the target subclass accesses a state variable present in its hierarchy - a message similar to the following is generated:
   `
   Method 'change_state_plus_two' of object 'b' directly accesses state 'state' of base class 'a'
   `

## Implementation Highlihts
1. Only variables that are accessed using the `self` object are considered. 
So, `self.state.write 10` is considered, while `state.write 10` is not considered, 
since it can potentially be a local variable.
2. The hierarchy where the defect takes place can be nested in objects.
3. Access to the state refers to actions that either read the state or call functions on it.


## Possible limitations
1. Variables can be accessed without using `self`. The current implementation does not consider such cases. 