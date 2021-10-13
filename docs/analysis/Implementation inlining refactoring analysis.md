# Implementation inlining refactoring analysis — Design document

This analysis is inspired by the paper [A Study of the Fragile Base Class Problem](https://www.researchgate.net/publication/248590657_A_Study_of_the_Fragile_Base_Class_Problem), specifically by the example `3.3 Unjustified Assumptions in Modifier`.

## Problem description

The original paper states the problem as follows:

![Unjustified assumption in modifier](img/unjustified-assumption-in-modifier.png)

After the revision of `C`, the behavior of `C'` is the same as it was before, but the behavior of `(M mod C)` is now different. It was expected that the modifier weakens the contract, but after the revision the contract is not weakened for methods `m` and `n`.

In the next section the problem is translated into C++, then in EO and finally a solution in EO is proposed.

### C++ example

The following examples are implemented in C++ and compiled via gcc 11.2 hosted at https://compiler-explorer.com with the following compiler flags:

```
--std=c++20 -Wall -Werror -Wpedantic -Wextra -Wconversion
```

> **Important**
> 
> `std::cout` is used to see in the output which methods are called and is assumed to be omitted when reasoning about the behavior of a program.

#### Base example

[Base example](https://compiler-explorer.com/z/Kq4Mb46cc).

```cpp
#include <iostream>
#include <cassert>

struct ICharacterCapabilities {
  virtual void checkMana(long mana) = 0;
  virtual void castSpell(long mana) = 0;
  virtual void attack(long mana) = 0;

  virtual ~ICharacterCapabilities() {};
};

class CharacterOps : virtual public ICharacterCapabilities {
public:
  virtual void checkMana(long mana) override {
    std::cout << "Checking character's mana" << "\n";
    assert(mana > 0);
  }

  virtual void castSpell(long mana) override {
    checkMana(mana);
    std::cout
      << "Character with " << mana
      << " mana casts a spell" << "\n";
  }

  virtual void attack(long mana) override {
    std::cout
      << "Character with " << mana
      << " mana attacks" << "\n";
  }

  virtual ~CharacterOps() {};
};

class GodModeCharacterOps : virtual public CharacterOps {
public:
  virtual void checkMana(long /*mana*/) override {
    std::cout << "The God has unlimited mana" << "\n";
  }

  virtual void attack(long mana) override {
    castSpell(mana);
  }

  virtual ~GodModeCharacterOps() {};
};

int main()
{
  long mana = 10;

  std::cout << "Regular character:" << "\n";

  CharacterOps* charOps = new CharacterOps();
  charOps->castSpell(mana);
  mana -= 5;
  charOps->attack(mana);
  delete charOps;

  std::cout << "\n" << "God character:" << "\n";

  charOps = new GodModeCharacterOps();
  charOps->castSpell(mana);
  charOps->attack(mana);
  delete charOps;

  return 0;
}
```

Output:

```
Regular character:
Checking character's mana
Character with 10 mana casts a spell
Character with 5 mana attacks

God character:
The God has unlimited mana
Character with 5 mana casts a spell
The God has unlimited mana
Character with 5 mana casts a spell
```

#### Revision

When the behavioral part of the implementation of `CharacterOps::checkMana` (i.e. `assert(mana > 0);`) [is inlined](https://compiler-explorer.com/z/f3q78GYs5) to the `CharacterOps::castSpell` instead of `checkMana(mana);`, the behavior of instance of the derived class changes.

```cpp
class CharacterOps : virtual public ICharacterCapabilities {
public:
  // ...
  virtual void castSpell(long mana) override {
    assert(mana > 0); // inlined
    std::cout
      << "Character with " << mana
      << " mana casts a spell" << "\n";
  }
  // ...
};
```

Output:

```
Regular character:
Character with 10 mana casts a spell
Character with 5 mana attacks

God character:
Character with 5 mana casts a spell
Character with 5 mana casts a spell
```

### EO example

In this section the example is translated to EO and the output shows that the same problem exists under the same conditions.

> **Important**
>
> `stdout` is used to see in the output which methods are called and is assumed to be omitted when reasoning about the behavior of a program.

#### Base example

```
+package sandbox
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

# Since EO does not have assert and
# we are not able to throw an exception
# or break the control flow in a similar
# manner, this wierd construct is used
# to simulate assert
[p cont] > assert
  if. > @
    p
    false
    cont

[] > character
  [self mana] > checkMana
    seq > @
      stdout "Checking character's mana\n"
      assert (mana.less 0) true
  [self mana] > castSpell
    seq > @
      self.checkMana self mana
      stdout
        sprintf
          "Character with %d mana casts a spell\n"
          mana
  [self mana] > attack
    seq > @
      stdout
        sprintf
          "Character with %d mana attacks\n"
          mana

[] > god
  character > @
  [self mana] > checkMana
    seq > @
      stdout
        sprintf "The God has unlimited mana\n"
  [self mana] > attack
    self.castSpell self mana > @

[args...] > app
  memory > mana
  character > regular_character
  god > god_character
  seq > @
    mana.write 10
    stdout
      sprintf "Regular character:\n"
    regular_character.castSpell regular_character mana
    mana.write (mana.sub 5)
    regular_character.attack regular_character mana
    stdout
      sprintf "\nGod character:\n"
    god_character.castSpell god_character mana
    god_character.attack god_character mana
    0
```

Output (the same as in the C++ example):

```
Regular character:
Checking character's mana
Character with 10 mana casts a spell
Character with 5 mana attacks

God character:
The God has unlimited mana
Character with 5 mana casts a spell
The God has unlimited mana
Character with 5 mana casts a spell
```

#### Revision

```
[] > character
  [self mana] > castSpell
    seq > @
      assert (mana.less 0) true  # inlined
      stdout
        sprintf
          "Character with %d mana casts a spell\n"
          mana
```

Output (the same as in the C++ example):

```
Regular character:
Character with 10 mana casts a spell
Character with 5 mana attacks

God character:
Character with 5 mana casts a spell
Character with 5 mana casts a spell
```

When the implementation of `character.checkMana` is inlined in the `character.castSpell` the behavior of the base class didn't change, while the derived class behavior is changed. 

## Reflexing on the examples

Usually, inheritance in OOP is used, when one wants to reuse the existing code. If we look at the relations between methods and how inheritance is used before revisiting the base class, we can notice the following facts:

1. `checkSpell` (`m`) calls `checkMana` (`l`) in `character` (`C`)
2. `checkSpell` (`m`) is not redefined by `god` (`M`)
3. `checkMana` (`l`) is redefined by `god` (`M`)

From these facts we can draw a conclusion, that in the modifier `god` (`M`), the method that is not redefined, i.e. `castSpell` (`m`) either:
1. refers to the method that is redefined, in which case the simple "inlining refactoring" will change the behavior of the method in the derived class, because the method will no longer call the redefined version
2. does not refer to any method that is redefined, in which case the revisiting of the method in base class in such a way that it will reference a method redefined in the derived class will change the behavior of the method in the derived class, because now it will call the redefined method

We will focus on the 1-st case only and propose a solution for it.

## Proposed solution (analysis description)

Having the class hierarchy in the state before revisiting the base class, the analyzer is able to perform the "implementation inlining refactoring" and compare how the derived class will look after refactoring with how it looks now and if the structure is different report that the program is designed poorly, because the inlining refactoring can change the derived class behaviour. 

### In terms of original problem formulation from the paper

1. Get `(M mod C)` by:
   1. substituting only non-overwritten members of `M` with those from `C`
   2. (recursively? No, because in case of recursion we will loop forever) substitute the self references to their bodies
2. Substitute all self calls (again problem with recursion) in `C` to the actual implementation to get `C'`
3. Get `(M mode C')`
4. Compare `(M mod C)` with `(M mod C')` by structure:
   - If the structure is the same => everything is ok
   - If the structure is different => report this fact

To cope with the recursion problem the analyzer needs to find the recursion first, which is not implemented yet for all cases. When this will be implemented the functions which are recursive are not subject to implementation inlining. For now the following heuristics will be used: the maximum number of substitutions will be equal to them number of methods in the "class".

### False positive

- If it is assumed that the base class will never be refactored in such way, then the report of the poor design can be considered a false positive

### False negative

Under the specified conditions in this document we are not able to come up with a counterexample that will result in not detecting such a bug. The contribution is encouraged!
