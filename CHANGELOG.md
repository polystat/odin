# v0.4.0
- Added direct access to base class analyzer (4th defect type) (see [#48](https://github.com/polystat/odin/pull/48))
- Added Liskov substitution principle analyzer (5th defect type) (see [#51](https://github.com/polystat/odin/pull/51))
- Improved the logic extraction algorithm (used in detection of defects 3 and 5) such that it supports functions with regular/mutual recursion (see [#51](https://github.com/polystat/odin/pull/51))
- Added support for imports without the alias (see [#44](https://github.com/polystat/odin/pull/44))
- Fixed the XMIR-to-AST parser such that it recognizes Array data correctly (see [#50](https://github.com/polystat/odin/pull/50))
- Revamped documentation such that it is contained within the README file (see [#53](https://github.com/polystat/odin/pull/53))


# v0.3.2
- Fixed setting locators for common EO objects (see [41fbce4](https://github.com/polystat/odin/commit/41fbce4c313b4fc1def41d799d5eb5a422e5e630))
- Partially rewrote the mutual recursion analyzer for better integration with the `inlining` module (see [#37](https://github.com/polystat/odin/pull/37)) 

# v0.3.1
- Fixed a bug in `inlining` that caused methods from decorated classes to not be inlined (see [891f4e3](https://github.com/polystat/odin/commit/891f4e3f4d6d156a1a5e0b6fb92a385eeac6fb79))
- Made SMT-solver not output information to the console (see [bd0d95c](https://github.com/polystat/odin/commit/bd0d95c6d5b11d384ea5b3ae8cea0a97a6257aa0))  

# v0.3.0
- Added a new EO AST node (`EOSimpleAppWithLocator`) to store locator information (see [f18e026b08](<https://github.com/nikololiahim/odin/blob/f18e026b0844904c516b315577619bf4d7c7fabf/core/src/main/scala/org/polystat/odin/core/ast/ast.scala#:~:text=sealed%20case%20class%20EOSimpleAppWithLocator,)%20extends%20EOApp%5BA%5D>)) 
- Added support for locators during parsing (see [#28](https://github.com/polystat/odin/pull/28))
- Improved the pretty-printer (see [#28](https://github.com/polystat/odin/pull/28))
- Added an algorithm for setting locators in the AST by replacing plain `EOSimpleApp`s with `EOSimpleAppWithLocator`s  (see [#28](https://github.com/polystat/odin/pull/28))
- Added an [`inlining`](https://github.com/nikololiahim/odin/tree/b3aeb59dbe4d478fda8a9424cb40ae6e9b39bfb5/analysis/src/main/scala/org/polystat/odin/analysis/inlining) module that allows all calls in the object to be inlined (see [#30](https://github.com/polystat/odin/pull/30))
- Made it possible to derive logical expressions from EO AST (see [#32](https://github.com/polystat/odin/pull/32))
- Added a polystat facade for the unjustified assumption analyzer (see [5fc07ff](https://github.com/polystat/odin/pull/32/commits/5fc07fffe7b044c3673ea15831bea860964924f3)) 

# v0.2.1

- Added documentation to mutual recursion analyzer (see [#24]( https://github.com/polystat/odin/pull/24 ));

# v0.2.0

- Improved the algorithm of mutual recursion analyzer (see [#11]( https://github.com/polystat/odin/pull/11 ));
- Rewrote EO parser with `cats-parse` (see [#10]( https://github.com/polystat/odin/pull/10 ));
- Refactoring XMIR parser (see [#9]( https://github.com/polystat/odin/pull/9 ));
- Make printing EO AST easier (see [2bf552e]( https://github.com/polystat/odin/commit/2bf552e ));
