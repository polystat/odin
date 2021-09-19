# EO Parser with Fastparse

## Description

This submodule holds the source code for EO parser written in Scala using a library
called [`fastparse`](https://github.com/com-lihaoyi/fastparse). It should recognize any valid EO
program and produce an AST defined in
the `core` module.

### Missing functionality
A part of the syntax features defined in the paper is missing in the implementation of this parser, namely:
 - array literals: `* a b c` (implemented, needs more tests)
 - parameter aliases `obj a:paramA b:paramB`
 - not all literals (regexes, identifiers, floating point numbers, etc) are defined according to the specifications (if at all)

The missing functionality will be implemented upon request (submit an issue and/or PR).

### Syntax Specification

When working on this parser, I tried to make it as close as possible
to [specifications from the paper (Section 2, Figure 1)](https://www.eolang.org/eolang-paper.pdf), although, due to
specifics of Tymur's AST and the `fastparse` library, there were some diversions.

### Tests

Most EO programs that were used to test this parser (but not all) are
available in `parser/src/test/resources/eo_sources`.
The tests themselves can be found in the form of ScalaTest unit 
tests in `parser/src/test/scala/org/polystat/odin/parser/fastparse`.

## Motivation

The [existing EO parser implementation](https://github.com/cqfn/eo/tree/master/eo-parser) was not satisfactory for
several reasons:

- Its output is a bunch of XML files collectively known as XMIR. I have already
  had [some experience](https://github.com/polystat/eo2py/blob/main/eo2py-maven-plugin/src/main/resources/org.eolang.maven/pre/to-python.xsl)
  of working with this representation, and I'm not a fan of it.
- Tymur's AST is defined as a suite of Scala classes, as opposed to XML, which is a file format. In other words, Tymur's
  AST exists at runtime, whereas XMIR exists on the hard drive. Converting XMIR into AST directly would be a lot more
  challenging due to the team's lack of knowledge and competence in using XML-related tools, such XSLT, XPath, etc.
- The existing parser implementation is not without its flaws:
    - https://github.com/cqfn/eo/issues/295
    - https://github.com/cqfn/eo/issues/285

## Advantages:

- No need to know XML/XSLT to use it.
- Although written in Scala, can be used in Java programs too (I believe).
- The parser is written in pure Scala, so it's just as readable and maintainable as Scala code (debatable, though).
- Since the parser is implemented in Scala, it also has access to many existing tools and libraries in Scala and Java,
  which XSLT does not.
- It has much better error-reporting abilities.

## Disadvantages

- The output of this parser can only be accessed from within Scala or Java programs. (Although I think AST can
  potentially be serialized to produce a similar (if not the same) AST)
- Maintaining this parser requires knowing the specifics of Scala and `fastparse`.
