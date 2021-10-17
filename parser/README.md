# EO Parser with Fastparse

## Description

This submodule holds the source code for EO parser written in Scala using a library
called [`cats-parse`](https://github.com/typelevel/cats-parse). It should recognize any valid EO program and produce an
AST defined in the `core` module.

### Missing functionality

A part of the syntax features defined in the paper is missing in the implementation of this parser, namely:

- parameter aliases `obj a:paramA b:paramB`
- regex, boolean and bytes data objects are not implemented
- identifiers cannot include arbitrary unicode
- atom objects (`[other] > add /int[?]`) are not implemented

The missing functionality will be implemented upon request (submit an issue and/or PR).

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

## Motivation

The [existing EO parser implementation](https://github.com/cqfn/eo/tree/master/eo-parser) was not satisfactory for
several reasons:

- Its output is a bunch of XML files collectively known as XMIR. I have already
  had [some experience](https://github.com/polystat/eo2py/blob/main/eo2py-maven-plugin/src/main/resources/org.eolang.maven/pre/to-python.xsl)
  of working with this representation, and I'm not a fan of it.
- The existing parser is very restrictive when it comes to whitespace and comments. This implementation has lifted some
  of these constraints.

## Advantages:

- No need to know XML/XSLT to use it.
- Very easy to extend or modify.
- Directly usable in Scala and Java programs.
- Can produce arbitrary Java and Scala and classes.
- It has much better, highly customizable error-reporting abilities.
- Can potentially be extended to produce any kind of intermediate representation.

## Disadvantages

- The output of this parser can only be accessed from within Scala or Java programs. On the other hand, the AST can be
  serialized to produce very similar (if not the same) XML.
- Maintaining this parser requires knowing the specifics of Scala and `cats-parse`.
