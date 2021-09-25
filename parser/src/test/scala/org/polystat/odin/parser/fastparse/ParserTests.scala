package org.polystat.odin.parser.fastparse

import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import IgnoreEmptyLinesOrComments._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.TestUtils.{astPrinter, fileNameOf, getListOfFiles, readCodeFrom}
import org.polystat.odin.parser.{MutualRecExample, SingleLineExamples}
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll



class ParserTests extends AnyWordSpec {



  def parseEntireInput[_: P, T](p: => P[T]): P[T] = P(Start ~ p ~ End)

  def checkParser[T](
                      parser: P[_] => P[T],
                      input: String,
                      check: Parsed[T] => Boolean
                    ): Assertion = {
    val parsed = parse(input, parser)
    parsed match {
      case Parsed.Success(value, _) => astPrinter.pprintln(value)
      case failure: Parsed.Failure => println(failure.trace())
    }
    assert(check(parsed))
  }

  def shouldParse[T](parser: P[_] => P[T], input: String): Assertion = {
    checkParser(parser, input, (result: Parsed[T]) => result.isSuccess)
  }

  def shouldFailParsing[T](parser: P[_] => P[T], input: String): Assertion = {
    checkParser(parser, input, (result: Parsed[T]) => !result.isSuccess)
  }

  def shouldProduceAST[AST](
                           parser: P[_] => P[AST],
                           input: String,
                           expectedAST: AST
                         ): Assertion = {
    checkParser(parser, input,
      check = (res: Parsed[AST]) => {
        res match {
          case Parsed.Success(value, _) => value == expectedAST
          case _: Parsed.Failure => false
        }
      }
    )
  }


  "tokens" should {
    "be recognized correctly" in {
      shouldParse(Tokens.emptyLine(_), "      \t\n")
      shouldParse(Tokens.comment(_), "#   oo 121 _= `12 e3\n")
      shouldParse(Tokens.identifier(_), "a-COOL-identifier")
      shouldParse(Tokens.string(_), "\" asd aa dd -= -21 123\"")
      shouldParse(Tokens.integer(_), "12345013")
      shouldParse(Tokens.integer(_), "-12345013")
      shouldParse(Tokens.char(_), "\'a\'")
    }
  }

  "metas" should {
    def metasAllInput[_: P]: P[EOMetas] = parseEntireInput(Parser.metas)

    "be recognized correctly" in {
      shouldParse(metasAllInput(_),
        """
          |
          |# start
          |
          |+package sandbox
          |
          |# meta specifying the current runtime for eo
          |+rt jvm org.eolang:eo-runtime:0.1.24
          |
          |# meta used to rename imported packages
          |  # can be used to resolve name conflicts or
          |  # to make the names more succinct
          |+alias stdout org.eolang.io.stdout
          |
          |
          |# some comment here
          |# some comment
          |
          |
          |
          |+alias sprintf org.eolang.txt.sprintf
          |
          |+alias biba boba
          |""".stripMargin)
    }
  }

  "args" should {

    def argsAllInput[_: P] =
      parseEntireInput(SingleLineApplication.args)

    val correctArgsExamples = List(
      "[a b  c    d...]",
      "[a b c d]",
      "[a b @]",
      "[@...]",
      "[]"
    )

    val incorrectArgsExamples = List(
      "[",
      "[...]"
    )

    forAll(correctArgsExamples) {
      example =>
        example in {
          shouldParse(argsAllInput(_), example)
        }
    }

    forAll(incorrectArgsExamples) {
      example =>
        example in {
          shouldFailParsing(argsAllInput(_), example)
        }
    }
  }

  "abstraction" should {
    def anonymousAbstractionAllInput[_: P] =
      parseEntireInput(AnonymousObjects.anonymousAbstraction(0, 2))

    val correctExamples = List(
      "simplest possible object" ->
        "[]",

      "many nested objects, formatted correctly" ->
        """[]
          |  [a b c] > a
          |  [@...] > freeDecoratee
          |    [] > stuff
          |      [] > mmm
          |      [] > asd
          |    [] > noArgs
          |    [someArgs] > someArg""".stripMargin,

      "shows the flexibility of whitespace" ->
        """[]
          |  # This is a
          |  [] > a
          |
          |  # This is b
          |
          |  [] > b
          |
          |
          |  # This is c.
          | # This line is here
          |    # to show that the comments can be
          |# multiline and don't have to be properly indented
          |  [] > c""".stripMargin
    )

    val incorrectExamples = List(
      "simplest malformed object" ->
        "[",

      "object with inconsistent indentation" ->
        """[]
          |  [] > objects
          |  [] > however
          |   [] > haveTo
          |   [] > beProperlyIndented
          |
          |""".stripMargin
    )

    forAll(correctExamples) {
      case (label, example) =>
        label in {
          shouldParse(anonymousAbstractionAllInput(_), example)
        }
    }

    forAll(incorrectExamples) {
      case (label, example) =>
        label in {
          shouldFailParsing(anonymousAbstractionAllInput(_), example)
        }
    }
  }

  "application" should {
    def namedApplicationAllInput[_: P] =
      parseEntireInput(NamedObjects.namedApplication(0, 2))

    val correctExamples = List(
      "some simple objects" ->
        """a > namedA
          |  b > namedB
          |    b2
          |  c > namedC
          |    c2
          |      c3 > namedC3
          |  []
          |    # some anonymous object
          |    [] > someArg
          |  d""".stripMargin,

      "some data objects" ->
        """a > namedA
          |  123
          |  'a'
          |  "a huge long long long string"
          |  a""".stripMargin,
      "some attribute chains" ->
        """a.b.c.d > namedA
          |  $.^.@.a > some-obj-from-outer-scope
          |  1.neg""".stripMargin,

      "some single line applications" ->
        """(a) > named-a-with-redundant-parentheses
          |
          |  # a applied to b, c and
          |  a b c d > normalApplication
          |
          |  # a applied to (b applied to (c applied to d))
          |  a (b (c d)) > cascadingApplication
          |
          |  # this a reverse application:
          |  # ((a applied to b) applied to c)
          |  ((a b) c) d > reverseApplication""".stripMargin,

      "anonymous inverse dot applications" ->
        """a > namedA
          |  # testing anonymous
          |  # inverse-dot applications
          |  a.
          |    b.
          |      c.
          |        d""".stripMargin,

      // TODO: here the information about inner names is lost
      "named inverse-dot applications" ->
        """a > main
          |  a. > d-c-b-a
          |    b. > d-c-b
          |      c. > d-c
          |        d > d-""".stripMargin
    )

    forAll(correctExamples) {
      case (label, example) =>
        label in {
          shouldParse(namedApplicationAllInput(_), example)
        }
    }
  }

  "arrays" should {

    val correctExamples = List(
      "simple single line array" ->
        """
          |* "Lucy" "Jeff" 314 > stuff
          |""".stripMargin
      ,

      "nested single line array" ->
        """
          |* (* deep stuff here) 'a' 'b' 'c' > some_deep_stuff
          |
          |""".stripMargin
      ,
    "simple multiline array" ->
      """
        |*
        |  "hello"
        |  "world"
        |  'I'
        |  "am"
        |  "array"
        |  "I have no name"
        |""".stripMargin
      ,
    "nested multiline array" ->
      """
        |* > cool
        |  "hello"
        |  "world"
        |  *
        |    'I'
        |    "am"
        |    "array"
        |  *
        |    *
        |      "My name is cool"
        |      *
        |        *
        |          literally
        |""".stripMargin

    )

    forAll(correctExamples) {
      case (label, example) =>
        label in {
          shouldParse(Parser.program(0, 2)(_), example)
        }
    }
  }

  "existing programs" should {
    forAll(getListOfFiles("/eo_sources")) {
      source =>
        fileNameOf(source) in {
          shouldParse(Parser.program(0, 2)(_), readCodeFrom(source))
        }
    }

    "mutual recursion example" in {
      shouldProduceAST[EOProg[EOExprOnly]](
        Parser.program(0, 2)(_),
        MutualRecExample.code,
        MutualRecExample.ast
      )
    }

    forAll(SingleLineExamples.correct){
      case (label, (code, ast)) =>
        label in {
          shouldProduceAST[EOExprOnly](
            SingleLineApplication.singleLineApplication(_),
            code,
            ast
          )
        }
    }
  }


}
