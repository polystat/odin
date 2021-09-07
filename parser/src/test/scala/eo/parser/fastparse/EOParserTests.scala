package eo.parser.fastparse

import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import NoWhitespace._
import eo.core.ast.EOMetas
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll


class EOParserTests extends AnyWordSpec {

  def parseEntireInput[_: P, T](p: => P[T]): P[T] = P(Start ~ p ~ End)

  def checkParser[T](
                      parser: P[_] => P[T],
                      input: String,
                      check: Parsed[T] => Boolean
                    ): Assertion = {
    val parsed = parse(input, parser)
    println(parsed)
    assert(check(parsed))
  }

  def shouldParse[T](parser: P[_] => P[T], input: String): Assertion = {
    checkParser(parser, input, (result: Parsed[T]) => result.isSuccess)
  }

  def shouldFailParsing[T](parser: P[_] => P[T], input: String): Assertion = {
    checkParser(parser, input, (result: Parsed[T]) => !result.isSuccess)
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
    def metasAllInput[_: P]: P[EOMetas] = parseEntireInput(Metas.metas)

    "be recognized correctly" in {
      shouldParse(metasAllInput(_),
        """
          |
          |+package sandbox
          |
          |+rt jvm org.eolang:eo-runtime:0.1.24
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
      parseEntireInput(new Objects().args)

    val correctArgsExamples = List(
      "[a b  c    d...]",
      "[a b c d]",
      "[a b @]",
      "[@...]",
      "[]"
    )

    forAll(correctArgsExamples) {
      example =>
        example in {
          shouldParse(argsAllInput(_), example)
        }
    }
  }

  "abstraction" should {
    def anonymousAbstractionAllInput[_: P] =
      parseEntireInput(new Objects().anonymousAbstraction)

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

       "object with inconsisten indentation" ->
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
}
