package eo.parser.fastparse

import org.scalatest.wordspec.AnyWordSpec
import fastparse._
import NoWhitespace._
import eo.core.ast.EOMetas
import org.scalatest.Assertion
import org.scalatest.Inspectors.forAll


class EOParserTests extends AnyWordSpec {

  def parseEntireInput[_: P, T](p: => P[T]): P[T] = P(Start ~ p ~ End)


  def shouldParse[T](parser: P[_] => P[T], input: String): Assertion = {
    val parsed = parse(input, parser)
    println(parsed)
    assert(parsed.isSuccess)
  }

  def shouldFailParsing[T](parser: P[_] => P[T], input: String): Assertion = {
    val parsed = parse(input, parser)
    println(parsed)
    assert(!parsed.isSuccess)
  }


  "tokens" should {
    "be recognized correctly" in {
      shouldParse(Tokens.emptyLine(_), "      \t\n")
      shouldParse(Tokens.comment(_), "#   oo 121 _= `12 e3\n")
      shouldParse(Tokens.identifier(_), "a-COOL-identifier")
      shouldParse(Tokens.string(_), "\" asd aa dd -= -21 123\"")
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

    def argsAllInput[_ : P] =
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

  "anonymous abstraction" should {
    def anonymousAbstractionAllInput[_ : P] =
      parseEntireInput(new Objects().anonymousAbstraction)

    "be recognized correctly" in {
      shouldParse(anonymousAbstractionAllInput(_), "[]")
      shouldParse(anonymousAbstractionAllInput(_),
        """[]
          |  [a b c] > a
          |  [@...] > freeDecoratee
          |    [] > stuff
          |      [] > mmm
          |      [] > asd
          |    [] > noArgs
          |    [someArgs] > someArg""".stripMargin)
    }
  }

}
