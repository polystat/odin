package eo.parser.fastparse

import org.scalatest.wordspec.AnyWordSpec
import eo.parser.fastparse.Tokens._
import fastparse._
import NoWhitespace._
import eo.core.ast.EOMetas
import org.scalatest.Assertion


class EOParserTests extends AnyWordSpec {

  def parseEntireInput[_: P, T](p: => P[T]): P[T] = P(Start ~ p ~ End)

  def metasAllInput[_: P]: P[EOMetas] = parseEntireInput(Metas.metas)


  def shouldParse[T](parser: P[_] => P[T], input: String): Assertion = {
    val parsed = parse(input, parser)
    println(parsed)
    assert(parsed.isSuccess)
  }


  "tokens" should {
    "be recognized correctly" in {
      shouldParse(emptyLine(_), "      \t\n")
      shouldParse(comment(_), "#   oo 121 _= `12 e3\n")
      shouldParse(identifier(_), "a-COOL-identifier")
      shouldParse(string(_), "\" asd aa dd -= -21 123\"")
    }
  }

  "metas" should {
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

}
