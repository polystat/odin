package eo.parser.fastparse
import org.scalatest.wordspec.AnyWordSpec
import eo.parser.fastparse.Tokens._
import fastparse._
import org.scalatest.Assertion


class EOParserTests extends AnyWordSpec {

  def shouldParse[T](parser: P[_] => P[T], input: String): Assertion = {
    val parsed = parse(input, parser)
    println(parsed)
    assert(parsed.isSuccess)
  }
  "tokens" should {
    "be recognized correctly" in {
      shouldParse(comment(_), "#   oo 121 _= `12 e3\n")
      shouldParse(identifier(_), "a-COOL-identifier")
      shouldParse(string(_),"\" asd aa dd -= -21 123\"" )
      shouldParse(meta(_),"+meta some-stuff-H3ЯE   \n" )
    }
  }

}
