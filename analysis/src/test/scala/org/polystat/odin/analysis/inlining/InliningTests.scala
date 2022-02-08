package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec
import SetLocatorsTestCases._
import InlineCallsTestCases._

class InliningTests extends AnyWordSpec {

  "setLocators" should {
    val locatorTests: List[LocatorTestCase] = List(
      vitaliyTestLocators,
      nikolayTestLocators,
    )
    locatorTests.foreach { case LocatorTestCase(label, before, after) =>
      registerTest(label) {
        val expected: Either[String, String] = Right(after)
        val obtained: Either[String, String] = Parser
          .parse(before)
          .map(Context.setLocators _ andThen (_.toEOPretty))
        assert(expected == obtained)
      }
    }
  }

  "inlineCalls" should {
    val inliningTests: List[InliningTestCase] = List(
      nikolayTestInlining,
      vitaliyTestInlining,
    )

    inliningTests.foreach { case InliningTestCase(label, before, after) =>
      registerTest(label) {
        val expected: Either[String, String] = Right(after)
        val actual: Either[String, String] = Parser
          .parse(before)
          .flatMap(Inliner.inlineCalls)
          .map(_.toEOPretty)
        assert(actual == expected)
      }

    }
  }

}

object InliningTests {

  def main(args: Array[String]): Unit = {
    val code = """[] > a
                 |  [self y] > x
                 |    y > @
                 |
                 |  [self x y] > f
                 |    self.g self x > h
                 |    [] > @
                 |      self.g self y > z
                 |
                 |  [self z] > g
                 |    x > k
                 |    z > l
                 |    [] > @
                 |      l > a
                 |      k > b
                 |      z > c
                 |      self > d
                 |""".stripMargin

    Parser.parse(code) match {
      case Left(value) => println(value)
      case Right(value) => pprint.pprintln(value)
    }
  }

}
