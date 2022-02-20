package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec
import SetLocatorsTestCases._
import InlineCallsTestCases._
import cats.syntax.either._
import cats.data.{EitherNel, NonEmptyList => Nel}

class InliningTests extends AnyWordSpec {

  "setLocators" should {
    val locatorTests: List[LocatorTestCase] = List(
      vitaliyTestLocators,
      nikolayTestLocators,
      nonExistentNameTestLocators,
    )
    locatorTests.foreach { case LocatorTestCase(label, before, after) =>
      registerTest(label) {
        val expected: EitherNel[String, String] = after
        val obtained: EitherNel[String, String] = Parser
          .parse(before)
          .leftMap(Nel.one)
          .flatMap(Context.setLocators)
          .map(_.toEOPretty)
        assert(expected == obtained)
      }
    }
  }

  "inlineCalls" should {
    val inliningTests: List[InliningTestCase] = List(
      nikolayTestInlining,
      vitaliyTestInlining,
      fakeCallTest,
      looksFakeButRealTest,
      factorialTest,
      evenOddTest,
      average3Test,
      average3WithComponentsTest,
      notEnoughArgs,
      tooManyArgs,
    ) ++
      simpleTests

    inliningTests.foreach { case InliningTestCase(label, before, after) =>
      registerTest(label) {
        val expected = after
        val actual = Parser
          .parse(before)
          .leftMap(Nel.one)
          .flatMap(Inliner.inlineAllCalls)
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
