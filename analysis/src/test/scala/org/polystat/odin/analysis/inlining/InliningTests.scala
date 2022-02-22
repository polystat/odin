package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.parser.eo.Parser
import SetLocatorsTestCases._
import InlineCallsTestCases._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.data.{NonEmptyList => Nel}

class InliningTests extends munit.FunSuite {

  val locatorTests: List[LocatorTestCase] = List(
    vitaliyTestLocators,
    nikolayTestLocators,
    nonExistentNameTestLocators,
  )

  locatorTests.foreach { case LocatorTestCase(label, before, after) =>
    test("setLocators - " + label) {
      val expected = after
        .leftMap(_.mkString_(util.Properties.lineSeparator))
        .merge
      val obtained = Parser
        .parse(before)
        .leftMap(Nel.one)
        .flatMap(Context.setLocators)
        .map(_.toEOPretty)
        .leftMap(_.mkString_(util.Properties.lineSeparator))
        .merge

      assertNoDiff(obtained, expected)
    }
  }

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
    test("inlineAllCalls - " + label) {
      val expected = after
        .leftMap(_.mkString_(util.Properties.lineSeparator))
        .merge
      val obtained = Parser
        .parse(before)
        .leftMap(Nel.one)
        .flatMap(Inliner.inlineAllCalls)
        .map(_.toEOPretty)
        .leftMap(_.mkString_(util.Properties.lineSeparator))
        .merge

      assertNoDiff(obtained, expected)

    }
  }

}
