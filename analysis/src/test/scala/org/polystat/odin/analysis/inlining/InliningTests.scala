package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec
import SetLocatorsTestCases._
import InlineCallsTestCases._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.data.{EitherNel, NonEmptyList => Nel}
// import org.polystat.odin.core.ast._

class InliningTests extends AnyWordSpec {

  "setLocators" should {
    val locatorTests: List[LocatorTestCase] = List(
      vitaliyTestLocators,
      nikolayTestLocators,
      nonExistentNameTestLocators,
      builtinObjects,
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
      withInheritance,
    )
      .concat(simpleTests)

    inliningTests.foreach { case InliningTestCase(label, before, after) =>
      registerTest(label) {
        val expected = after
        val obtained = Parser
          .parse(before)
          .leftMap(Nel.one)
          .flatMap(Inliner.inlineAllCalls)
          .map(_.toEOPretty)

        println(
          Parser
            .parse(before)
            .leftMap(Nel.one)
            .flatMap(Inliner.createObjectTree)
            .flatMap(
              _.toList.traverse(kv => Inliner.zipWithInlinedMethod(kv._2))
            )
        )

        assert(obtained == expected)
      }

    }
  }

}

object InliningTests {

  def main(args: Array[String]): Unit = {
    val code = """
                 |[] > obj
                 |  [self y] > g
                 |    3.div y  > @
                 |
                 |  [self z] > method
                 |    self.g self z > x
                 |    x > @
                 |    
                 |  
                 |  [] > bebra
                 |    ^.zhepa > @
                 |    [self] > aboba
                 |      12 > @
                 |  [] > zhepa
                 |    opa > @
                 |    [self] > aboba
                 |      1 > @
                 |    [self] > indirect
                 |      2 > @
                 |  [] > opa
                 |    [self] > even-more-indirect
                 |      3 > @
                 |    
                 |  
                 |
                 |[] > derived
                 |  obj > @
                 |  [self y] > g
                 |    3.div (y.add 1) > @  
                 |  [] > kukozh
                 |    ^.^.obj.bebra > @
                 |    
                 |[] > am
                 |  derived > @
                 |""".stripMargin

    val parsed = Parser.parse(code)

//    val tree = parsed
//      .leftMap(Nel.one)
//      .flatMap(Inliner.createObjectTree)

//    val newTree = tree
//      .flatMap(Inliner.resolveParents)

    parsed
      .map(Inliner.zipMethodsWithTheirInlinedVersionsFromParent)
      .bimap(pprint.pprintln(_), pprint.pprintln(_))
      .merge

  }

}
