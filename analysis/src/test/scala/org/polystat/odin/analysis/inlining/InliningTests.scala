package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec
import InlingTestCases._

class InliningTests extends AnyWordSpec {

  "setLocators" should {
    val locatorTests: List[LocatorTestCase] = List(
      vitaliyTest,
      nikolayTest
    )
    locatorTests.foreach { case LocatorTestCase(label, before, after) =>
      registerTest(label) {
        val expected: EOProg[EOExprOnly] = after
        val obtained: EOProg[EOExprOnly] = Context.setLocators(before)
        println("Expected: ")
        println(expected.toEOPretty)
        println("Obtained: ")
        println(obtained.toEOPretty)
        assertResult(expected)(obtained)

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
