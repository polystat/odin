package org.polystat.odin.analysis.inlining

import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec
import SetLocatorsTestCases._
import InlineCallsTestCases._
import cats.syntax.either._
import cats.data.{EitherNel, NonEmptyList => Nel}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.gens.ast.eoProg
import org.scalacheck.{Prop, Test}
import org.scalatestplus.scalacheck.Checkers

class InliningTests extends AnyWordSpec with Checkers {

  val scalacheckParams: Test.Parameters =
    Test
      .Parameters
      .default
      .withMaxSize(10000)
      .withMinSuccessfulTests(10000)
      .withWorkers(4)

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

    "transform ast so that no EOSimpleApps remain" in {
      def findAllEOSimpleApp(
        bnd: EOBnd[EOExprOnly]
      ): Vector[EOSimpleApp[EOExprOnly]] = {
        Fix.un(bnd.expr) match {
          case app: EOSimpleApp[EOExprOnly] => Vector(app)
          case EOCopy(trg, args) =>
            findAllEOSimpleApp(EOAnonExpr(trg)) ++
              args.flatMap(findAllEOSimpleApp)
          case EODot(trg, _) => findAllEOSimpleApp(EOAnonExpr(trg))
          case EOObj(_, _, bndAttrs) => bndAttrs.flatMap(findAllEOSimpleApp)
          case EOArray(elems) => elems.flatMap(findAllEOSimpleApp)
          case _ => Vector()
        }
      }

      def containsNoEOSimpleApp(prog: EOProg[EOExprOnly]): Boolean =
        prog.bnds.flatMap(findAllEOSimpleApp).isEmpty

      val prop = Prop.forAll(eoProg(4)) { prog =>
        Context.setLocators(prog).map(containsNoEOSimpleApp)
          // setLocators may fail for arbitrary ASTs
          // this property is only concerned with successful
          // terminations of setLocators
          .getOrElse(true)
      }
      check(prop, scalacheckParams)
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
