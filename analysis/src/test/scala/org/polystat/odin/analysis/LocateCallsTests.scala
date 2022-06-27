package org.polystat.odin.analysis

import cats.data.NonEmptyVector
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.utils.Optics
import org.polystat.odin.analysis.utils.inlining.LocateCalls
import org.polystat.odin.analysis.utils.inlining.LocatorContext
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams._
import org.polystat.odin.parser.eo.Parser
import org.scalatest.wordspec.AnyWordSpec

class LocateCallsTests extends AnyWordSpec {

  private case class TestCase(
    label: String,
    methodBody: EOBndExpr[EOExprOnly],
    expected: Set[String]
  )

  private def runTestCase(test: TestCase): Unit = {
    test match {
      case TestCase(label, methodBody, expected) =>
        registerTest(label) {
          val parsed = LocateCalls.parseMethod(methodBody, 0)
          parsed match {
            case None => fail("LocateCalls.parseMethods returned None!")
            case Some(parsed) =>
              assert(parsed.calls.map(_.methodName).toSet == expected)
          }
        }
    }
  }

  def simpleMethod(
    name: String,
    calledMethods: NonEmptyVector[String],
    selfArg: String,
  ): EOBndExpr[EOExprOnly] = EOBndExpr(
    bndName = EOAnyNameBnd(LazyName(name)),
    Fix(
      EOObj(
        freeAttrs = Vector(LazyName(selfArg)),
        varargAttr = None,
        bndAttrs = Vector(
          EOBndExpr(
            EODecoration,
            Fix(
              EOCopy(
                Fix[EOExpr](EOSimpleAppWithLocator("seq", 1)),
                calledMethods.map(name =>
                  EOAnonExpr(
                    Fix(
                      EOCopy(
                        Fix(
                          EODot(
                            Fix[EOExpr](EOSimpleAppWithLocator(selfArg, 0)),
                            name,
                          )
                        ),
                        NonEmptyVector.one(
                          EOAnonExpr(
                            Fix[EOExpr](EOSimpleAppWithLocator(selfArg, 0))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  private val testCases: List[TestCase] = List(
    "this",
    "self",
  ).map(selfArg =>
    TestCase(
      label = s"method with $selfArg",
      methodBody = simpleMethod(
        name = "f",
        calledMethods = NonEmptyVector.one("g"),
        selfArg = selfArg
      ),
      expected = Set("g")
    )
  ).appendedAll(
    List()
  )

  "LocateCalls.inlineThisObject" should {
    "find and replace this object correctly" in {
      val method = Optics
        .prisms
        .fixToEOObj
        .getOption(
          Parser
            .parse("""|[this x] > n
                      |  seq > @
                      |    s1237550792
                      |  [] > s1237550792
                      |    b517052730 > @
                      |  [] > b517052730
                      |    f_a1506809545.add > @
                      |      s_r1019384604
                      |  [] > f_a1506809545
                      |    t550668305.state > @
                      |  [] > t550668305
                      |    this > @
                      |  [] > s_r1019384604
                      |""".stripMargin)
            .toOption
            .flatMap(p => LocatorContext.setLocators(p).toOption)
            .get
            .bnds
            .head
            .expr
        )
        .get

      val obtained = LocateCalls.inlineThisObject("this")(method).toEOPretty
      println(obtained)
      val expected = """|[this x]
                        |  ^.seq > @
                        |    $.s1237550792
                        |  [] > s1237550792
                        |    ^.b517052730 > @
                        |  [] > b517052730
                        |    ^.f_a1506809545.add > @
                        |      ^.s_r1019384604
                        |  [] > f_a1506809545
                        |    ^.this.state > @
                        |  [] > t550668305
                        |    ^.this > @
                        |  [] > s_r1019384604""".stripMargin
      assert(obtained == expected)
    }
  }

  testCases.foreach(runTestCase)

}
