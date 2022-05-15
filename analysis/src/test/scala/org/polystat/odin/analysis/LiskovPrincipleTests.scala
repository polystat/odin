package org.polystat.odin.analysis

import cats.effect._
import org.scalatest.wordspec.AnyWordSpec
import EOOdinAnalyzer.liskovPrincipleViolationAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import cats.effect.unsafe.implicits.global
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult._

class LiskovPrincipleTests extends AnyWordSpec {

  case class TestCase(label: String, code: String, expected: List[String])

  def analyze(code: String): IO[List[String]] = EOOdinAnalyzer
    .analyzeSourceCode[String, IO](liskovPrincipleViolationAnalyzer)(code)(
      cats.Monad[IO],
      sourceCodeEoParser()
    )
    .flatMap {
      case Ok(_) => IO.pure(List.empty)
      case DefectsDetected(_, message) => IO.pure(message.toList)
      case AnalyzerFailure(_, e) => IO.raiseError(e)
    }

  val testCasesWithErrors: List[TestCase] = List(
    TestCase(
      label = "Nested bad method",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self y] > g
          |      y > @
          |    [self z] > h
          |      z > @
          |  [] > child
          |    parent > @
          |    [self x] > g
          |      x.sub 5 > y1
          |      seq > @
          |        assert (0.less y1)
          |        y1
          |
          |""".stripMargin,
      expected = List(
        "Method g of object child violates the Liskov substitution principle as compared to version in parent object parent"
      )
    ),
    TestCase(
      label = "Bad method",
      code =
        """
          |[] > base
          |  [self x] > f
          |    seq > @
          |      assert (x.less 9)
          |      x.add 1
          |[] > derived
          |  base > @
          |  [self x] > f
          |    seq > @
          |      assert (x.greater 9)
          |      x.sub 1
          |""".stripMargin,
      expected = List(
        "Method f of object derived violates the Liskov substitution principle as compared to version in parent object base"
      )
    ),
    TestCase(
      label = "Bad nested method with division",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x > @
          |  [] > child
          |    test.parent > @
          |    [self y] > f
          |      10.div y > @
          |""".stripMargin,
      expected = List(
        "Method f of object child violates the Liskov substitution principle as compared to version in parent object parent"
      )
    ),
    TestCase(
      label =
        "Bad method and method indirectly affected by change in another method",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x > @
          |    [self x] > g
          |      self.f self x > @
          |
          |  [] > child
          |    test.parent > @
          |    [self y] > f
          |      10.div y > @
          |""".stripMargin,
      expected = List(
        "Method f of object child violates the Liskov substitution principle as compared to version in parent object parent",
        "Method g of object child violates the Liskov substitution principle as compared to version in parent object parent"
      )
    ),
    TestCase(
      label =
        "Method deeper in the hierarchy affecting method of the basest class",
      code =
        """
          |[] > test
          |  [] > grandparent
          |    [self x] > a
          |      10 > @
          |    [self x] > b
          |      self.a self x > @
          |
          |  [] > parent
          |    test.grandparent > @
          |    [self x] > f
          |      x > @
          |    [self x] > g
          |      self.f self x > @
          |
          |  [] > child
          |    test.parent > @
          |    [self x] > a
          |      seq > @
          |        assert (x.less 100)
          |        x
          |""".stripMargin,
      expected = List(
        "Method a of object child violates the Liskov substitution principle as compared to version in parent object grandparent",
        "Method a of object child violates the Liskov substitution principle as compared to version in parent object parent",
        "Method b of object child violates the Liskov substitution principle as compared to version in parent object grandparent",
        "Method b of object child violates the Liskov substitution principle as compared to version in parent object parent"
      )
    ),
    TestCase(
      label = "Two bad nested methods",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x.sub 5 > y1
          |      seq > @
          |        assert ((y1.add 5).greater 0)
          |        x
          |    [self z] > h
          |      z > @
          |  [] > child
          |    parent > @
          |    [self y] > f
          |      seq > @
          |        assert (y.less 0)
          |        y
          |    [self z] > h
          |      10.div z > @
          |""".stripMargin,
      expected = List(
        "Method f of object child violates the Liskov substitution principle as compared to version in parent object parent",
        "Method h of object child violates the Liskov substitution principle as compared to version in parent object parent"
      )
    ),
  )

  val testCasesWithoutErrors: List[TestCase] = List(
    TestCase(
      label = "No violation",
      code =
        """[] > test
          |  [] > base
          |    [self v] > n
          |      2 > @
          |    [self v] > m
          |      self.n self v > @
          |  [] > derived
          |    base > @
          |    [self v] > n
          |      33 > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "Method with no arguments",
      code =
        """[] > test
          |  [] > base
          |    [self] > n
          |      2 > @
          |  [] > derived
          |    base > @
          |    [self] > n
          |      33 > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "No children :-(",
      code =
        """
          |[] > base
          |  [self v] > n
          |    2 > @
          |  [self v] > m
          |    self.n self v > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "Redefinition expands the input domain",
      code =
        """
          |[] > base
          |  [self x] > f
          |    seq > @
          |      assert (x.less 9)
          |      x.add 1
          |[] > derived
          |  base > @
          |  [self x] > f
          |    seq > @
          |      assert (x.less 20)
          |      x.sub 1
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "No violation, neutral result",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x.sub 5 > y1
          |      seq > @
          |        assert ((y1.add 5).greater 0)
          |        x
          |  [] > child
          |    parent > @
          |    [self y] > f
          |      y > @
          |""".stripMargin,
      expected = List()
    )
  )

  def runTests(tests: List[TestCase]): Unit =
    tests.foreach { case TestCase(label, code, expected) =>
      registerTest(label) {
        val obtained = analyze(code).unsafeRunSync()
        assert(obtained == expected)
      }

    }

  "analyzer" should {
    "find errors" should {
      runTests(testCasesWithErrors)
    }

    "not find errors" should {
      runTests(testCasesWithoutErrors)
    }
  }

}
