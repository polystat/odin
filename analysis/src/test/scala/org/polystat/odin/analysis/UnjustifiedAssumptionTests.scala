package org.polystat.odin.analysis

import cats.effect._
import cats.effect.unsafe.implicits.global
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult._
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.scalatest.wordspec.AnyWordSpec

import EOOdinAnalyzer.unjustifiedAssumptionAnalyzer

class UnjustifiedAssumptionTests extends AnyWordSpec {

  case class TestCase(label: String, code: String, expected: List[String])

  def analyze(code: String): IO[List[String]] = EOOdinAnalyzer
    .analyzeSourceCode[String, IO](unjustifiedAssumptionAnalyzer)(code)(
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
      label = "One not referentially transparent method",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x.sub 5 > y1
          |      seq > @
          |        assert (0.less y1)
          |        x
          |    [self y] > g
          |      self.f self y > @
          |    [self z] > h
          |      z > @
          |  [] > child
          |    parent > @
          |    [self y] > f
          |      y > @
          |    [self z] > h
          |      self.g self z > @
          |""".stripMargin,
      expected = List("Method g is not referentially transparent")
    ),
    TestCase(
      label = "One not referentially transparent method 2",
      code =
        """
          |[] > base
          |  [self x] > f
          |    seq > @
          |      assert (x.less 9)
          |      x.add 1
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self (x.add 1)).less 10)
          |      22
          |[] > derived
          |  base > @
          |  [self x] > f
          |    seq > @
          |      assert (5.less x)
          |      x.sub 1
          |""".stripMargin,
      expected = List("Method g is not referentially transparent")
    ),
    TestCase(
      label =
        "One not referentially transparent method 2 with shuffled method order",
      code =
        """[] > base
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self (x.add 1)).less 10)
          |      22
          |  [self x] > f
          |    seq > @
          |      assert (x.less 9)
          |      x.add 1
          |[] > derived
          |  base > @
          |  [self x] > f
          |    seq > @
          |      assert (5.less x)
          |      x.sub 1
          |""".stripMargin,
      expected = List("Method g is not referentially transparent")
    ),
    TestCase(
      label = "One not referentially transparent method 4",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x.sub 5 > t
          |      seq > @
          |        assert (0.less t)
          |        x
          |    [self z] > h
          |      z > @
          |
          |  [] > child
          |    test.parent > @
          |    [self y] > g
          |      self.f self y > @
          |
          |  [] > grandchild
          |    test.child > @
          |    [self y] > f
          |      y > @
          |    [self z] > h
          |      self.g self z > @
          |""".stripMargin,
      expected = List("Method g is not referentially transparent")
    ),
    TestCase(
      label =
        "One not referentially transparent method in presence of mutual recursion",
      code =
        """
          |[] > test
          |  [] > base
          |    [self v] > n
          |      seq > @
          |        assert (v.less 10)
          |        2
          |    [self v] > m
          |      self.n self v > @
          |  [] > derived
          |    base > @
          |    [self v] > n
          |      self.m self v > @
          |""".stripMargin,
      expected = List("Method m is not referentially transparent")
    ),
    TestCase(
      label = "Two not referentially transparent method",
      code =
        """
          |[] > test
          |  [] > parent
          |    [self x] > f
          |      x.sub 5 > y1
          |      seq > @
          |        assert (0.less y1)
          |        x
          |    [this y] > g
          |      this.f this y > @
          |    [hahaha_this_is_self y] > g2
          |      hahaha_this_is_self.f hahaha_this_is_self y > @
          |    [self z] > h
          |      z > @
          |  [] > child
          |    parent > @
          |    [self y] > f
          |      y > @
          |    [self z] > h
          |      self.g self z > @
          |""".stripMargin,
      expected = List(
        "Method g is not referentially transparent",
        "Method g2 is not referentially transparent"
      )
    ),
    TestCase(
      label = "Two not referentially transparent methods 2",
      code =
        """
          |[] > test
          |  [] > parent
          |
          |    [self y1] > g
          |      self.f self y1 > @
          |
          |    [self x] > f
          |      x.sub 5 > t
          |      seq > @
          |        assert (0.less t)
          |        x
          |
          |    [this y2] > gg
          |      this.g this y2 > @
          |
          |    [this y3] > ggg
          |      this.gg this y3 > @
          |
          |    [self z] > h
          |      z > @
          |  [] > child
          |    test.parent > @
          |    [self y] > f
          |      y > @
          |    [slf z] > h
          |      slf.ggg slf z > @
          |""".stripMargin,
      expected = List(
        "Method g is not referentially transparent",
        "Method ggg is not referentially transparent"
      )
    ),
  )

  val testCasesWithoutErrors: List[TestCase] = List(
    TestCase(
      label = "All methods are referentially transparent",
      code =
        """[] > test
          |  [] > base
          |    [this v] > n
          |      2 > @
          |    [this v] > m
          |      this.n this v > @
          |  [] > derived
          |    base > @
          |    [self v] > n
          |      33 > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "All methods are referentially transparent with mutual recursion",
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
          |      self.m self v > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "All methods are referentially transparent with recursion",
      code =
        """[] > test
          |  [] > base
          |    [self v] > n
          |      2 > @
          |    [self v] > m
          |      self.m self v > @
          |  [] > derived
          |    base > @
          |    [self v] > n
          |      self.m self v > @
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
