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

  def errorMessage(name: String): String =
    s"Inlining calls in method $name is not safe: doing so may break the behaviour of subclasses!"

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
      expected = List(errorMessage("g"))
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
      expected = List(errorMessage("g"))
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
      expected = List(errorMessage("g"))
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
      expected = List(errorMessage("g"))
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
      expected = List(errorMessage("m"))
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
          |    [self y] > g
          |      self.f self y > @
          |    [self y] > g2
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
      expected = List(
        errorMessage("g"),
        errorMessage("g2"),
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
          |    [self y2] > gg
          |      self.g self y2 > @
          |
          |    [self y3] > ggg
          |      self.gg self y3 > @
          |
          |    [self z] > h
          |      z > @
          |  [] > child
          |    test.parent > @
          |    [self y] > f
          |      y > @
          |    [self z] > h
          |      self.ggg self z > @
          |""".stripMargin,
      expected = List(
        errorMessage("g"),
        errorMessage("ggg"),
      )
    ),
    TestCase(
      label = "Test from the fragile base class paper",
      code =
        """|[] > c
           |  [self v] > l
           |    assert (v.less 5) > @
           |  [self v] > m
           |    self.l self v > @
           |  [self v] > n
           |    v > @
           |
           |[] > m
           |  c > @
           |  [self v] > l
           |    v > @
           |  [self v] > n
           |    self.m self v > @
           |""".stripMargin,
      expected = List(errorMessage("m")),
    ),
    TestCase(
      label = "Test from the fragile base class paper but with functions without arguments",
      code =
        """|[] > c
           |  [self] > method
           |    2 > @
           |  [self v] > l
           |    assert (v.less 5) > @
           |  [self v] > m
           |    self.l self v > @
           |  [self v] > n
           |    seq > @
           |      self.method self
           |      v
           |
           |[] > m
           |  c > @
           |  [self v] > l
           |    v > @
           |  [self v] > n
           |    self.m self v > @
           |""".stripMargin,
      expected = List(errorMessage("m")),
    ),
    TestCase(
      label =
        "Test from the fragile base class paper but with functions without arguments",
      code =
        """|[] > c
           |  [self] > method
           |    2 > @
           |  [self v] > l
           |    assert (v.less 5) > @
           |  [self v] > m
           |    self.l self v > @
           |  [self v] > n
           |    seq > @
           |      self.method self
           |      v
           |
           |[] > m
           |  c > @
           |  [self v] > l
           |    v > @
           |  [self v] > n
           |    self.m self v > @
           |""".stripMargin,
      expected = List(errorMessage("m")),
    ),
    TestCase(
      label =
        "Unjustified assumption in two participants of the inheritance chain",
      code =
        """
          |[] > base
          |  [self v] > n
          |    seq > @
          |      assert (v.less 10)
          |      2
          |  [self v] > m
          |    self.n self v > @
          |
          |[] > osnova
          |  base > @
          |  [self x] > k
          |    self.n self x > @
          |
          |[] > derived
          |  osnova > @
          |  [self v] > n
          |    33 > @
          |""".stripMargin,
      expected = List(
        errorMessage("m"),
        errorMessage("k")
      ),
    ),
  )

  val testCasesWithoutErrors: List[TestCase] = List(
    TestCase(
      label = "All methods are referentially transparent",
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
    ),
    TestCase(
      label = "No precondition strengthening with a function without arguments",
      code =
        """
          |[] > base
          |  [self] > n
          |    seq > @
          |      assert (2.less 10)
          |      2
          |  [self v] > m
          |    self.n self > @
          |
          |[] > osnova
          |  base > @
          |  [self x] > k
          |    self.n self > @
          |
          |[] > derived
          |  osnova > @
          |  [self] > n
          |    33 > @
          |""".stripMargin,
      expected = List()
    ),
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
