package org.polystat.odin.analysis

import cats.effect._
import org.scalatest.wordspec.AnyWordSpec
import EOOdinAnalyzer.unjustifiedAssumptionAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import cats.effect.unsafe.implicits.global

class UnjustifiedAssumptionTests extends AnyWordSpec {

  case class TestCase(label: String, code: String, expected: List[String])

  def analyze(code: String): IO[List[String]] =
    EOOdinAnalyzer
      .analyzeSourceCode[String, IO](unjustifiedAssumptionAnalyzer[IO])(code)(sourceCodeEoParser())
      .compile
      .toList
      .map(_.map(_.value))

  val testCasesWithErrors = List(
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
      label = "One not referentially transparent method 2 with shuffled method order",
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
      label = "One not referentially transparent method 3",
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
      label = "One not referentially transparent method in presence of mutual recursion",
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
      expected = List("Method g is not referentially transparent", "Method g2 is not referentially transparent")
    ),

  )

  val testCasesWithoutErrors = List(
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
          |      self.m self v > @
          |""".stripMargin,
      expected = List()
    )
  )

  def runTests(tests: List[TestCase]) : Unit =
    tests.foreach {
      case TestCase(label, code, expected) =>
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

//    "fail" should {
//      assert(true)
//    }
  }

}
