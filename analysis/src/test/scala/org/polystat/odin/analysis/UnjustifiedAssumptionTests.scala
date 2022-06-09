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
      label = "Test from the fragile base class paper but with functions without arguments and keywords ",
      code =
        """|[] > c
           |  [self] > method
           |    memory > local_m
           |    cage > local_m2
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
    TestCase(label = "J2EO example with mutual recursion",
      code =
        """
          |[] > prim__int
          |  [] > new
          |    []> @
          |  [self x] > constructor_1
          |    [] > @
          |  [self x] > constructor_2
          |    [] > @
          |
          |[] > class__Parent
          |  [] > new
          |    [] > self
          |      "class__Parent" > className
          |      [self] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [self x] > f
          |        i_s1147580192 > t
          |        [] > i_s1147580192
          |          b1337335626 > @
          |        [] > b1337335626
          |          s_r78204644.div > @
          |            l1287934450
          |        [] > s_r78204644
          |          x > @
          |        [] > l1287934450
          |          5 > @
          |        [] > s712609105
          |          s_r1364913072 > @
          |        [] > s_r1364913072
          |          x > @
          |        seq > @
          |          t
          |          i_s1147580192
          |      # g :: int -> int
          |      [self y] > g
          |        [] > s758348212
          |          m_i817978763 > @
          |        [] > m_i817978763
          |          self.f > @
          |            self
          |            s_r1798219673
          |        [] > s_r1798219673
          |          y > @
          |        seq > @
          |          s758348212
          |      # h :: int -> int
          |      [self z] > h
          |        [] > s1092572064
          |          s_r728885526 > @
          |        [] > s_r728885526
          |          z > @
          |        seq > @
          |          s1092572064
          |
          |    seq > @
          |      self
          |
          |[] > class__Child
          |  class__Parent > super
          |  class__Parent > @
          |  [] > new
          |    [] > self
          |      class__Parent.new.self > super
          |      class__Parent.new.self > @
          |      "class__Child" > className
          |      [self] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [self y] > f
          |        seq > @
          |          s1647809929
          |        [] > s1647809929
          |          s_r1258084361 > @
          |        [] > s_r1258084361
          |          y > @
          |      # h :: int -> int
          |      [self z] > h
          |        seq > @
          |          s391914049
          |        [] > s391914049
          |          m_i96406857 > @
          |        [] > m_i96406857
          |          self.g > @
          |            self
          |            s_r1534745514
          |        [] > s_r1534745514
          |          z > @
          |    seq > @
          |      self
          |""".stripMargin,
      expected = List("THere should be mutaul recurtsion somewhere here")
    )
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
