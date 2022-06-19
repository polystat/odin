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
      case DefectsDetected(_, message) => IO.pure(message.distinct.toList)
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
          |    x.add 1 > @
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self x).less 10)
          |      22
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x > @
          |""".stripMargin,
      expected = List(errorMessage("g"))
    ),
    TestCase(
      label =
        "Another not referentially transparent method 2",
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
      label =
        "Bad method that uses nested variables",
      code =
        """[] > base
          |  [self x] > g
          |    [] > local
          |      10 > x
          |      [] > very_local
          |        1 > y
          |    x.div local.very_local.y > sas
          |    seq > @
          |      assert ((self.f self (sas.add 1)).less local.x)
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
      label =
        "J2EO 'prim__int' and .write",
      code =
        """[] > base
          |  [self x] > g
          |    prim__int.constructor_1 > t
          |      prim__int.new
          |    prim__int.constructor_2 > temp
          |      prim__int.new
          |      x
          |    seq > @
          |      t.write temp
          |      assert ((self.f self t).less 10)
          |      t
          |  [self x] > f
          |    x.add 1 > @
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x.sub 1 > @
          |
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
      label =
        "Test from the fragile base class paper but with functions without arguments and keywords ",
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
    TestCase(
      label = "J2EO example with mutual recursion",
      code =
        """
          |# 2022-05-25T15:02:29.112794500
          |# j2eo team
          |+alias stdlib.lang.class__Object
          |+alias stdlib.primitives.prim__int
          |+alias org.eolang.gray.cage
          |
          |[] > class__Parent
          |  class__Object > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Object.new > super
          |      super > @
          |      "class__Parent" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [this x] > f
          |        seq > @
          |          d50720817
          |          s1135935001
          |          s1649847375
          |        prim__int.constructor_1 > t
          |          prim__int.new
          |        [] > d50720817
          |          t.write > @
          |            i_s1496220730
          |        [] > i_s1496220730
          |          b488600086 > @
          |        [] > b488600086
          |          s_r1111379131.sub > @
          |            l1846982837
          |        [] > s_r1111379131
          |          x > @
          |        [] > l1846982837
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            5
          |        [] > s1135935001
          |          p635288507.if > @
          |            TRUE
          |            []
          |              "AssertionError" > msg
          |        [] > p635288507
          |          b355885103 > @
          |        [] > b355885103
          |          s_r1321115948.greater > @
          |            l706665172
          |        [] > s_r1321115948
          |          t > @
          |        [] > l706665172
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            0
          |        [] > s1649847375
          |          s_r1153933106 > @
          |        [] > s_r1153933106
          |          x > @
          |      # g :: int -> int
          |      [this y] > g
          |        seq > @
          |          s2144067911
          |        [] > s2144067911
          |          m_i593447952 > @
          |        [] > m_i593447952
          |          this.f > @
          |            this
          |            s_r1950136544
          |        [] > s_r1950136544
          |          y > @
          |      # h :: int -> int
          |      [this z] > h
          |        seq > @
          |          s209360730
          |        [] > s209360730
          |          s_r740007499 > @
          |        [] > s_r740007499
          |          z > @
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1971152916
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1971152916
          |      super.constructor > @
          |        this.super
          |
          |[] > class__Child
          |  class__Parent > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Parent.new > super
          |      super > @
          |      "class__Child" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [this y] > f
          |        seq > @
          |          s1687627235
          |        [] > s1687627235
          |          s_r1007660652 > @
          |        [] > s_r1007660652
          |          y > @
          |      # h :: int -> int
          |      [this z] > h
          |        seq > @
          |          s1276544608
          |        [] > s1276544608
          |          m_i1387620926 > @
          |        [] > m_i1387620926
          |          this.g > @
          |            this
          |            s_r265348534
          |        [] > s_r265348534
          |          z > @
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1324173038
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1324173038
          |      super.constructor > @
          |        this.super
          |""".stripMargin,
      expected = List(
        errorMessage("g"),
      )
    )
    ,
    TestCase(
      label = "J2EO example with mutual recursion without .write",
      code =
        """
          |# 2022-05-25T15:02:29.112794500
          |# j2eo team
          |+alias stdlib.lang.class__Object
          |+alias stdlib.primitives.prim__int
          |+alias org.eolang.gray.cage
          |
          |[] > class__Parent
          |  class__Object > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Object.new > super
          |      super > @
          |      "class__Parent" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [this x] > f
          |        seq > @
          |          d50720817
          |          s1135935001
          |          s1649847375
          |        [] > t
          |          s_r1111379131.sub > @
          |            l1846982837
          |        [] > s_r1111379131
          |          x > @
          |        [] > l1846982837
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            5
          |        [] > s1135935001
          |          p635288507.if > @
          |            TRUE
          |            []
          |              "AssertionError" > msg
          |        [] > p635288507
          |          b355885103 > @
          |        [] > b355885103
          |          s_r1321115948.greater > @
          |            l706665172
          |        [] > s_r1321115948
          |          t > @
          |        [] > l706665172
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            0
          |        [] > s1649847375
          |          s_r1153933106 > @
          |        [] > s_r1153933106
          |          x > @
          |      # g :: int -> int
          |      [this y] > g
          |        seq > @
          |          s2144067911
          |        [] > s2144067911
          |          m_i593447952 > @
          |        [] > m_i593447952
          |          this.f > @
          |            this
          |            s_r1950136544
          |        [] > s_r1950136544
          |          y > @
          |      # h :: int -> int
          |      [this z] > h
          |        seq > @
          |          s209360730
          |        [] > s209360730
          |          s_r740007499 > @
          |        [] > s_r740007499
          |          z > @
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1971152916
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1971152916
          |      super.constructor > @
          |        this.super
          |
          |[] > class__Child
          |  class__Parent > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Parent.new > super
          |      super > @
          |      "class__Child" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |      # f :: int -> int
          |      [this y] > f
          |        seq > @
          |          s1687627235
          |        [] > s1687627235
          |          s_r1007660652 > @
          |        [] > s_r1007660652
          |          y > @
          |      # h :: int -> int
          |      [this z] > h
          |        seq > @
          |          s1276544608
          |        [] > s1276544608
          |          m_i1387620926 > @
          |        [] > m_i1387620926
          |          this.g > @
          |            this
          |            s_r265348534
          |        [] > s_r265348534
          |          z > @
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1324173038
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1324173038
          |      super.constructor > @
          |        this.super
          |""".stripMargin,
      expected = List(
        errorMessage("g"),
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
    TestCase(
      label = "J2EO dataization",
      code =
        """
          |[] > base
          |  [self x] > f
          |    [] > t
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        1
          |    x.add (t.sub 1) > @
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self x).less 8018)
          |      22
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "J2EO 'prim__int'",
      code =
        """
          |[] > base
          |  [self x] > f
          |    prim__int.constructor_2 > t
          |      prim__int.new
          |      1
          |    x.add (t.sub 1) > @
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self x).less 8018)
          |      22
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "J2EO 'prim__float'",
      code =
        """
          |[] > base
          |  [self x] > f
          |    prim__float.constructor_2 > t
          |      prim__float.new
          |      1
          |    x.add (t.sub 1) > @
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self x).less 8018)
          |      22
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x > @
          |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "J2EO 'prim__int.constructor_3' ",
      code =
        """
          |[] > base
          |  [self x] > f
          |    prim__int.constructor_2 > dummy_int
          |      prim__int.new
          |      1
          |    prim__int.constructor_3 > t
          |      prim__int.new
          |      dummy_int
          |    x.add (t.sub 1) > @
          |  [self x] > g
          |    seq > @
          |      assert ((self.f self x).less 8018)
          |      22
          |
          |[] > derived
          |  base > @
          |  [self x] > f
          |    x > @
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
