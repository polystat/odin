package org.polystat.odin.analysis

import cats.effect._
import cats.effect.unsafe.implicits.global
import org.polystat.odin.analysis.EOOdinAnalyzer.directStateAccessAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.scalatest.wordspec.AnyWordSpec

import EOOdinAnalyzer.OdinAnalysisResult._

class DetectStateAccessTests extends AnyWordSpec {
  case class TestCase[A](label: String, code: String, expected: A)

  def analyze(code: String): IO[List[String]] = EOOdinAnalyzer
    .analyzeSourceCode[String, IO](directStateAccessAnalyzer)(code)(
      cats.Monad[IO],
      sourceCodeEoParser()
    )
    .flatMap {
      case Ok(_) => IO.pure(List.empty)
      case DefectsDetected(_, errors) => IO.pure(errors.toList)
      case AnalyzerFailure(_, e) => IO.raiseError(e)
    }

  val testsWithDefect: List[TestCase[List[String]]] = List(
    TestCase(
      label = "Write to state",
      code = """[] > a
               |  memory > state
               |  [self new_state] > update_state
               |    self.state.write new_state > @
               |[] > b
               |  a > @
               |  [self new_state] > change_state_plus_two
               |    self.state.write (new_state.add 2) > @
               |""".stripMargin,
      expected = List(
        "Method 'change_state_plus_two' of object 'b' directly accesses state 'state' of base class 'a'"
      )
    ),
    TestCase(
      label = "Access to state",
      code = """[] > base
               |  memory > state
               |[] > b
               |  base > @
               |  [self var] > alter_var
               |    var.write 10 > @
               |  [self] > change_state
               |    self.alter_var self self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'change_state' of object 'b' directly accesses state 'state' of base class 'base'"
      )
    ),
    TestCase(
      label = "calculation chain",
      code = """[] > test
               |  [] > a
               |    memory > state
               |  [] > b
               |    a > @
               |    [self x] > n
               |      add. > @
               |        x
               |        mul.
               |          100
               |          add.
               |            100
               |            sub.
               |              100
               |              self.state
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'test.a'"
      )
    ),
    TestCase(
      label = "read-in-calculation-chain",
      code = """[] > test
               |  [] > a
               |    memory > state
               |  [] > b
               |    a > @
               |    [self x] > n
               |      add. > @
               |        self.state
               |        mul.
               |          100
               |          add.
               |            100
               |            sub.
               |              100
               |              x
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'test.a'"
      )
    ),
    TestCase(
      label = "read-in-inheritance-chain",
      code = """[] > test
               |  [] > a
               |    memory > state
               |  [] > b
               |    a > @
               |  [] > c
               |    b > @
               |    [self x] > n
               |      self.state.add x > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.c' directly accesses state 'state' of base class 'test.a'"
      )
    ),
    TestCase(
      label = "access-read-nested-class-2",
      code = """[] > test
               |  [] > very_outer
               |    [] > outer
               |      [] > a
               |        memory > state
               |      [] > b
               |        a > @
               |        [self x] > n
               |          self.state.add x > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.very_outer.outer.b' directly accesses state 'state' of base class 'test.very_outer.outer.a'"
      )
    ),
    TestCase(
      label = "write-through-another-method",
      code = """[] > test
               |  [] > a
               |    memory > state
               |  [] > b
               |    a > @
               |    [self x y] > m
               |      x.write y > @
               |    [self y] > n
               |      self.m self (self.state) y > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'test.a'"
      )
    ),
    TestCase(
      label = "Access to cage",
      code = """[] > a
               |  cage > state
               |[] > second_obj
               |  a > @
               |  [self] > func
               |    self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'func' of object 'second_obj' directly accesses state 'state' of base class 'a'"
      )
    ),
    TestCase(
      label = "Access to cage AND memory",
      code = """[] > a
               |  cage > state
               |  memory > mem
               |[] > second_obj
               |  a > @
               |  [self] > func
               |    self.state > tmp
               |    self.mem > @
               |""".stripMargin,
      expected = List(
        "Method 'func' of object 'second_obj' directly accesses state 'state' of base class 'a'",
        "Method 'func' of object 'second_obj' directly accesses state 'mem' of base class 'a'"
      )
    ),
    TestCase(
      label = "Access to inner state",
      code = """[] > base
               |  memory > plain_state
               |  [] > inner_state
               |    [] > very_inner_state
               |      memory > hidden_state
               |    memory > inner_mem
               |    cage > inner_cage
               |[] > b
               |  base > @
               |  [self] > func
               |    self.inner_state.very_inner_state.hidden_state > super_tmp
               |    self.inner_state.inner_cage > tmp
               |    seq > @
               |      self.plain_state.write 10
               |      self.inner_state.inner_mem
               |""".stripMargin,
      expected = List(
        "Method 'func' of object 'b' directly accesses state 'inner_state.very_inner_state.hidden_state' of base class 'base'",
        "Method 'func' of object 'b' directly accesses state 'inner_state.inner_cage' of base class 'base'",
        "Method 'func' of object 'b' directly accesses state 'plain_state' of base class 'base'",
        "Method 'func' of object 'b' directly accesses state 'inner_state.inner_mem' of base class 'base'"
      )
    ),
    TestCase(
      label = "Access to state in inner hierarchy",
      code = """
               |[] > superroot
               |  [] > root
               |    [] > parent
               |      memory > state
               |
               |    [] > child
               |      parent > @
               |      [self] > method
               |        self.state.write 10 > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'superroot.root.child' directly accesses state 'state' of base class 'superroot.root.parent'",
      )
    ),
    TestCase(
      label = "Access to state that is high in the hierarchy",
      code = """
               |[] > super_puper
               |  memory > omega_state
               |
               |[] > super
               |  super_puper > @
               |  memory > super_state
               |  [self] > super_bad_func
               |    seq > @
               |      self.omega_state.write 10
               |      self.super_state.write 30
               |
               |[] > parent
               |  super > @
               |  memory > parent_state
               |
               |[] > child
               |  parent > @
               |  memory > local_state
               |  [self] > bad_func
               |    seq > @
               |      self.omega_state.write 10
               |      self.super_state.write 10
               |      self.parent_state.write 10
               |      self.local_state.write 10
               |""".stripMargin,
      expected = List(
        "Method 'super_bad_func' of object 'super' directly accesses state 'omega_state' of base class 'super_puper'",
        "Method 'bad_func' of object 'child' directly accesses state 'omega_state' of base class 'super_puper'",
        "Method 'bad_func' of object 'child' directly accesses state 'super_state' of base class 'super'",
        "Method 'bad_func' of object 'child' directly accesses state 'parent_state' of base class 'parent'",
      )
    ),
    TestCase(
      label =
        "Access to state that is high in the hierarchy | shadowing, attaching attributes during application",
      code = """
               |[] > super_puper
               |  memory > omega_state
               |  memory > additional_state
               |
               |[] > super
               |  super_puper > @
               |  super_puper.additional_state > omega_state
               |  [self] > super_bad_func
               |    seq > @
               |      self.omega_state.write 10
               |
               |[] > parent
               |  ((super)) > @
               |  memory > parent_state
               |
               |[] > child
               |  parent > @
               |  memory > local_state
               |  [self] > bad_func
               |    seq > @
               |      self.omega_state.write 10
               |      self.parent_state.write 10
               |      self.local_state.write 10
               |""".stripMargin,
      expected = List(
        "Method 'super_bad_func' of object 'super' directly accesses state 'omega_state' of base class 'super_puper'",
        "Method 'bad_func' of object 'child' directly accesses state 'omega_state' of base class 'super_puper'",
        "Method 'bad_func' of object 'child' directly accesses state 'parent_state' of base class 'parent'"
      )
    ),
    TestCase(
      label = "Access to state with further method call",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [this] > method
               |    this.state.add 10 > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to state with further method call | indirect",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [this] > method
               |    (this.state.add 10).write 4 > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to state with a funky method call",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [slf] > method
               |    3.sub ((slf.state.add 10).add 10) > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "State access with a weird decoration",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  seq > @
               |    parent
               |  [self] > method
               |    self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to state by simply returning it",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [self] > method
               |    self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to state in simple weird local definitions",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [self] > method
               |    seq > @
               |      s689401025
               |    [] > s689401025
               |      self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to state in complex weird local definitions",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > @
               |  [self] > method
               |    seq > @
               |      s689401025
               |    [] > s689401025
               |      b306980751 > @
               |    [] > b306980751
               |      s_r1826699684.add > @
               |        s_r1769193365
               |    [] > s_r1826699684
               |      self.state > @
               |    [] > s_r1769193365
               |      5 > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label =
        "State access when decorated object is an attribute of the same object, but with transitivity",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > super
               |  super > superpuper
               |  superpuper > omegaduper
               |  omegaduper > megadupersuperpuper
               |  megadupersuperpuper > @
               |  [self] > method
               |    self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "Access to inner state in a nested object",
      code = """
               |[] > test
               |  [] > a
               |    [] > inner_a
               |      [] > very_inner_a
               |        memory > state
               |  [] > b
               |    a > @
               |  [] > c
               |    b > @
               |  [] > d
               |    c > @
               |    [self x] > n
               |      self.inner_a.very_inner_a.state.add x > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'test.d' directly accesses state 'inner_a.very_inner_a.state' of base class 'test.a'"
      )
    ),
    TestCase(
      label = "Access to inner state in a tripple nested object",
      code = """
               |[] > nest
               |  [] > prog
               |    [] > test
               |      [] > a
               |        [] > inner_a
               |          [] > very_inner_a
               |            memory > state
               |      [] > b
               |        a > @
               |      [] > c
               |        b > @
               |      [] > d
               |        c > @
               |        [self x] > n
               |          self.inner_a.very_inner_a.state.add x > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'nest.prog.test.d' directly accesses state 'inner_a.very_inner_a.state' of base class 'nest.prog.test.a'"
      )
    ),
    TestCase(
      label = "Access to plain state in a double nested object",
      code = """
               |[] > prog
               |  [] > test
               |    [] > a
               |      memory > state
               |    [] > b
               |      a > @
               |    [] > c
               |      b > @
               |    [] > d
               |      c > @
               |      [self x] > n
               |        self.state.add x > @
               |""".stripMargin,
      expected = List(
        "Method 'n' of object 'prog.test.d' directly accesses state 'state' of base class 'prog.test.a'"
      )
    ),
    TestCase(
      label = "J2EO example with custom primitives",
      code =
        """
          |# 2022-05-25T15:02:28.522793500
          |# j2eo team
          |+alias stdlib.lang.class__Object
          |+alias stdlib.lang.class__System
          |+alias stdlib.primitives.prim__int
          |+alias stdlib.primitives.prim__float
          |+alias org.eolang.gray.cage
          |
          |[] > class__A
          |  class__Object > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Object.new > super
          |      super > @
          |      "class__A" > className
          |      [this] > init
          |        seq > @
          |          d1987169128
          |        [] > d1987169128
          |          this.state.write > @
          |            i_s1239183618
          |        [] > i_s1239183618
          |          l1804379080 > @
          |        [] > l1804379080
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            0
          |      prim__int.constructor_1 > state
          |        prim__int.new
          |      prim__float.constructor_1 > state2
          |        prim__float.new
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1757880885
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1757880885
          |      super.constructor > @
          |        this.super
          |
          |[] > class__B
          |  class__A > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__A.new > super
          |      super > @
          |      "class__B" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |      # n :: int -> int
          |      [this x] > n
          |        seq > @
          |          s278240974
          |        [] > s278240974
          |          b980138431 > @
          |        [] > b980138431
          |          s_r888655833.add > @
          |            s_r1710265848
          |        [] > s_r888655833
          |          seq > @
          |            this.state
          |            this.state2
          |        [] > s_r1710265848
          |          x > @
          |    seq > @
          |      this
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s1504642150
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s1504642150
          |      super.constructor > @
          |        this.super
          |
          |""".stripMargin,
      expected = List(
        "Method 'n' of object 'class__B.new.this' directly accesses state 'state' of base class 'class__A.new.this'",
        "Method 'n' of object 'class__B.new.this' directly accesses state 'state2' of base class 'class__A.new.this'"
      )
    ),
    TestCase(
      label = "J2EO example of state access (with inlined primitives)",
      code =
        """
          |[] > class__A
          |  [] > new
          |    [] > self
          |      "class__A" > className
          |      [self] > init
          |        seq > @
          |          d2147046752
          |        [] > d2147046752
          |          self.state.write > @
          |            i_s331418503
          |        [] > i_s331418503
          |          l2039810346 > @
          |        [] > l2039810346
          |          self.state.write 0 > @
          |      memory > state
          |    seq > @
          |      self
          |
          |[] > class__B
          |  class__A > super
          |  class__A > @
          |  [] > new
          |    [] > self
          |      class__A.new.self > super
          |      class__A.new.self > @
          |      "class__B" > className
          |      [self] > init
          |        seq > @
          |          TRUE
          |      # n :: int -> int
          |      [self x] > n
          |        seq > @
          |          s689401025
          |        [] > s689401025
          |          b306980751 > @
          |        [] > b306980751
          |          s_r1826699684.add > @
          |            s_r1769193365
          |        [] > s_r1826699684
          |          self.state > @
          |        [] > s_r1769193365
          |          x > @
          |    seq > @
          |      self
          |""".stripMargin,
      expected = List(
        "Method 'n' of object 'class__B.new.self' directly accesses state 'state' of base class 'class__A.new.self'"
      )
    )
  )

  val testsWithoutDefect: List[TestCase[List[String]]] = List(
    TestCase(
      label = "Proper access to state",
      code = """[] > a
               |  memory > state
               |  [self new_state] > update_state
               |    self.state.write new_state > @
               |[] > b
               |  a > @
               |  [this new_state] > change_state_plus_two
               |    new_state.add 2 > tmp
               |    this.update_state this tmp > @
               |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "Read on not inherited object",
      code = """[] > test
               |  [] > a_factory
               |    [] > get_a
               |      memory > state
               |  [] > b
               |    a_factory.get_a > @
               |    [self x] > n
               |      a_factory.get_a.state.add x > @
               |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "State that is not accessed",
      code = """[] > a
               |  memory > state
               |  memory > state_2
               |  cage > obj_state
               |  [] > more_state
               |    memory > inner_state
               |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "Access to local state",
      code = """[] > a
               |  memory > state
               |[] > b
               |  a > @
               |  memory > local_state
               |  [self] > func
               |    self.local_state.write 10 > @
               |""".stripMargin,
      expected = List()
    ),
    TestCase(
      label = "Access to state that is locally redefined",
      code = """[] > a
               |  memory > state
               |[] > b
               |  a > @
               |  memory > state
               |  [self] > func
               |    self.state.write 10 > @
               |""".stripMargin,
      expected = List()
    )
  )

  val shouldFail: List[TestCase[String]] = List(
    TestCase(
      label = "Funny test from J2EO",
      code =
        """# 2022-06-06T13:12:16.537007026
          |# j2eo team
          |+alias stdlib.primitives.prim__char
          |+alias stdlib.lang.class__Object
          |+alias stdlib.primitives.prim__boolean
          |+alias stdlib.lang.class__String
          |+alias stdlib.primitives.prim__int
          |+alias stdlib.primitives.prim__byte
          |+alias org.eolang.gray.cage
          |
          |[] > class__StreamUtil
          |  class__Object > super
          |  super > @
          |  [] > new
          |    [] > this
          |      class__Object.new > super
          |      super > @
          |      "class__StreamUtil" > className
          |      [this] > init
          |        seq > @
          |          TRUE
          |    seq > @
          |      this
          |  # goodClassOrNull :: Configuration -> String -> String -> Class
          |  [conf className defaultPackage] > goodClassOrNull
          |    seq > @
          |      d1978503001
          |      s1532750524
          |      s1112879561
          |      s864946514
          |    cage > clazz
          |    [] > d1978503001
          |      clazz.write > @
          |        i_s1570602757
          |    [] > i_s1570602757
          |      l1934117158 > @
          |    [] > l1934117158
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > s1532750524
          |      try_block_placeholder > @
          |    [] > s1112879561
          |      b1946557393.if > @
          |        b1169587470
          |        empty1865541902
          |    [] > b1946557393
          |      s_r1094480142.eq > @
          |        l713870233
          |    [] > s_r1094480142
          |      clazz > @
          |    [] > l713870233
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > b1169587470
          |      seq > @
          |        s1854969226
          |      [] > s1854969226
          |        b406375259.if > @
          |          b984509779
          |          empty1607674707
          |      [] > b406375259
          |        b1991420316.and > @
          |          b476339717
          |      [] > b1991420316
          |        m_i1113078977.eq > @
          |          u_pre190125356
          |      [] > m_i1113078977
          |        className.indexOf > @
          |          className
          |          l1749590931
          |      [] > l1749590931
          |        prim__int.constructor_2 > @
          |          prim__int.new
          |          123456
          |      [] > u_pre190125356
          |        l1449286889.neg > @
          |      [] > l1449286889
          |        prim__int.constructor_2 > @
          |          prim__int.new
          |          1
          |      [] > b476339717
          |        s_r1440734416.not_eq > @
          |          l1988124466
          |      [] > s_r1440734416
          |        defaultPackage > @
          |      [] > l1988124466
          |        prim__int.constructor_2 > @
          |          prim__int.new
          |          0
          |      [] > b984509779
          |        seq > @
          |          s1534668362
          |          s300780867
          |        [] > s1534668362
          |          s_r532198616.write > @
          |            b875221667
          |        [] > s_r532198616
          |          className > @
          |        [] > b875221667
          |          b1084715739.add > @
          |            s_r1379799035
          |        [] > b1084715739
          |          s_r150634256.add > @
          |            l1789332326
          |        [] > s_r150634256
          |          defaultPackage > @
          |        [] > l1789332326
          |          class__String.constructor_2 > @
          |            class__String.new
          |            "."
          |        [] > s_r1379799035
          |          className > @
          |        [] > s300780867
          |          try_block_placeholder > @
          |      [] > empty1607674707
          |        0 > @
          |    [] > empty1865541902
          |      0 > @
          |    [] > s864946514
          |      s_r1750590156 > @
          |    [] > s_r1750590156
          |      clazz > @
          |  # findInClasspath :: String -> String
          |  [className] > findInClasspath
          |    seq > @
          |      s981018779
          |    [] > s981018779
          |      m_i1191390466 > @
          |    [] > m_i1191390466
          |      findInClasspath > @
          |        this
          |        s_r67401769
          |        m_i1402121191
          |    [] > s_r67401769
          |      className > @
          |    [] > m_i1402121191
          |      primary_expression_placeholder_ClassExpressionContext.getClassLoader > @
          |        primary_expression_placeholder_ClassExpressionContext
          |  # findInClasspath :: String -> ClassLoader -> String
          |  [className loader] > findInClasspath
          |    seq > @
          |      d551316734
          |      s528515419
          |      s1856120422
          |      d311467367
          |      d255610573
          |      s303907663
          |      s853022812
          |    cage > relPath
          |    [] > d551316734
          |      relPath.write > @
          |        i_s2129215194
          |    [] > i_s2129215194
          |      s_r1502331934 > @
          |    [] > s_r1502331934
          |      className > @
          |    [] > s528515419
          |      s_r1559799241.write > @
          |        m_i1808813922
          |    [] > s_r1559799241
          |      relPath > @
          |    [] > m_i1808813922
          |      relPath.replace > @
          |        relPath
          |        l1828506930
          |        l2003637167
          |    [] > l1828506930
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        123456
          |    [] > l2003637167
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        123456
          |    [] > s1856120422
          |      s_r123789199.add_equal > @
          |        l226432780
          |    [] > s_r123789199
          |      relPath > @
          |    [] > l226432780
          |      class__String.constructor_2 > @
          |        class__String.new
          |        ".class"
          |    cage > classUrl
          |    [] > d311467367
          |      classUrl.write > @
          |        i_s1836976197
          |    [] > i_s1836976197
          |      m_i1826827970 > @
          |    [] > m_i1826827970
          |      loader.getResource > @
          |        loader
          |        s_r424772389
          |    [] > s_r424772389
          |      relPath > @
          |    cage > codePath
          |    [] > d255610573
          |      TRUE > @
          |    [] > s303907663
          |      b1736309488.if > @
          |        b1914705297
          |        b520582974
          |    [] > b1736309488
          |      s_r1735936170.not_eq > @
          |        l1868207311
          |    [] > s_r1735936170
          |      classUrl > @
          |    [] > l1868207311
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > b1914705297
          |      seq > @
          |        d497326120
          |        s1381895283
          |        s647888311
          |        s1467916079
          |        s1955873763
          |      prim__boolean.constructor_1 > inJar
          |        prim__boolean.new
          |      [] > d497326120
          |        inJar.write > @
          |          i_s960410149
          |      [] > i_s960410149
          |        m_i480673968 > @
          |      [] > m_i480673968
          |        m_i660706055.equals > @
          |          m_i660706055
          |          l515713563
          |      [] > l515713563
          |        class__String.constructor_2 > @
          |          class__String.new
          |          "jar"
          |      [] > m_i660706055
          |        classUrl.getProtocol > @
          |          classUrl
          |      [] > s1381895283
          |        s_r2060825992.write > @
          |          m_i1359476862
          |      [] > s_r2060825992
          |        codePath > @
          |      [] > m_i1359476862
          |        classUrl.toString > @
          |          classUrl
          |      [] > s647888311
          |        m_i75645059.if > @
          |          b1403837550
          |          empty1630852780
          |      [] > m_i75645059
          |        codePath.startsWith > @
          |          codePath
          |          l1032194669
          |      [] > l1032194669
          |        class__String.constructor_2 > @
          |          class__String.new
          |          "jar:"
          |      [] > b1403837550
          |        seq > @
          |          s1958688815
          |        [] > s1958688815
          |          s_r1655695459.write > @
          |            m_i1465194484
          |        [] > s_r1655695459
          |          codePath > @
          |        [] > m_i1465194484
          |          codePath.substring > @
          |            codePath
          |            m_i1519233553
          |        [] > m_i1519233553
          |          unsupported_qualifier > @
          |            unsupported_qualifier
          |      [] > empty1630852780
          |        0 > @
          |      [] > s1467916079
          |        m_i1242536275.if > @
          |          b890948945
          |          empty4247765
          |      [] > m_i1242536275
          |        codePath.startsWith > @
          |          codePath
          |          l49292481
          |      [] > l49292481
          |        class__String.constructor_2 > @
          |          class__String.new
          |          "file:"
          |      [] > b890948945
          |        seq > @
          |          s559174743
          |        [] > s559174743
          |          s_r396854319.write > @
          |            m_i1957995471
          |        [] > s_r396854319
          |          codePath > @
          |        [] > m_i1957995471
          |          codePath.substring > @
          |            codePath
          |            m_i2002864052
          |        [] > m_i2002864052
          |          unsupported_qualifier > @
          |            unsupported_qualifier
          |      [] > empty4247765
          |        0 > @
          |      [] > s1955873763
          |        s_r1605969826.if > @
          |          b239230273
          |          b1944315866
          |      [] > s_r1605969826
          |        inJar > @
          |      [] > b239230273
          |        seq > @
          |          d624830750
          |          s394997233
          |        prim__int.constructor_1 > bang
          |          prim__int.new
          |        [] > d624830750
          |          bang.write > @
          |            i_s858557996
          |        [] > i_s858557996
          |          m_i798520669 > @
          |        [] > m_i798520669
          |          codePath.lastIndexOf > @
          |            codePath
          |            l1135188547
          |        [] > l1135188547
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            123456
          |        [] > s394997233
          |          s_r1082232002.write > @
          |            m_i744438546
          |        [] > s_r1082232002
          |          codePath > @
          |        [] > m_i744438546
          |          codePath.substring > @
          |            codePath
          |            l598460248
          |            s_r1837208185
          |        [] > l598460248
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            0
          |        [] > s_r1837208185
          |          bang > @
          |      [] > b1944315866
          |        seq > @
          |          d564847767
          |          s513790493
          |          s799225701
          |        prim__int.constructor_1 > pos
          |          prim__int.new
          |        [] > d564847767
          |          pos.write > @
          |            i_s1962466331
          |        [] > i_s1962466331
          |          m_i160711899 > @
          |        [] > m_i160711899
          |          codePath.lastIndexOf > @
          |            codePath
          |            s_r1759976906
          |        [] > s_r1759976906
          |          relPath > @
          |        [] > s513790493
          |          b741723601.if > @
          |            b128775696
          |            empty1439554370
          |        [] > b741723601
          |          s_r1467327074.eq > @
          |            u_pre932879787
          |        [] > s_r1467327074
          |          pos > @
          |        [] > u_pre932879787
          |          l902010316.neg > @
          |        [] > l902010316
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            1
          |        [] > b128775696
          |          seq > @
          |            s1430251794
          |          [] > s1430251794
          |            statement_placeholder > @
          |        [] > empty1439554370
          |          0 > @
          |        [] > s799225701
          |          s_r1287098623.write > @
          |            m_i1702039888
          |        [] > s_r1287098623
          |          codePath > @
          |        [] > m_i1702039888
          |          codePath.substring > @
          |            codePath
          |            l1336808341
          |            s_r2030046490
          |        [] > l1336808341
          |          prim__int.constructor_2 > @
          |            prim__int.new
          |            0
          |        [] > s_r2030046490
          |          pos > @
          |    [] > b520582974
          |      seq > @
          |        s155212461
          |      [] > s155212461
          |        s_r1950626628.write > @
          |          l1271506146
          |      [] > s_r1950626628
          |        codePath > @
          |      [] > l1271506146
          |        prim__int.constructor_2 > @
          |          prim__int.new
          |          0
          |    [] > s853022812
          |      s_r960936344 > @
          |    [] > s_r960936344
          |      codePath > @
          |  # qualifyHost :: String -> String
          |  [url] > qualifyHost
          |    seq > @
          |      s1817015271
          |    [] > s1817015271
          |      try_block_placeholder > @
          |  # qualifyHost :: URL -> URL
          |  [url] > qualifyHost
          |    seq > @
          |      s1789211704
          |    [] > s1789211704
          |      try_block_placeholder > @
          |  cage > regexpSpecials
          |  # regexpEscape :: String -> String
          |  [plain] > regexpEscape
          |    seq > @
          |      d407751624
          |      d1348027050
          |      d228288079
          |      s1974955634
          |      s198110873
          |    cage > buf
          |    [] > d407751624
          |      buf.write > @
          |        i_s348863264
          |    [] > i_s348863264
          |      inst329162773 > @
          |    [] > inst329162773
          |      StringBuffer.constructor > @
          |        StringBuffer.new
          |    cage > ch
          |    [] > d1348027050
          |      ch.write > @
          |        i_s641519842
          |    [] > i_s641519842
          |      m_i1801715006 > @
          |    [] > m_i1801715006
          |      plain.toCharArray > @
          |        plain
          |    prim__int.constructor_1 > csup
          |      prim__int.new
          |    [] > d228288079
          |      csup.write > @
          |        i_s390936900
          |    [] > i_s390936900
          |      f_a1463177359 > @
          |    [] > f_a1463177359
          |      s_r600366426.length > @
          |    [] > s_r600366426
          |      ch > @
          |    [] > s1974955634
          |      for_loop_placeholder > @
          |    [] > s198110873
          |      m_i1855550791 > @
          |    [] > m_i1855550791
          |      buf.toString > @
          |        buf
          |  # slurp :: File -> String
          |  [f] > slurp
          |    seq > @
          |      d199116739
          |      d1696410901
          |      d361184000
          |      d1923754932
          |      s1589254291
          |      s2112782203
          |    prim__int.constructor_1 > len
          |      prim__int.new
          |    [] > d199116739
          |      len.write > @
          |        i_s1163160628
          |    [] > i_s1163160628
          |      c965928993 > @
          |    [] > c965928993
          |      prim__int.from > @
          |        m_i478445849
          |    [] > m_i478445849
          |      f.length > @
          |        f
          |    cage > buf
          |    [] > d1696410901
          |      buf.write > @
          |        i_s55494734
          |    [] > i_s55494734
          |      a_c766766054 > @
          |    [] > a_c766766054
          |      cannot_get_access_to_array_initializer > @
          |    cage > in
          |    [] > d361184000
          |      in.write > @
          |        i_s436128032
          |    [] > i_s436128032
          |      inst1812430569 > @
          |    [] > inst1812430569
          |      FileInputStream.constructor > @
          |        FileInputStream.new
          |        s_r1591301204
          |    [] > s_r1591301204
          |      f > @
          |    cage > contents
          |    [] > d1923754932
          |      contents.write > @
          |        i_s502418184
          |    [] > i_s502418184
          |      l1255509609 > @
          |    [] > l1255509609
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > s1589254291
          |      try_block_placeholder > @
          |    [] > s2112782203
          |      s_r1925960133 > @
          |    [] > s_r1925960133
          |      contents > @
          |  # slurpHadoop :: Path -> FileSystem -> String
          |  [p fs] > slurpHadoop
          |    seq > @
          |      d336310067
          |      d586557497
          |      d564165122
          |      d1316897063
          |      s446029707
          |      s652132999
          |    prim__int.constructor_1 > len
          |      prim__int.new
          |    [] > d336310067
          |      len.write > @
          |        i_s444285857
          |    [] > i_s444285857
          |      c341893682 > @
          |    [] > c341893682
          |      prim__int.from > @
          |        m_i948522114
          |    [] > m_i948522114
          |      m_i1899105943.getLen > @
          |        m_i1899105943
          |    [] > m_i1899105943
          |      fs.getFileStatus > @
          |        fs
          |        s_r823410011
          |    [] > s_r823410011
          |      p > @
          |    cage > buf
          |    [] > d586557497
          |      buf.write > @
          |        i_s1349098937
          |    [] > i_s1349098937
          |      a_c827221909 > @
          |    [] > a_c827221909
          |      cannot_get_access_to_array_initializer > @
          |    cage > in
          |    [] > d564165122
          |      in.write > @
          |        i_s847522287
          |    [] > i_s847522287
          |      m_i773647955 > @
          |    [] > m_i773647955
          |      fs.open > @
          |        fs
          |        s_r1814841034
          |    [] > s_r1814841034
          |      p > @
          |    cage > contents
          |    [] > d1316897063
          |      contents.write > @
          |        i_s1562821206
          |    [] > i_s1562821206
          |      l335663575 > @
          |    [] > l335663575
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > s446029707
          |      try_block_placeholder > @
          |    [] > s652132999
          |      s_r1300336760 > @
          |    [] > s_r1300336760
          |      contents > @
          |  cage > env
          |  cage > host
          |  # getHost :: null -> String
          |  [] > getHost
          |    seq > @
          |      s1460957046
          |    [] > s1460957046
          |      s_r1665768344 > @
          |    [] > s_r1665768344
          |      host > @
          |  # env :: null -> Environment
          |  [] > env
          |    seq > @
          |      s869513100
          |      s188230690
          |      s222056817
          |    [] > s869513100
          |      b1913713630.if > @
          |        b101403957
          |        empty82420747
          |    [] > b1913713630
          |      s_r691110711.not_eq > @
          |        l124792394
          |    [] > s_r691110711
          |      env > @
          |    [] > l124792394
          |      prim__int.constructor_2 > @
          |        prim__int.new
          |        0
          |    [] > b101403957
          |      seq > @
          |        s1913369465
          |      [] > s1913369465
          |        s_r775240424 > @
          |      [] > s_r775240424
          |        env > @
          |    [] > empty82420747
          |      0 > @
          |    [] > s188230690
          |      try_block_placeholder > @
          |    [] > s222056817
          |      s_r579058644 > @
          |    [] > s_r579058644
          |      env > @
          |  # isLocalJobTracker :: JobConf -> boolean
          |  [job] > isLocalJobTracker
          |    seq > @
          |      d635840278
          |      s1140441491
          |    cage > framework
          |    [] > d635840278
          |      framework.write > @
          |        i_s1869960884
          |    [] > i_s1869960884
          |      m_i2962930 > @
          |    [] > m_i2962930
          |      job.get > @
          |        job
          |        f_a1062167352
          |        f_a721763664
          |    [] > f_a1062167352
          |      s_r1845497458.FRAMEWORK_NAME > @
          |    [] > s_r1845497458
          |      MRConfig > @
          |    [] > f_a721763664
          |      s_r124466405.LOCAL_FRAMEWORK_NAME > @
          |    [] > s_r124466405
          |      MRConfig > @
          |    [] > s1140441491
          |      m_i1091865345 > @
          |    [] > m_i1091865345
          |      framework.equals > @
          |        framework
          |        f_a797540047
          |    [] > f_a797540047
          |      s_r1897638855.LOCAL_FRAMEWORK_NAME > @
          |    [] > s_r1897638855
          |      MRConfig > @
          |  # null :: null -> void
          |  [this] > constructor
          |    seq > @
          |      initialization
          |      s2095253806
          |      this
          |    [] > initialization
          |      this.init > @
          |        this
          |    [] > s2095253806
          |      super.constructor > @
          |        this.super""".stripMargin,
      expected =
        "There is a cycle in the decoration chain: class__StreamUtil.env.s222056817 -> class__StreamUtil.env.s_r579058644 -> class__StreamUtil.env -> class__StreamUtil.env.s222056817"
    )
  )

  def runTests(tests: List[TestCase[List[String]]]): Unit =
    tests.foreach { case TestCase(label, code, expected) =>
      registerTest(label) {
        val obtained = analyze(code).unsafeRunSync()
        assert(obtained.toSet == expected.toSet)
      }
    }

  def runFailing(tests: List[TestCase[String]]): Unit =
    tests.foreach { case TestCase(label, code, expected) =>
      registerTest(label) {
        val obtained = analyze(code).attempt.unsafeRunSync().swap.toOption.get
        assert(obtained.getMessage == expected)
      }
    }

  "analyzer" should {
    "find errors" should {
      runTests(testsWithDefect)
    }

    "not find errors" should {
      runTests(testsWithoutDefect)
    }

    "fail" should {
      runFailing(shouldFail)
    }

  }

}
