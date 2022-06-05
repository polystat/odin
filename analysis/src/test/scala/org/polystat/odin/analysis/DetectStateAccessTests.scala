package org.polystat.odin.analysis

import cats.effect._
import cats.effect.unsafe.implicits.global
import org.polystat.odin.analysis.EOOdinAnalyzer.directStateAccessAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.scalatest.wordspec.AnyWordSpec

import EOOdinAnalyzer.OdinAnalysisResult._

class DetectStateAccessTests extends AnyWordSpec {
  case class TestCase(label: String, code: String, expected: List[String])

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

  val testsWithDefect: List[TestCase] = List(
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
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'a'"
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
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'a'"
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
        "Method 'n' of object 'test.c' directly accesses state 'state' of base class 'a'"
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
        "Method 'n' of object 'test.very_outer.outer.b' directly accesses state 'state' of base class 'a'"
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
        "Method 'n' of object 'test.b' directly accesses state 'state' of base class 'a'"
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
        "Method 'func' of object 'b' directly accesses state 'hidden_state' of base class 'base.inner_state.very_inner_state'",
        "Method 'func' of object 'b' directly accesses state 'inner_cage' of base class 'base.inner_state'",
        "Method 'func' of object 'b' directly accesses state 'plain_state' of base class 'base'",
        "Method 'func' of object 'b' directly accesses state 'inner_mem' of base class 'base.inner_state'"
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
        "Method 'method' of object 'superroot.root.child' directly accesses state 'state' of base class 'parent'",
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
               |  [self] > method
               |    self.state.add 10 > @
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
               |  [self] > method
               |    (self.state.add 10).write 4 > @
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
               |  [self] > method
               |    3.sub ((self.state.add 10).add 10) > @
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
      label = "State access with a weird nested decoration",
      code = """
               |[] > parent
               |  memory > state
               |[] > nest
               |  [] > child
               |    seq > @
               |      parent
               |    [self] > method
               |      self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'nest.child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "State access when decorated object is an attribute of the same object",
      code = """
               |[] > parent
               |  memory > state
               |[] > child
               |  parent > super
               |  super > @
               |  [self] > method
               |    self.state > @
               |""".stripMargin,
      expected = List(
        "Method 'method' of object 'child' directly accesses state 'state' of base class 'parent'",
      )
    ),
    TestCase(
      label = "State access when decorated object is an attribute of the same object, but with transitivity",
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
        "Method 'n' of object 'test.d' directly accesses state 'state' of base class 'a.inner_a.very_inner_a'",
      )
    )
  )

  val testsWithoutDefect: List[TestCase] = List(
    TestCase(
      label = "Proper access to state",
      code = """[] > a
               |  memory > state
               |  [self new_state] > update_state
               |    self.state.write new_state > @
               |[] > b
               |  a > @
               |  [self new_state] > change_state_plus_two
               |    new_state.add 2 > tmp
               |    self.update_state self tmp > @
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
      runTests(testsWithDefect)
    }

    "not find errors" should {
      runTests(testsWithoutDefect)
    }

  }

}
