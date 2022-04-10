package org.polystat.odin.analysis

import cats.effect._
import org.scalatest.wordspec.AnyWordSpec
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import cats.effect.unsafe.implicits.global
import org.polystat.odin.analysis.EOOdinAnalyzer.directStateAccessAnalyzer
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
