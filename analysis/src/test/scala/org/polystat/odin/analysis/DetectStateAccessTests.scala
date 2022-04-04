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
      label = "Improper access to state",
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
