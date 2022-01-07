package org.polystat.odin.analysis

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import cats.syntax.functor._
import org.polystat.odin.utils.files
import org.scalacheck.{Prop, Test}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import pprint.pprintln
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.scalatest.Assertion
import fs2.io.file.Files

import scala.util.Try

class MutualrecTests extends AnyWordSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)
    .withWorkers(4)

  def odinErrors(code: String): List[EOOdinAnalyzer.OdinAnalysisError] =
    EOOdinAnalyzer
      .analyzeSourceCode[String, IO](
        EOOdinAnalyzer.advancedMutualRecursionAnalyzer[IO]
      )(code)(sourceCodeEoParser())
      .compile
      .toList
      .unsafeRunSync()

  "odin" should {
    "find mutual recursion in auto-generated tests" in {
      val prop = Prop
        .forAllNoShrink(
          genProgram(15).retryUntil(p => p.findMultiObjectCycles.nonEmpty)
        ) { prog =>
          val code = prog.toEO + "\n"
          Try(if (odinErrors(code).isEmpty) pprintln(prog, height = 10000))
            .recover(_ => pprintln(prog, height = 10000))
          odinErrors(code).nonEmpty
        }
      check(prop, params)
    }

    def runTestsFrom[F[_]: Sync: Files](
      path: String,
      check: String => Assertion,
    ): F[Unit] =
      for {
        files <- files.readEoCodeFromResources[F](path)
      } yield files.foreach { case (name, code) =>
        registerTest(name)(check(code))
      }

    "manual tests" should {

      "pass" should {
        runTestsFrom[IO](
          "/mutualrec/with_recursion",
          code => {
            assert(odinErrors(code).nonEmpty)
          }
        ).unsafeRunSync()
      }

      "fail" should {
        runTestsFrom[IO](
          "/mutualrec/no_recursion",
          code => {
            assert(odinErrors(code).isEmpty)
          }
        ).unsafeRunSync()
      }

      "crash" should {
        runTestsFrom[IO](
          "/mutualrec/failing",
          code => {
            assertThrows[java.lang.Exception](odinErrors(code))
          }
        ).unsafeRunSync()
      }

    }

  }

}
