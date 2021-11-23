package org.polystat.odin.sandbox

import org.scalatest.wordspec.{AnyWordSpec, AsyncWordSpec}
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.sandbox.SimpleRecursionTestGen.{methods, objs}
import org.scalacheck.{Prop, Test}
import org.polystat.odin.analysis.EOOdinAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.effect.unsafe.implicits.global
import org.polystat.odin.utils.files

class OdinTests extends AnyWordSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)

  "odin" should {
    "find mutual recursion in auto-generated tests" in {
      val prop = Prop
        .forAllNoShrink(objs(methods)) { obj =>
          val code = obj.mkString("\n")
          EOOdinAnalyzer
            .impl[String, IO](
              implicitly[Sync[IO]],
              sourceCodeEoParser()
            )
            .analyzeSourceCode(code)
            .compile
            .toList
            .unsafeRunSync()
            .nonEmpty
        }
      check(prop, params)
    }

    "manual tests" should {
      (for {
        files <-
          files.readCodeFromDir[IO](
            "sandbox/src/test/resources/mutualrec"
          )
      } yield files.foreach { case (name, code) =>
        registerTest(name) {
          val producedErrors = EOOdinAnalyzer
            .impl[String, IO](
              implicitly[Sync[IO]],
              sourceCodeEoParser()
            )
            .analyzeSourceCode(code)
            .compile
            .toList
            .unsafeRunSync()

          print(producedErrors)
          assert(producedErrors.nonEmpty)
        }
      }).unsafeRunSync()
    }
  }

}

object OdinTests extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      code <- files.readCodeFromDir[IO](
        "sandbox/src/test/resources/mutualrec"
      )
      _ <- IO.println(code)
    } yield ExitCode.Success
  }

}
