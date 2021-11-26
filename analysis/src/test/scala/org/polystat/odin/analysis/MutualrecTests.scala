package org.polystat.odin.analysis

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import org.polystat.odin.analysis.SimpleRecursionTestGen.{methods, objs}
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.utils.files
import org.scalacheck.{Prop, Test}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers

class MutualrecTests extends AnyWordSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)

  def odinErrors(code: String): List[EOOdinAnalyzer.OdinAnalysisError] =
    EOOdinAnalyzer
      .impl[String, IO](
        implicitly[Sync[IO]],
        sourceCodeEoParser()
      )
      .analyzeSourceCode(code)
      .compile
      .toList
      .unsafeRunSync()

  "odin" should {
    "find mutual recursion in auto-generated tests" in {
      val prop = Prop
        .forAllNoShrink(objs(methods)) { obj =>
          val code = obj.mkString("\n")
          odinErrors(code).nonEmpty
        }
      check(prop, params)
    }

    "manual tests" should {
      (for {
        files <-
          files.readCodeFromResources[IO](
            "/mutualrec"
          )
      } yield files.foreach { case (name, code) =>
        registerTest(name) {
          val producedErrors = odinErrors(code)
          print(producedErrors)
          assert(producedErrors.nonEmpty)
        }
      }).unsafeRunSync()
    }

  }

}
