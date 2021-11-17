package org.polystat.odin.sandbox

import org.scalatest.wordspec.AsyncWordSpec
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.sandbox.SimpleRecursionTestGen.{methods, objs}
import org.scalacheck.{Prop, Test}
import org.polystat.odin.analysis.EOOdinAnalyzer
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import cats.effect.{IO, Sync}
import cats.effect.testing.scalatest.AsyncIOSpec

class OdinTests extends AsyncWordSpec with AsyncIOSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)
    .withInitialSeed(123L)

  "odin" should {
    "find mutual recursion" in {
      val prop = Prop
        .forAll(objs(methods)) { obj =>
          val code = obj.mkString("\n")
          EOOdinAnalyzer
            .impl[String, IO](
              implicitly,
              sourceCodeEoParser()
            )
            .analyzeSourceCode(code)
            .toString
            .nonEmpty
        }
      check(prop, params)
    }
  }

}
