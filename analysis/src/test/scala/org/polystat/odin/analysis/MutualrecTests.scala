package org.polystat.odin.analysis

import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import cats.syntax.functor._
import org.polystat.odin.utils.files
import org.scalacheck.{Prop, Test}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import pprint.pprintln
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer
import org.scalatest.Assertion
import fs2.io.file.Files
import org.polystat.odin.parser.eo.Parser
import org.scalacheck.Gen
import cats.parse.{Parser => P, Parser0 => P0}

import scala.util.Try

class MutualrecTests extends AnyWordSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)
    .withWorkers(4)

  def odinErrors(
                  code: String
                ): Either[String, List[CallChain]] = {
    Parser
      .parse(code)
      .flatMap(
        Analyzer
          .produceChains[Either[String, *]](_)
      )
  }

  "odin" should {
    "find mutual recursion in auto-generated tests" in {
      val gen = Gen
        .choose(2, 100)
        .flatMap(n =>
          genProgram(n).retryUntil(p => p.findMultiObjectCycles.nonEmpty)
        )

      val prop = Prop
        .forAllNoShrink(gen) { prog =>
          val code = prog.toEO + "\n"
          val assertion = for {
            errors <- odinErrors(code)
            _ <- Right(
              Try(if (errors.isEmpty) pprintln(prog, height = 10000))
                .recover(_ => pprintln(prog, height = 10000))
            )
          } yield errors.toSet == prog.findMultiObjectCycles.toSet

          assertion.getOrElse(false)

        }
      check(prop, params)
    }

    def runTestsFrom[F[_] : Sync : Files](
                                           path: String,
                                           check: (String, String) => Assertion,
                                         ): F[Unit] =
      for {
        files <- files.readEoCodeFromResources[F](path)
      } yield files.foreach { case (name, code) =>
        registerTest(name)(check(name, code))
      }

    "manual tests" should {

      "pass" should {
        val fileNameToChain = Map(
          "mutual_rec_somewhere.eo" -> {
            List(
              List("c" % "g", "b" % "f", "c" % "g")
            )
          },
          "nested_eo.eo" -> {
            //            val nestedA = ObjectName(None, "nestedA")
            //            val a = ObjectName(Some(nestedA), "a")
            //            val nestedA_a_g = MethodName(a, "g")
            //            val nestedB = ObjectName(Some(a), "nestedB")
            //            val nestedB_f = MethodName(nestedB, "f")
            //
            //            List(
            //              List(nestedA_a_g, nestedB_f, nestedA_a_g)
            //            )
          },
          "nested_objects.eo" -> "",
          "nested_objs.eo" -> "",
          "realistic.eo" -> "",
        )

        runTestsFrom[IO](
          "/mutualrec/with_recursion",
          (fileName, code) => {
            val expectedError =
              EOOdinAnalyzer.OdinAnalysisError(fileNameToChain(fileName))
            assert(odinErrors(code) == expectedError)
          }
        ).unsafeRunSync()
      }

      "fail" should {
        runTestsFrom[IO](
          "/mutualrec/no_recursion",
          (_, code) => {
            odinErrors(code)
              .map(errors => assert(errors.isEmpty))
              .getOrElse(assert(condition = false))
          }
        ).unsafeRunSync()
      }

      "crash" should {
        runTestsFrom[IO](
          "/mutualrec/failing",
          (_, code) => {
            assertThrows[java.lang.Exception](odinErrors(code))
          }
        ).unsafeRunSync()
      }

    }

  }

}

}

object MutualrecTests {

  val simpleName: P[String] = {
    P.charsWhile((('a' to 'z') ++ ('A' to 'Z') ++ List('_')).contains(_))

  }

  def stringsToMethodName(strs: NonEmptyList[String]): Option[MethodName] = {
    def stringsToObjName(strs: List[String]): Option[ObjectName] = {
      strs match {
        case Nil => None
        case obj :: tail => Some(ObjectName(stringsToObjName(tail), obj))
      }
    }

    stringsToObjName(strs.init).map(MethodName(_, strs.last))
  }

  val methodName: P[MethodName] = simpleName
    .repSep(P.string("."))
    .flatMap(strs =>
      stringsToMethodName(strs).fold[P0[MethodName]](P.fail)(P.pure)
    )

  val cc: P[CallChain] =
    methodName
      .repSep(P.string(" -> "))
      .map(_.toList)

  val ccs: P[List[CallChain]] = cc.repSep(P.string("\n")).map(_.toList)

  def main(args: Array[String]): Unit = {
    ccs
      .parseAll(
        """a -> b -> c
          |nestedA.a.g -> nestedB.f -> nestedA.a.g
          |""".stripMargin
      )
      .foreach(println)
  }

}
