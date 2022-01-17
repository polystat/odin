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

    def runTestsFrom[F[_]: Sync: Files](
      path: String,
      check: (String, String) => Assertion,
    ): F[Unit] =
      for {
        files <- files.readEoCodeFromResources[F](path)
      } yield files.foreach { case (name, code) =>
        registerTest(name)(check(name, code))
      }

    "manual tests" should {

      "find mutual recursion in" should {
        val fileNameToChain = Map(
          "mutual_rec_somewhere.eo" ->
            """
              |c.g -> b.f -> c.g
              |b.f -> c.g -> b.f
              |""".stripMargin,
          "nested_eo.eo" ->
            """
              |nestedA.a.g -> nestedB.f -> nestedA.a.g
              |nestedB.f -> nestedA.a.g -> nestedB.f
              |""".stripMargin,
          "nested_objects.eo" ->
            """
              |nested_objects.abstractions.base.f -> nested_objects.implementations.derived.g -> nested_objects.abstractions.base.f
              |nested_objects.implementations.derived.g -> nested_objects.abstractions.base.f -> nested_objects.implementations.derived.g
              |""".stripMargin,
          "realistic.eo" ->
            """
              |c.new.g -> a.new.f -> c.new.g
              |a.new.f -> c.new.g -> a.new.f
              |""".stripMargin,
        )

        runTestsFrom[IO](
          "/mutualrec/with_recursion",
          (fileName, code) => {
            val passes =
              for {
                expectedErrors <-
                  MutualrecTests.parseCallChains(fileNameToChain(fileName))
                actualErrors <- odinErrors(code)
              } yield {
                actualErrors.map(_.show).foreach(println)
                actualErrors.toSet == expectedErrors.toSet
              }

            assert(passes.getOrElse(false))
          }
        ).unsafeRunSync()
      }

      "not find mutual recursion" should {
        runTestsFrom[IO](
          "/mutualrec/no_recursion",
          (_, code) => {
            assert(
              odinErrors(code)
                .map(errors => errors.isEmpty)
                .getOrElse(false)
            )
          }
        ).unsafeRunSync()
      }

      "fail" should {
        runTestsFrom[IO](
          "/mutualrec/failing",
          (fileName, code) => {
            val expectedError = Map(
              "decoration.eo" -> Left(
                """Method "f" was called from the object "derived", although it is not defined there!"""
              )
            )
            assert(odinErrors(code) == expectedError(fileName))
          }
        ).unsafeRunSync()
      }

    }

  }

}

object MutualrecTests {

  import cats.syntax.foldable._

  val simpleName: P[String] =
    P.charsWhile((('a' to 'z') ++ ('A' to 'Z') ++ List('_')).contains(_))

  def stringsToMethodName(strs: NonEmptyList[String]): Option[MethodName] = {
    def stringsToObjName(strs: List[String]): Option[ObjectName] =
      strs.foldLeft[Option[ObjectName]](None) { case (acc, next) =>
        Some(ObjectName(acc, next))
      }

    stringsToObjName(strs.init).map(MethodName(_, strs.last))
  }

  val methodName: P[MethodName] = simpleName
    .repSep(P.string("."))
    .flatMap(strs =>
      stringsToMethodName(strs).fold[P0[MethodName]](
        P.failWith(
          s"Method name ${strs.combineAll} lacks an accompanying object binding!"
        )
      )(P.pure)
    )

  val cc: P[CallChain] =
    methodName
      .repSep(P.string(" -> "))
      .map(_.toList)

  val eol: P[Unit] = P.string("\r\n") | P.string("\n")

  val ccs: P[List[CallChain]] =
    eol.?.with1 *> (cc.repSep(eol).map(_.toList) <* eol.?)

  def parseCallChains(str: String): Either[P.Error, List[CallChain]] =
    ccs.parseAll(str)

}
