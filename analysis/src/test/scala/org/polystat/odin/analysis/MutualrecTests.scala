package org.polystat.odin.analysis

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.effect.{IO, Sync}
import cats.parse.{Parser => P, Parser0 => P0}
import cats.implicits._
import fs2.io.file.Files
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.utils.files
import org.scalacheck.{Gen, Test}
import org.scalacheck.effect.PropF
import pprint.pprintln

class MutualrecTests extends CatsEffectSuite with ScalaCheckEffectSuite {

  import MutualrecTests.parseCallChains

  override def scalaCheckTestParameters: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)
    .withWorkers(4)

  def odinErrors[F[_]](
    code: String
  )(implicit F: ApplicativeError[F, Throwable]): F[List[CallChain]] = {
    F.fromEither(
      Parser
        .parse(code)
        .flatMap(
          Analyzer
            .produceChains[Either[String, *]](_)
        )
        .leftMap(new Exception(_))
    )
  }

  test("find mutual recursion in auto-generated tests") {
    val gen = Gen
      .choose(2, 100)
      .flatMap(n =>
        genProgram(n).retryUntil(p => p.findMultiObjectCycles.nonEmpty)
      )

    PropF.forAllNoShrinkF(gen) { prog =>
      val code = prog.toEO + "\n"
      odinErrors[IO](code)
        .map(errors =>
          assertEquals(errors.toSet, prog.findMultiObjectCycles.toSet)
        )
        .onError(_ => IO.delay(pprintln(prog, height = 10000)))
    }
  }

  def runTestsFrom[F[_]: Sync: Files](path: String)(
    check: (String, String) => F[Unit]
  ): F[Unit] =
    files
      .readEoCodeFromResources[F](path)
      .map(files =>
        files.foreach { case (name, code) =>
          test(name)(check(name, code))
        }
      )

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

  val expectedError = Map(
    "decoration.eo" ->
      """Method "f" was called from the object "derived", although it is not defined there!"""
  )

  runTestsFrom[IO]("/mutualrec/with_recursion") { (fileName, code) =>
    for {
      expectedErrors <- parseCallChains[IO](fileNameToChain(fileName))
      actualErrors <- odinErrors[IO](code)
      _ <- actualErrors.traverse_(e => IO.println(e.show))
    } yield assertEquals(actualErrors.toSet, expectedErrors.toSet)
  }.unsafeRunSync()

  runTestsFrom[IO]("/mutualrec/no_recursion") { (_, code) =>
    odinErrors[IO](code).map(errors => assert(errors.isEmpty))
  }.unsafeRunSync()

  runTestsFrom[IO]("/mutualrec/failing") { (fileName, code) =>
    interceptIO[Exception](odinErrors[IO](code))
      .map(error => assertEquals(error.getMessage, expectedError(fileName)))
  }.unsafeRunSync()

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

  def parseCallChains[F[_]](
    str: String
  )(implicit F: ApplicativeError[F, Throwable]): F[List[CallChain]] =
    F.fromEither(
      ccs
        .parseAll(str)
        .leftMap(error => new Exception(error.toString))
    )

}
