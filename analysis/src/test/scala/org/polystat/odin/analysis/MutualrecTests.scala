package org.polystat.odin.analysis

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Sync
import cats.implicits._
import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.utils.files
import org.scalacheck.Gen
import org.scalacheck.Test
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

  def runTestsFrom[F[_]: Sync](path: String)(
    check: (String, String) => F[Unit]
  ): F[Unit] =
    files
      .readEoCodeFromDirectory[F](path)
      .map(files =>
        files.foreach { case (name, code) =>
          test(name)(check(name, code))
        }
      )

  val fileNameToChain: Map[String, String] = Map(
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
    "3rd_defect.eo" ->
      """
        |test.parent.g -> test.child.h -> test.parent.g
        |test.child.h -> test.parent.g -> test.child.h
        |""".stripMargin
  )

  val expectedError: Map[String, String] = Map(
    "decoration.eo" ->
      """Method "f" was called from the object "derived", although it is not defined there!"""
  )

  runTestsFrom[IO]("mutualrec/with_recursion") { (fileName, code) =>
    for {
      expectedErrors <- parseCallChains[IO](fileNameToChain(fileName))
      actualErrors <- odinErrors[IO](code)
      // _ <- actualErrors.traverse_(e => IO.println(e.show))
    } yield assertEquals(actualErrors.toSet, expectedErrors.toSet)
  }.unsafeRunSync()

  runTestsFrom[IO]("mutualrec/no_recursion") { (_, code) =>
    odinErrors[IO](code).map(errors => assert(errors.isEmpty))
  }.unsafeRunSync()

  runTestsFrom[IO]("mutualrec/failing") { (fileName, code) =>
    interceptIO[Exception](odinErrors[IO](code))
      .map(error => assertEquals(error.getMessage, expectedError(fileName)))
  }.unsafeRunSync()

}

object MutualrecTests {

  val simpleName: P[String] =
    P.charsWhile((('a' to 'z') ++ ('A' to 'Z') ++ List('_')).contains(_))

  def stringsToMethodName(strs: NonEmptyList[String]): Option[MethodName] = {
    def stringsToObjName(strs: List[String]): Option[ObjectName] =
      NonEmptyList.fromList(strs).map(ObjectName(_))

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
