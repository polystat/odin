package org.polystat.odin.analysis.gens

import cats.syntax.show._
import CallGraph._
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalacheck.Gen
import fs2.Stream
import fs2.text.utf8
import cats.syntax.flatMap._

import scala.util.Try

object MutualRecursionTestGen {

  // TODO: allow this to generate programs with more than 26 objects
  def genTopLvlObjectName(p: Program): Gen[ObjectName] = {
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)
      .retryUntil(!p.containsObjectWithName(_))
      .map(ObjectName(None, _))
  }

  def between[T](min: Int, max: Int, g: Gen[T]): Gen[List[T]] =
    for {
      n <- Gen.choose(min, max)
      lst <- Gen.listOfN(n, g)
    } yield lst

  def pickOneOrZero[T](lst: List[T]): Gen[Set[T]] =
    for {
      n <- Gen.oneOf(0, 1)
      lst <- Try(Gen.pick(n, lst)).fold(
        _ => Gen.const(Set.empty[T]),
        gen => gen.map(_.toSet)
      )
    } yield lst

  def randomlySplit[T](list: List[T]): Gen[(List[T], List[T])] =
    for {
      part1 <- Gen
        .atLeastOne(list)
        .map(_.toList)
      part2 = list.filter(!part1.contains(_))
    } yield (part1, part2)

  def genMethodName: Gen[String] =
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)

  def genCallGraph(
    methodNamesToDefine: List[MethodName],
    methodNamesToCall: List[MethodName]
  ): Gen[CallGraph] = {
    for {
      calls <- Gen.sequence[CallGraph, CallGraphEntry](
        methodNamesToDefine.map(method =>
          pickOneOrZero(methodNamesToCall.filter(_ != method))
            .map(calls => (method, calls))
        )
      )

    } yield calls
  }

  def genExtendTopLvlObj(obj: Object, p: Program): Gen[Object] = for {
    // name for new object
    objName <- genTopLvlObjectName(p)

    // new method definitions
    newMethodNames <-
      between(1, 2, genMethodName)
        .retryUntil(names =>
          names.forall(n => !obj.callGraph.containsMethodWithName(n))
        )
        .map(_.map(MethodName(objName, _)))

    // redefined methods, e.g. methods with the same name as those from the
    // parent object
    (redefinedMethodNames, otherMethodNames) <-
      randomlySplit(
        obj.callGraph.keys.toList
      )

    methodNamesToDefine = newMethodNames ++ redefinedMethodNames.map(method =>
      // replacing the object part of the method name with the new object
      // so, method 'a.s' becomes 'name.s'
      MethodName(objName, method.name)
    )

    // new methods may call any other method, both new and old
    methodNamesToCall = methodNamesToDefine ++ otherMethodNames
    callGraph <- genCallGraph(methodNamesToDefine, methodNamesToCall)

  } yield obj.extended(objName, callGraph)

  def genTopLvlObj(p: Program): Gen[Object] =
    for {
      objectName <- genTopLvlObjectName(p)
      methods <-
        between(1, 4, genMethodName)
          .map(_.map(MethodName(objectName, _)))
      cg <- genCallGraph(methods, methods)
    } yield Object(
      name = objectName,
      ext = None,
      callGraph = cg,
    )

  def genProgram(size: Int): Gen[Program] =
    for {
      initObj <- genTopLvlObj(Program(Nil)).retryUntil(obj =>
        !obj.callGraph.containsCycles
      )
      init = Gen.const(Program(List(initObj)))
      program <- (1 until size).foldLeft(init) { case (acc, _) =>
        for {
          extend <- Gen.frequency(
            (1, true),
            (1, false)
          )
          prog <- acc.flatMap(p =>
            (
              if (extend)
                Gen
                  .oneOf(p.objs)
                  .flatMap(genExtendTopLvlObj(_, p))
                  .retryUntil(obj => !obj.callGraph.containsSingleObjectCycles)
              else
                genTopLvlObj(p).retryUntil(obj => !obj.callGraph.containsCycles)
            ).map(newObj => Program(p.objs ++ List(newObj)))
          )
        } yield prog
      }

    } yield program

  def generateProgramFiles[F[_]: Files: Sync](
    n: Int,
    dir: Path,
    programGen: Gen[String]
  ): Stream[F, Unit] =
    Stream
      .range(1, n + 1)
      .evalMap(i =>
        Stream
          .eval(retryUntilComplete(programGen))
          .map(p => p.show)
          .through(utf8.encode)
          .through(
            Files[F].writeAll(dir.resolve(s"$i.eo"))
          )
          .compile
          .drain
      )

  def retryUntilComplete[F[_]: Sync, T](g: Gen[T]): F[T] =
    Sync[F]
      .attempt(
        Sync[F].delay(g.sample.get)
      )
      .flatMap {
        case Left(_) => retryUntilComplete(g)
        case Right(value) => Sync[F].pure(value)
      }

  def generateProgramFile(size: Int): Gen[String] =
    for {
      prog <- genProgram(size)
        .retryUntil(p => p.objs.exists(_.callGraph.containsMultiObjectCycles))

      cycles = prog
        .objs
        .flatMap(_.callGraph.findCycles.map(cc => "# " + cc.show))
        .mkString("\n")

      progText = prog.objs.map(_.show).mkString("\n")

    } yield s"""
               |$cycles
               |
               |$progText
               |""".stripMargin

  def main(args: Array[String]): Unit = {
    generateProgramFile(10).sample.foreach(println)
    generateProgramFiles[IO](
      300,
      Path("analysis/src/test/resources/mutualrec/generated"),
      generateProgramFile(Gen.choose(2, 8).sample.get)
    ).compile.drain.unsafeRunSync()
    //    println(
    // retryUntilComplete[IO, String](generateProgramFile(27)).unsafeRunSync()
    //    )
  }

}
