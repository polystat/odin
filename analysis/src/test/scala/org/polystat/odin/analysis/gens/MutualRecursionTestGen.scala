package org.polystat.odin.analysis.gens

import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import cats.effect.IO
import cats.effect.kernel.Sync
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import org.scalacheck.Gen
import fs2.Stream
import fs2.text.utf8
import cats.syntax.flatMap._
import org.polystat.odin.analysis.mutualrec.advanced.{
  MethodName,
  Object,
  ObjectName,
  Program
}

import scala.util.Try

object MutualRecursionTestGen {

  // TODO: allow this to generate programs with more than 26 objects
  def genObjectName(
    p: Program,
    containerObjName: Option[ObjectName]
  ): Gen[ObjectName] = {
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)
      .retryUntil(!p.containsObjectWithName(_))
      .map(ObjectName(containerObjName, _))
  }

  def between[T](min: Int, max: Int, g: Gen[T]): Gen[List[T]] =
    for {
      n <- Gen.choose(min, max)
      lst <- Gen.listOfN(n, g)
    } yield lst

  def mapRandom[T](lst: List[T])(f: T => Gen[T]): Gen[List[T]] = {
    lst match {
      case Nil => Gen.const(List.empty)
      case last :: Nil => f(last).map(_ :: Nil)
      case head :: tail => Gen
          .oneOf(true, false)
          .flatMap(stop =>
            if (stop) f(head).map(_ :: tail)
            else mapRandom(tail)(f).map(tail => head :: tail)
          )
    }
  }

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

  def genExtendObject(
    obj: Object,
    scope: Program,
    containerObj: Option[ObjectName]
  ): Gen[Object] = for {
    // name for new object
    objName <- genObjectName(scope, containerObj)

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

  def genObject(
    scope: Program,
    containerObj: Option[ObjectName]
  ): Gen[Object] =
    for {
      objectName <- genObjectName(scope, containerObj)
      nestedObjects <- Gen.frequency(
        4 -> Gen.const(List()),
        1 -> (for {
          n <- Gen.choose(1, 3)
          a <- (0 until n).foldLeft[Gen[List[Object]]](
            Gen.const(List())
          )((accGen, _) =>
            for {
              acc <- accGen
              obj <- genObject(Program(acc), Some(objectName))
                .retryUntil(!_.callGraph.containsSingleObjectCycles)
            } yield acc ++ List(obj)
          )
        } yield a)
      )
      methods <-
        between(1, 4, genMethodName)
          .map(_.map(MethodName(objectName, _)))
      cg <- genCallGraph(methods, methods)
    } yield Object(
      name = objectName,
      parent = None,
      nestedObjs = nestedObjects,
      callGraph = cg,
    )

  def genProgram(size: Int): Gen[Program] = {

    def flattenProgram(prog: Program): List[Object] = {
      prog.objs ++
        prog
          .objs
          .flatMap(obj => flattenProgram(Program(obj.nestedObjs)))
    }

    def addExtendedOrSimpleObj(
      scope: Program,
      extendCandidates: List[Object],
      container: Option[ObjectName]
    ): Gen[Program] = for {
      extend <- Gen.oneOf(false, true)
      newObj <-
        (
          if (extend && extendCandidates.nonEmpty)
            Gen
              .oneOf(extendCandidates)
              .flatMap(extendCandidate =>
                genExtendObject(extendCandidate, scope, container)
              )
          else
            genObject(scope, container)
        )
          .retryUntil(!_.callGraph.containsSingleObjectCycles)
    } yield Program(scope.objs ++ List(newObj))

    // Type 1 -> add to topLevel
    // Type 2 -> Add to some obj as nested
    // Both T1 & T2 can be extensions of other objs
    def addObj(prog: Program): Gen[Program] = {
      def addObjRec(
        container: Object,
        containerName: Option[ObjectName]
      ): Gen[Program] = {
        for {
          deeper <- Gen.oneOf(true, false)
          currentLevel = container.nestedObjs
          next <-
            if (deeper) {
              mapRandom(currentLevel)(randomObj =>
                addObjRec(randomObj, Some(randomObj.name))
                  .map(nextLevel => randomObj.copy(nestedObjs = nextLevel.objs))
              ).map(objs => Program(objs))
            } else {
              for {
                newProg <- addExtendedOrSimpleObj(
                  Program(currentLevel),
                  flattenProgram(prog),
                  containerName
                )
              } yield newProg
            }
        } yield next
      }
      if (prog.objs.isEmpty) addExtendedOrSimpleObj(prog, List.empty, None)
      else
        addObjRec(
          Object(
            name = ObjectName(None, "THE VALUE OF THIS STRING DOESN'T MATTER"),
            parent = None,
            nestedObjs = prog.objs,
            callGraph = Map()
          ),
          None
        )
    }

    (1 to size).foldLeft(Gen.const(Program(List.empty))) { case (acc, _) =>
      for {
        oldProg <- acc
        newProg <- addObj(oldProg)
      } yield newProg
    }
  }

  def generateProgramFiles[F[_]: Files: Sync](
    n: Int,
    dir: Path,
    programGen: Gen[Program],
    converters: List[(Program => String, String)]
  ): Stream[F, Unit] =
    for {
      i <- Stream.range(1, n + 1)
      prog <- Stream.eval(retryUntilComplete(programGen))
      (convert, ext) <- Stream.emits(converters)
      _ <- Stream
        .emit(prog)
        .map(convert)
        .through(utf8.encode)
        .through(
          Files[F].writeAll(dir.resolve(s"$i.$ext"))
        )
        .as(())
    } yield ()

  def retryUntilComplete[F[_]: Sync, T](g: Gen[T]): F[T] = {
    Sync[F]
      .attempt(
        Sync[F].delay(g.sample.get)
      )
      .flatMap {
        case Left(_) => retryUntilComplete(g)
        case Right(value) => Sync[F].pure(value)
      }
  }

  def textFromProgram(
    prog: Program,
    commentMarker: String,
    display: Object => String
  ): String = {
    val cycles = prog
      .objs
      .flatMap(_.callGraph.findCycles.map(cc => commentMarker + cc.show))
      .mkString("\n")

    val progText = prog.objs.map(display).mkString("\n")

    s"""$cycles
       |
       |$progText
       |""".stripMargin

  }

  def main(args: Array[String]): Unit = {
    generateProgramFiles[IO](
      n = 20,
      dir = Path("analysis/src/test/resources/mutualrec/generated"),
      programGen = genProgram(3).retryUntil(p =>
        p.objs.exists(_.callGraph.containsMultiObjectCycles)
      ),
      converters = List(
        (p => textFromProgram(p, "# ", _.toEO), "eo"),
//        (p => textFromProgram(p, "// ", _.toCPP), "cpp"),
      )
    )
      .compile
      .drain
      .unsafeRunSync()
  }

}
