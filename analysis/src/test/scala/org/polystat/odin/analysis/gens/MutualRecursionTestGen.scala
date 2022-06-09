package org.polystat.odin.analysis.gens

import cats.effect.kernel.Sync
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.flatMap._
import fs2.Stream
import fs2.io.file.{Files, Flags, Path}
import fs2.text.utf8
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.{
  genProgram,
  generateProgramFiles
}
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.scalacheck.Gen

object MutualRecursionTestGen extends IOApp {

  var i = 0

  override def run(args: List[String]): IO[ExitCode] =
    Files[IO].delete(Path("sandbox/src/main/resources/huge/prog.eo")) *>
      (1 to 1200)
        .foldLeft[IO[Unit]](IO.unit) { case (acc, n) =>
          acc.flatMap(_ =>
            for {
              _ <- IO.println(s"Writing program #$n...")
              _ <- Stream
                .emits(genProgram(10, 50, 50, 0, 3).sample.get.toEO.getBytes)
                .through(
                  Files[IO].writeAll(
                    Path("sandbox/src/main/resources/huge/prog.eo"),
                    Flags.Append
                  )
                )
                .compile
                .drain
            } yield ()
          )
        }
        .as(ExitCode.Success)

  // NOTE: there can only be 26*27 different object names
  def genObjectName(
//                     p: Program,
    containerObjName: Option[ObjectName]
  ): ObjectName = {
    val name = s"o$i"
    i += 1
    ObjectName.fromContainer(containerObjName, name)

    //    Gen
//      .oneOf(1, 2)
//      .flatMap(n =>
//        Gen
//          .listOfN(n, Gen.alphaLowerChar)
//          .map(_.mkString)
//      )
//      .retryUntil(!p.containsObjectWithName(_))
//      .map(name => ObjectName.fromContainer(containerObjName, name))
  }

  def between[T](min: Int, max: Int, g: Gen[T]): Gen[List[T]] =
    for {
      n <- Gen.choose(min, max)
      lst <- Gen.listOfN(n, g)
    } yield lst

  def applyToRandom[T](lst: List[T])(f: T => Gen[T]): Gen[List[T]] = {
    if (lst.isEmpty)
      Gen.const(List.empty)
    else
      for {
        index <- Gen.choose(0, lst.length - 1)
        newElem <- f(lst(index))
      } yield lst.updated(index, newElem)
  }

  def pickOneOrZero[T](lst: List[T]): Gen[Option[T]] =
    for {
      n <- Gen.oneOf(0, 1)
      lst <-
        if (n == 0 || lst.isEmpty) Gen.const(None)
        else Gen.pick(n, lst).map(_.headOption)
    } yield lst

  def randomlySplit[T](list: List[T]): Gen[(List[T], List[T])] = {
    list
      .foldLeft[Gen[(List[T], List[T])]](Gen.const((List.empty, List.empty))) {
        case (acc, next) =>
          for {
            (left, right) <- acc
            isLeft <- Gen.oneOf(true, false)
          } yield if (isLeft) (left :+ next, right) else (left, right :+ next)
      }
  }

  def genMethodName(scope: Int): String = {
    val name = s"m$scope"
    name
  }
  // Gen
  //   .listOfN(1, Gen.alphaLowerChar)
  //   .map(_.mkString)
  //   .retryUntil(s => !scope.contains(s))

  def genCallGraph(
    methodNamesToDefine: List[MethodName],
    methodNamesToCall: List[MethodName]
  ): Gen[CallGraph] = {
    Gen.sequence[CallGraph, CallGraphEntry](
      methodNamesToDefine.map(method =>
        pickOneOrZero(methodNamesToCall.filter(_ != method))
          .map(calls => (method, calls.toSet))
      )
    )
  }

  def genExtendedObject(
    obj: Object,
    scope: Program,
    containerObj: Option[ObjectName],
    minMethods: Int,
    maxMethods: Int
  ): Gen[Object] = for {
    // name for new object
    _ <- Gen.const(())
    objName = genObjectName(containerObj)

    // new method definitions
    newMethodNames <-
      Gen
        .choose(minMethods, maxMethods)
        .map(n => (0 until n).map(i => MethodName(objName, genMethodName(i))))
    // between(0, 2, genMethodName(List(objName.name)))
    //   .retryUntil(names =>
    //     names.forall(n => !obj.callGraph.containsMethodWithName(n))
    //   )
    //   .map(_.map(MethodName(objName, _)))

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
    callGraph <-
      genCallGraph(methodNamesToDefine.toList, methodNamesToCall.toList)
    nestedObjects <- genNestedObjs(objName, scope)
  } yield obj.extended(objName, callGraph, nestedObjects)

  def genNestedObjs(
    containerObj: ObjectName,
    scope: Program
  ): Gen[List[Object]] =
    Gen.frequency(
      4 -> Gen.const(List()),
      1 -> (for {
        n <- Gen.choose(1, 3)
        a <- (0 until n).foldLeft[Gen[List[Object]]](
          Gen.const(List())
        )((accGen, _) =>
          for {
            acc <- accGen
            obj <- genObject(scope, Some(containerObj))
              .retryUntil(o => !o.callGraph.containsSingleObjectCycles)
          } yield acc :+ obj
        )
      } yield a)
    )

  def genObject(
    scope: Program,
    containerObj: Option[ObjectName]
  ): Gen[Object] =
    for {
      _ <- Gen.const(())
      objectName = genObjectName(containerObj)
      nestedObjects <- genNestedObjs(objectName, scope)
      methods <- Gen
        .choose(0, 4)
        .map(n =>
          (0 until n).map(i => MethodName(objectName, genMethodName(i)))
        )
      // between(0, 4, genMethodName(List(objectName.name)))
      //   .map(_.map(MethodName(objectName, _)))
      cg <- genCallGraph(methods.toList, methods.toList)
    } yield Object(
      name = objectName,
      parent = None,
      nestedObjs = nestedObjects,
      callGraph = cg,
    )

  def genProgram(
    size: Int,
    probDeepnessPercent: Int,
    probExtendednessPercent: Int,
    minMethods: Int,
    maxMethods: Int
  ): Gen[Program] = {

    def flattenProgram(prog: Program): List[Object] = {
      prog ++ prog.flatMap(obj => flattenProgram(obj.nestedObjs))
    }

    def addExtendedOrSimpleObj(
      scope: Program,
      extendCandidates: List[Object],
      container: Option[ObjectName],
      probExtendedNessPercent: Int
    ): Gen[Program] = {
      val probs = List.fill(100 - probExtendedNessPercent)(false) ++ List.fill(
        probExtendedNessPercent
      )(true)

      for {

        extend <- Gen.oneOf(probs)
        newObj <-
          (
            if (extend && extendCandidates.nonEmpty)
              Gen
                .oneOf(extendCandidates)
                .flatMap(extendCandidate =>
                  genExtendedObject(
                    extendCandidate,
                    scope,
                    container,
                    minMethods,
                    maxMethods
                  )
                )
            else
              genObject(scope, container)
          )
            .retryUntil(p => !p.callGraph.containsSingleObjectCycles)
      } yield scope ++ List(newObj)
    }
    // Type 1 -> add to topLevel
    // Type 2 -> Add to some obj as nested
    // Both T1 & T2 can be extensions of other objs
    def addObj(
      prog: Program
    ): Gen[Program] = {
      def addObjRec(
        container: Object,
        containerName: Option[ObjectName]
      ): Gen[Program] = {
        val probs = List.fill(100 - probDeepnessPercent)(false) ++ List.fill(
          probDeepnessPercent
        )(true)
        for {
          deeper <- Gen.oneOf(probs)
          currentLevel = container.nestedObjs
          next <-
            if (deeper) {
              applyToRandom(currentLevel)(randomObj =>
                addObjRec(randomObj, Some(randomObj.name))
                  .map(nextLevel => randomObj.copy(nestedObjs = nextLevel))
              ).map(objs => objs)
            } else {
              for {
                newProg <- addExtendedOrSimpleObj(
                  currentLevel,
                  flattenProgram(prog),
                  containerName,
                  probExtendednessPercent
                )
              } yield newProg
            }
        } yield next
      }

      if (prog.isEmpty)
        addExtendedOrSimpleObj(prog, List.empty, None, probExtendednessPercent)
      else
        addObjRec(
          Object(
            name =
              ObjectName("THE VALUE OF THIS STRING DOESN'T MATTER"),
            parent = None,
            nestedObjs = prog,
            callGraph = Map(),
          ),
          None
        )
    }

    (1 to size).foldLeft[Gen[Program]](Gen.const(List())) { case (acc, _) =>
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
    converter: Program => String,
    ext: String
  ): Stream[F, Unit] =
    for {
      i <- Stream.range(1, n + 1)
      prog <- Stream.eval(retryUntilComplete(programGen))
      convert <- Stream.emit(converter)
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

  def textFromProgram(displays: List[(Object => String, String)])(
    prog: Program,
  ): String = {
    def dummifyObj(prevObject: Object): Object =
      Object(prevObject.name, None, List.empty, Map.empty)

    val cyclesStruct = prog
      .flatMap(_.callGraph.findCycles)

    val badObjs = cyclesStruct
      .map(_.last.whereDefined)

    val fixedProg = badObjs.foldLeft(prog) { case (prog, badName) =>
      prog.replaceObj(badName, dummifyObj)
    }

    val cycles = cyclesStruct
      .map(cc => cc.show)
      .mkString("\n")
      .split("\n")
      .mkString("\n  ")

    val bads = displays
      .map { case (converter, marker) =>
        val programCode = prog
          .map(converter)
          .mkString("\n")
          .split("\n")
          .mkString("\n    ")
        s"""
           |  $marker: |
           |    $programCode
           |
           |""".stripMargin
      }
      .mkString("\n")

    val goods = displays
      .map { case (converter, marker) =>
        val programCode = fixedProg
          .map(converter)
          .mkString("\n")
          .split("\n")
          .mkString("\n    ")
        s"""
           |  $marker: |
           |    $programCode
           |
           |""".stripMargin
      }
      .mkString("\n")

    s"""title: Mutual recursion in a chain of inheritance
       |description: >
       |  The defect manifests in the form of call chain cycles.
       |  The following cycles are present here:
       |  $cycles
       |
       |features:
       |  - inheritance
       |  - overriding
       |  - method
       |  - polymorphism
       |
       |bad:
       |   $bads
       |   
       |good:
       |   $goods
       |""".stripMargin

  }

}

object Main extends IOApp {

  import MutualRecursionTestGen.textFromProgram

  def run(args: List[String]): IO[ExitCode] = {

    val gen = Gen
      .choose(13, 150)
      .flatMap(n =>
        genProgram(n, 1, 99, 20, 60).retryUntil(p => {
          val cycles = p.findMultiObjectCycles.map(_.length)
          println(cycles.max)
          cycles.exists(_ > 13)
        }
        )
      )

    generateProgramFiles[IO](
      1,
      Path("./generated"),
      gen,
      textFromProgram(
        List(
          ((x: Object) => x.toCPP, "source.cpp"),
          ((x: Object) => x.toJava, "Test.java"),
          ((x: Object) => x.toEO, "test.eo"),
          ((x: Object) => x.toPython, "test.py"),
        )
      ),
      "yml"
    )
      .compile
      .drain
      .as(ExitCode(0))
  }

}
