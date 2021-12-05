package org.polystat.odin.analysis.gens

import cats.syntax.show._
import CallGraph._
import MethodName._
import Object._
import org.scalacheck.Gen

import scala.util.Try

object MutualRecursionTestGen {

  val exampleCgBefore: CallGraph = Map(
    "a" % "s" -> Set("a" % "b"),
    "a" % "b" -> Set("a" % "d"),
    "a" % "c" -> Set("a" % "b"),
    "a" % "d" -> Set("a" % "s"),
  )

  val exampleCgExtends: CallGraph = Map(
    "b" % "s" -> Set("b" % "f"),
    "b" % "f" -> Set("a" % "c"),
  )

  val exampleCgAfter: CallGraph = Map(
    "b" % "s" -> Set("b" % "f"),
    "a" % "b" -> Set("a" % "d"),
    "a" % "c" -> Set("a" % "b"),
    "a" % "d" -> Set("b" % "s"),
    "b" % "f" -> Set("a" % "c"),
  )

  def genTopLvlObjectName(p: Program): Gen[ObjectName] =
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)
      .retryUntil(!p.containsObjectWithName(_))
      .map(ObjectName(None, _))

  def between[T](min: Int, max: Int, g: Gen[T]): Gen[List[T]] =
    for {
      n <- Gen.choose(min, max)
      lst <- Gen.listOfN(n, g)
    } yield lst

  def randomlySplit[T](list: List[T]): Gen[(List[T], List[T])] =
    for {
      part1 <- Gen.atLeastOne(list).map(_.toList)
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
          Try(
            Gen
              .pick(1, methodNamesToCall.filter(_ != method))
          )
            .fold(
              _ => (method, Set.empty[MethodName]),
              calls => calls.map(calls => (method, calls.toSet))
            )
        )
      )

    } yield calls
  }

  def genExtendTopLvlObj(obj: Object, p: Program): Gen[Object] = for {
    // name for new object
    name <- genTopLvlObjectName(p)

    // new method definitions
    newMethodNames <-
      between(1, 2, genMethodName).map(_.map(MethodName(name, _)))

    // redefined methods, e.g. methods with the same name as those from the
    // parent object
    (redefinedMethodNames, otherMethodNames) <-
      randomlySplit(
        obj.callGraph.keys.toList
      )

    methodNamesToDefine = newMethodNames ++ redefinedMethodNames.map(method =>
      // replacing the object part of the method name with the new object
      // so, method 'a.s' becomes 'name.s'
      MethodName(name, method.name)
    )

    // new methods may call any other method, both new and old
    methodNamesToCall = methodNamesToDefine ++ otherMethodNames
    callGraph <- genCallGraph(methodNamesToDefine, methodNamesToCall)

  } yield obj.extended(name, callGraph)

  def genTopLvlObj(p: Program): Gen[Object] =
    for {
      objectName <- genTopLvlObjectName(p)
      methods <-
        between(1, 4, genMethodName).map(_.map(MethodName(objectName, _)))
      cg <- genCallGraph(methods, methods)
    } yield Object(
      name = objectName,
      ext = None,
      callGraph = cg
    )

  val exampleNoCycles: CallGraph =
    exampleCgBefore.updated("a" % "d", Set("a" % "f"))

  def main(args: Array[String]): Unit = {
    //    println(exampleNoCycles.containsCycles)
    //    println(exampleCgBefore.containsCycles)
    //    println(exampleCgExtends.containsCycles)
    //    println(exampleCgAfter.containsCycles)
    //
    val obj = genTopLvlObj(Program(Nil)).sample.get
    println(obj.show)

    genExtendTopLvlObj(obj, Program(Nil))
      .sample
      .map(_.show)
      .foreach(println)
    //    println(exampleCgBefore.extendWith(exampleCgExtends).size)
    //    println(exampleCgBefore.extendWith(exampleCgExtends).show)
    // println(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
    //    println(exampleCgExtends.extendWith(exampleCgBefore).show)
  }

}
