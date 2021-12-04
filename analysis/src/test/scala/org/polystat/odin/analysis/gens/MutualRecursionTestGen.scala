package org.polystat.odin.analysis.gens

import cats.syntax.show._
import CallGraph._
import MethodName._
// import cats.Show
import org.scalacheck.Gen

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

  def genMethodName: Gen[String] =
    Gen
      .listOfN(1, Gen.alphaLowerChar)
      .map(_.mkString)

  def genCallGraph(objName: ObjectName, names: List[String]): Gen[CallGraph] = {
    val methodNames = names.map(MethodName(objName, _))
    for {
      calls <- Gen.sequence[CallGraph, CallGraphEntry](
        methodNames.map(method =>
          Gen
            .pick(1, methodNames.filter(_ != method))
            .map(calls => (method, calls.toSet))
        )
      )

    } yield calls
  }

  def genTopLvlObj(p: Program): Gen[Object] =
    for {
      name <- genTopLvlObjectName(p)
      methods <- Gen.listOfN(4, genMethodName)
      cg <- genCallGraph(name, methods)
    } yield Object(
      where = None,
      name = name,
      ext = None,
      callGraph = cg
    )

  val exampleNoCycles: CallGraph =
    exampleCgBefore.updated("a" % "d", Set("a" % "f"))

  def main(args: Array[String]): Unit = {
    println(exampleNoCycles.containsCycles)
    println(exampleCgBefore.containsCycles)
    println(exampleCgExtends.containsCycles)
    println(exampleCgAfter.containsCycles)

    genCallGraph(ObjectName(None, "a"), List("s", "b", "c", "d"))
      .sample
      .foreach(cg => println(cg.show))
    println(exampleCgBefore.extendWith(exampleCgExtends).size)
    println(exampleCgBefore.extendWith(exampleCgExtends).show)
    println(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
    println(exampleCgExtends.extendWith(exampleCgBefore).show)
  }

}
