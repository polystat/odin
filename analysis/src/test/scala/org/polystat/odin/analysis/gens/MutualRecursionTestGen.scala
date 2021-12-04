package org.polystat.odin.analysis.gens

import cats.syntax.show._
import CallGraph._
import Program._

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

  def main(args: Array[String]): Unit = {
    println(exampleCgBefore.extendWith(exampleCgExtends).size)
    println(exampleCgBefore.extendWith(exampleCgExtends).show)
    println(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
    println(exampleCgExtends.extendWith(exampleCgBefore).show)
  }

}
