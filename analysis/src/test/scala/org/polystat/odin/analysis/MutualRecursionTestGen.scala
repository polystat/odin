package org.polystat.odin.analysis

import cats.Show
import cats.syntax.show._

import scala.annotation.tailrec

object MutualRecursionTestGen {

  type CallGraph = Map[MethodName, Set[MethodName]]

  object CallGraph {

    implicit val show: Show[CallGraph] = (g: CallGraph) =>
      g
        .foldLeft(List.empty[String]) { case (acc, (m, calls)) =>
          s"${m.show} -> ${calls.map(_.show).mkString(", ")}" :: acc
        }
        .mkString("\n")

  }

  import CallGraph._

  case class ObjectName(parent: Option[ObjectName], name: String)

  object ObjectName {

    implicit val show: Show[ObjectName] =
      (t: ObjectName) => t.parent.fold(t.name)(p => s"${p.toString}.${t.name}")

  }

  case class MethodName(whereDefined: ObjectName, name: String)

  object MethodName {

    implicit val show: Show[MethodName] =
      (t: MethodName) => s"${t.whereDefined.show}.${t.name}"

  }

  case class Object(
    name: ObjectName,
    ext: Option[Object],
    callGraph: CallGraph,
  ) {

    def extended(name: String, cg: CallGraph): Object =
      copy(
        name = ObjectName(Some(this.name), name),
        ext = Some(this),
        callGraph = callGraph.extendWith(cg),
      )

  }

  implicit final class CallGraphOps(cg: CallGraph) {

    def extendWith(other: CallGraph): CallGraph = {
      type CallGraphEntry = (MethodName, Set[MethodName])

      @tailrec
      def extendWithRec(rem: List[CallGraphEntry], acc: CallGraph): CallGraph =
        rem match {
          case Nil => acc
          case (newMethod, calls) :: tail =>
            extendWithRec(
              tail,
              acc
                .find { case (oldMethod, _) =>
                  oldMethod.name == newMethod.name
                }
                .fold[CallGraph](acc.updated(newMethod, calls)) {
                  case (oldMethod, _) => acc
                      .removed(oldMethod)
                      .updated(newMethod, calls)
                      .replaceCallsTo(oldMethod, newMethod)
                }
            )
        }

      extendWithRec(other.toList, cg)
    }

    def replaceCallsTo(
      oldMethod: MethodName,
      newMethod: MethodName
    ): CallGraph = {
      cg.map { case (m, calls) =>
        (
          m,
          if (calls.contains(oldMethod))
            (calls - oldMethod) + newMethod
          else
            calls
        )
      }
    }

  }

  implicit final class MethodNameOps(obj: String) {

    def %(method: String): MethodName =
      MethodName(ObjectName(None, obj), method)

  }

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
  }

}
