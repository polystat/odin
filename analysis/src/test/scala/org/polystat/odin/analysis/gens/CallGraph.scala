package org.polystat.odin.analysis.gens

import cats.Show
import Program._
import cats.syntax.show._
import scala.annotation.tailrec

object CallGraph {

  type CallGraph = Map[MethodName, Set[MethodName]]

  implicit val show: Show[CallGraph] = (g: CallGraph) =>
    g
      .foldLeft(List.empty[String]) { case (acc, (m, calls)) =>
        s"${m.show} -> ${calls.map(_.show).mkString(", ")}" :: acc
      }
      .mkString("\n")

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

}
