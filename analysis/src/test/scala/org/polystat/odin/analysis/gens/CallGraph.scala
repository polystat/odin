package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._
import Program._
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

    type CallGraphEntry = (MethodName, Set[MethodName])

    def extendWith(other: CallGraph): CallGraph = {

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

    def containsCycles: Boolean = {

      def traversalFrom(
        start: MethodName,
        path: List[MethodName]
      ): Boolean = {
        cg
          .get(start)
          .fold(false)(calls =>
            calls.toList match {
              case Nil => false
              case calls if calls.exists(call => path.contains(call)) => true
              case calls => calls.foldLeft(false) { (acc, call) =>
                  acc || traversalFrom(call, call :: path)
                }
            }
          )
      }

      cg.keys
        .foldLeft(false)((acc, start) =>
          acc || traversalFrom(start, start :: Nil)
        )
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
