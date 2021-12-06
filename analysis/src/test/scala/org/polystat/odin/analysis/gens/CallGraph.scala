package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._

import scala.annotation.tailrec
import org.scalatest.wordspec.AnyWordSpec

object CallGraph {

  type CallGraph = Map[MethodName, Set[MethodName]]
  type CallGraphEntry = (MethodName, Set[MethodName])

  implicit val showForCallGraph: Show[CallGraph] = (g: CallGraph) =>
    g
      .foldLeft(List.empty[String]) { case (acc, (m, calls)) =>
        s"${m.show} -> ${calls.map(_.show).mkString(", ")}" :: acc
      }
      .mkString("\n")

  implicit final class CallGraphOps(cg: CallGraph) {

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

class CallGraphTests extends AnyWordSpec {

  import CallGraph._
  import MethodName._

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

  val exampleNoCycles: CallGraph =
    exampleCgBefore.updated("a" % "d", Set("a" % "f"))

  "call graph" should {
    "correctly detect cycles" in {
      assert(!exampleNoCycles.containsCycles)
      assert(exampleCgBefore.containsCycles)
      assert(!exampleCgExtends.containsCycles)
      assert(exampleCgAfter.containsCycles)
    }

    "correctly extend another call graph" in {

      assert(exampleCgBefore.extendWith(exampleCgExtends).size == 5)
      assert(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
      assert(
        exampleCgBefore.extendWith(exampleCgExtends) !=
          exampleCgExtends.extendWith(exampleCgBefore)
      )

    }
  }

  //
}
