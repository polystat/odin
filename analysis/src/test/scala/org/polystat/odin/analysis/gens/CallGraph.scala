package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._

import scala.annotation.tailrec
import org.scalatest.wordspec.AnyWordSpec

object CallGraph {

  type CallGraph = Map[MethodName, Set[MethodName]]
  type CallGraphEntry = (MethodName, Set[MethodName])
  type CallChain = List[MethodName]

  implicit final class CallChainOps(cc: CallChain) {

    def isShiftOf(other: CallChain): Boolean = {
      other
        .indices
        .map(i => other.drop(i) ++ other.take(i))
        .contains(cc)
    }

    def show: String = cc.map(_.show).mkString(" -> ")

  }

  implicit val showForCallGraph: Show[CallGraph] = new Show[CallGraph] {

    override def show(g: CallGraph): String =
      g.foldLeft(List.empty[String]) { case (acc, (m, calls)) =>
        s"${m.show} -> ${calls.map(_.show).mkString(", ")}" :: acc
      }.mkString("\n")

  }

  implicit final class CallGraphOps(cg: CallGraph) {

    def containsMethodWithName(name: String): Boolean = {
      cg.keySet.map(_.name).contains(name)
    }

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

    def findCycles: List[CallChain] = {
      def findCyclesRec(
        start: MethodName,
        path: CallChain,
      ): List[CallChain] = {
        cg
          .get(start)
          .fold[List[CallChain]](Nil)(calls =>
            calls.toList match {
              case Nil => Nil
              case calls if calls.exists(call => path.contains(call)) =>
                List(path ++ calls.find(call => path.contains(call)))
              case calls => calls.foldLeft[List[CallChain]](Nil) {
                  (acc, call) =>
                    acc ++ findCyclesRec(call, path ++ List(call))
                }
            }
          )
      }

      cg.foldLeft[List[CallChain]](Nil) { case (acc, (start, _)) =>
        acc ++
          findCyclesRec(start, List(start))
      // .filterNot(newChain => acc.exists(cc => newChain isShiftOf cc))
      }
    }

    def containsMultiObjectCycles: Boolean = {
      cg
        .findCycles
        .map(cc => cc.map(_.whereDefined).toSet.size)
        .exists(_ > 1)
    }

    def containsSingleObjectCycles: Boolean = {
      cg
        .findCycles
        .map(cc => cc.map(_.whereDefined).toSet.size)
        .contains(1)
    }

    def containsCycles: Boolean = {

      def traversalFrom(
        start: MethodName,
        path: CallChain
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
          acc || traversalFrom(start, List(start))
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

  val cc1: CallChain = List("a" % "s", "a" % "b", "a" % "d")
  val cc2: CallChain = List("a" % "b", "a" % "d", "a" % "s")

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
    "correctly find cycles" in {
      val cycles: Set[CallChain] = Set(
        List(
          MethodName(ObjectName(None, "a"), "s"),
          MethodName(ObjectName(None, "a"), "b"),
          MethodName(ObjectName(None, "a"), "d"),
          MethodName(ObjectName(None, "a"), "s")
        ),
        List(
          MethodName(ObjectName(None, "a"), "b"),
          MethodName(ObjectName(None, "a"), "d"),
          MethodName(ObjectName(None, "a"), "s"),
          MethodName(ObjectName(None, "a"), "b")
        ),
        List(
          MethodName(ObjectName(None, "a"), "c"),
          MethodName(ObjectName(None, "a"), "b"),
          MethodName(ObjectName(None, "a"), "d"),
          MethodName(ObjectName(None, "a"), "s"),
          MethodName(ObjectName(None, "a"), "b")
        ),
        List(
          MethodName(ObjectName(None, "a"), "d"),
          MethodName(ObjectName(None, "a"), "s"),
          MethodName(ObjectName(None, "a"), "b"),
          MethodName(ObjectName(None, "a"), "d")
        )
      )

      assert(exampleCgBefore.findCycles.toSet == cycles)
    }
  }

  "call chain" should {
    "be shift-equivalent" in {
      assert(cc1 isShiftOf cc2)
      assert(cc2 isShiftOf cc1)
    }
  }

  //
}
