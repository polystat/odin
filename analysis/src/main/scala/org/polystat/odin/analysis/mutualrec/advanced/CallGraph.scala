package org.polystat.odin.analysis.mutualrec.advanced

import scala.annotation.tailrec

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

  implicit final class CallGraphOps(cg: CallGraph) {

    def toEO: String = cg
      .foldLeft(List.empty[String]) { case (acc, (m, calls)) =>
        s"${m.show} -> ${calls.map(_.show).mkString(", ")}" :: acc
      }
      .mkString("\n")

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

    def show: String =
      cg.map(cge => s"${cge._1.show} -> ${cge._2.map(_.show).mkString(", ")}").mkString("\n")

  }

}
