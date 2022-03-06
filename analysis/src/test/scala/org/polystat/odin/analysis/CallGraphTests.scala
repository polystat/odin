package org.polystat.odin.analysis

import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.scalatest.wordspec.AnyWordSpec

class CallGraphTests extends AnyWordSpec {

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
          MethodName(ObjectName("a"), "s"),
          MethodName(ObjectName("a"), "b"),
          MethodName(ObjectName("a"), "d"),
          MethodName(ObjectName("a"), "s")
        ),
        List(
          MethodName(ObjectName("a"), "b"),
          MethodName(ObjectName("a"), "d"),
          MethodName(ObjectName("a"), "s"),
          MethodName(ObjectName("a"), "b")
        ),
        List(
          MethodName(ObjectName("a"), "c"),
          MethodName(ObjectName("a"), "b"),
          MethodName(ObjectName("a"), "d"),
          MethodName(ObjectName("a"), "s"),
          MethodName(ObjectName("a"), "b")
        ),
        List(
          MethodName(ObjectName("a"), "d"),
          MethodName(ObjectName("a"), "s"),
          MethodName(ObjectName("a"), "b"),
          MethodName(ObjectName("a"), "d")
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
