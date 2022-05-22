package org.polystat.odin.analysis

import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._

class CallChainTests extends munit.FunSuite {

  val cc1: CallChain = List("a" % "s", "a" % "b", "a" % "d")
  val cc2: CallChain = List("a" % "b", "a" % "d", "a" % "s")

  test("be shift-equivalent") {
    assert(cc1 isShiftOf cc2)
    assert(cc2 isShiftOf cc1)
  }

}

class CallGraphTests extends munit.FunSuite {

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

  test("correctly detect cycles") {
    assert(!exampleNoCycles.containsCycles)
    assert(exampleCgBefore.containsCycles)
    assert(!exampleCgExtends.containsCycles)
    assert(exampleCgAfter.containsCycles)
  }

  test("correctly extend another call graph") {

    assert(exampleCgBefore.extendWith(exampleCgExtends).size == 5)
    assert(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
    assert(
      exampleCgBefore.extendWith(exampleCgExtends) !=
        exampleCgExtends.extendWith(exampleCgBefore)
    )

  }

  test("correctly find cycles") {
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

  test("correctly extend another call graph") {

    assert(exampleCgBefore.extendWith(exampleCgExtends).size == 5)
    assert(exampleCgBefore.extendWith(exampleCgExtends) == exampleCgAfter)
    assert(
      exampleCgBefore.extendWith(exampleCgExtends) !=
        exampleCgExtends.extendWith(exampleCgBefore)
    )

  }

}
