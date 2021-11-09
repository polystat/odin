package org.polystat.odin.analysis

import org.scalacheck.Gen

case class MethodCall(name: String) {

  override def toString: String =
    s"""self.$name self"""

}

case class Method(name: String, calls: List[MethodCall]) {

  override def toString: String = {

    val methodBody = calls match {
      case Nil => ""
      case head :: Nil => s"""  $head > @""".stripMargin
      case head :: tail =>
        s"""  seq > @
           |      ${(head :: tail).mkString("\n      ")}""".stripMargin
    }

    s"""[self] > $name
       |  $methodBody""".stripMargin

  }

}

case class TopLevelObj(name: String, defines: List[Method]) {

  override def toString: String =
    s"""[] > $name
       |  ${defines.mkString("\n  ")}""".stripMargin

}

object SimpleRecursionTestGen {
  val toplevelObjs = List("a", "b")
  val methods = List("f", "g", "h", "i")

  val exampleAst = List(
    TopLevelObj(
      "a",
      List(
        Method("g", List(MethodCall("f"))),
        Method("h", List(MethodCall("i"))),
      )
    ),
    TopLevelObj(
      "b",
      List(
        Method("f", List(MethodCall("g"))),
        Method("i", List(MethodCall("g")))
      )
    )
  )

  val method: Gen[Method] = for {
    name <- Gen.oneOf(methods)
    calls <- Gen
      .atLeastOne(methods)
      .retryUntil(calls => !calls.contains(name)) // method shouldn't call itself
  } yield Method(name, calls.map(MethodCall).toList)

  val topLevelObj: Gen[TopLevelObj] = for {
    name <- Gen.oneOf(toplevelObjs)
    methods <- Gen.containerOfN[Set, Method](2, method) // methods should have different names
  } yield TopLevelObj(name, methods.toList)

  val objs: Gen[List[TopLevelObj]] = Gen.listOfN(2, topLevelObj)

  def main(args: Array[String]): Unit = {
    objs
      .retryUntil(_ == exampleAst, maxTries = 1e7.toInt)
      .sample
      .map(_.mkString("\n"))
      .foreach(println)
  }

}
