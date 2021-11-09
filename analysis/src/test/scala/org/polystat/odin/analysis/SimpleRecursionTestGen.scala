package org.polystat.odin.analysis

import org.scalacheck.{Gen, Prop, Test}

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

  def method(name: String): Gen[Method] = for {
    calls <- Gen
      .atLeastOne(methods.filter(_ != name)) // method shouldn't call itself
  } yield Method(name, calls.map(MethodCall).toList)

  def topLevelObj(name: String): Gen[TopLevelObj] = for {
    names <- Gen.atLeastOne(methods)
    methods <- Gen.sequence[List[Method], Method](names.map(method)) // methods should have different names
  } yield TopLevelObj(name, methods)

  val objs: Gen[List[TopLevelObj]] = for {
    names <- Gen.atLeastOne(toplevelObjs)
    objs <- Gen.sequence[List[TopLevelObj], TopLevelObj](names.map(topLevelObj))
  } yield objs

  def containsUnique[T](s: Seq[T]): Boolean = {
    Set.from(s).size == s.length
  }

  def main(args: Array[String]): Unit = {
    Prop
      .forAll[List[TopLevelObj], Boolean](objs)(objs => {
        val objNames: List[String] = objs.map(_.name)
        val objNamesUnique: Boolean = containsUnique(objNames)
        val methodNamesUnique: List[Boolean] = for {
          obj <- objs
          methodNames = obj.defines.map(_.name)
        } yield containsUnique(methodNames)

        (objNamesUnique :: methodNamesUnique).forall(p => p)
      })
      .check(Test.Parameters.default.withMinSuccessfulTests(1e5.toInt))

    objs.sample.map(_.mkString("\n")).foreach(println)
  }

}
