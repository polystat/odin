package org.polystat.odin.sandbox

import cats.Eq
import cats.effect.IO
import cats.syntax.eq._
import org.scalacheck.{Gen, Prop, Test}
import fs2.Stream

case class MethodCall(name: String) {

  override def toString: String =
    s"""self.$name self"""

}

case class Method(name: String, calls: List[MethodCall]) {

  override def toString: String = {
    val methodBody = calls match {
      case Nil => ""
      case head :: Nil => s"""  $head > @""".stripMargin
      case lst =>
        s"""  seq > @
           |      ${lst.mkString("\n      ")}""".stripMargin
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

  def containsUnique[T](s: Seq[T]): Boolean = {
    Set.from(s).size == s.length
  }

  def between[T](min: Int, max: Int, seq: Seq[T]): Gen[Seq[T]] = for {
    size <- Gen.choose(min, max)
    seq <- Gen.pick(size, seq)
  } yield seq.toSeq

  def method(methodPool: List[String])(name: String): Gen[Method] = for {
    call <- Gen.oneOf(methodPool.filter(_ != name)) // method shouldn't call itself
  } yield Method(name, (call :: Nil).map(MethodCall))

  def topLevelObj(
    methodPool: List[String]
  )(methodCallPool: List[String])(name: String): Gen[TopLevelObj] =
    for {
      names <- Gen.const(methodPool)
      methods <-
        Gen.sequence[List[Method], Method](names.map(method(methodCallPool))) // methods should have different names
    } yield TopLevelObj(name, methods)

  def objs(methodDeclPool: List[String]): Gen[List[TopLevelObj]] = for {
    aMethods <- Gen.pick(2, methodDeclPool).map(_.toList)
    bMethods = methodDeclPool.filter(name => !aMethods.contains(name))
    a <- topLevelObj(aMethods)(bMethods)("a")
    b <- topLevelObj(bMethods)(aMethods)("b")
  } yield List(a, b)

  def main(args: Array[String]): Unit = {

    Gen
      .listOfN(1, objs(methods))
      .sample
      .foreach(_.foreach(s => println(s.mkString("\n"))))

    import cats.effect.unsafe.implicits.global
    val max = 1e7.toInt
    Stream
      .range(0, max)
      .evalMap[IO, List[TopLevelObj]](i =>
        IO.print(f"${(i / max.toDouble) * 100}%1.2f%%\r") >>
          IO.pure(objs(methods).sample.get)
      )
      .dropWhile(_ != exampleAst)
      .compile
      .toList
      .map(_.headOption)
      .flatMap(sample =>
        IO.delay(sample.foreach(l => println(l.mkString("\n"))))
      )
      .unsafeRunSync()
  }

}
