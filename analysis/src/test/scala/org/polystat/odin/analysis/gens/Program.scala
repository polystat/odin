package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._
import CallGraph._

case class Program(objs: List[Object]) {

  def containsObjectWithName(s: String): Boolean = {
    objs.exists(obj => obj.name.name == s)
  }

}

object Program {

  implicit val showForProgram: Show[Program] = new Show[Program] {

    override def show(p: Program): String = {
      p.objs.map(_.show).mkString("\n")
    }

  }

}

case class Object(
  name: ObjectName, // full object name
  ext: Option[Object], // the object extended by this object
  callGraph: CallGraph, // the call graph for methods of this object
) {

  def extended(
    name: ObjectName, // name of the extending object
    cg: CallGraph // call graph containing method (re-)definitions
  ): Object =
    Object(
      name = name,
      ext = Some(this.copy()),
      callGraph = callGraph.extendWith(cg),
    )

}

object Object {

  implicit val showForObject: Show[Object] = new Show[Object] {

    val renderMethod: CallGraphEntry => String = { case (name, calls) =>
      s"""[self] > ${name.name}
         |    ${if (calls.nonEmpty)
        calls
          .map(call => s"self.${call.name} self > @")
          .mkString("\n    ")
      else
        "self > @"}""".stripMargin
    }

    override def show(t: Object): String =
      s"""[] > ${t.name.name}
         |  ${t.ext.fold("")(ext => s"${ext.name.name} > @\n  ")}${t
        .callGraph
        .filter { case (method, _) =>
          method.whereDefined.name == t.name.name
        }
        .map(renderMethod)
        .mkString("\n  ")}""".stripMargin

  }

}

case class ObjectName(parent: Option[ObjectName], name: String)

object ObjectName {

  implicit val showForObjectName: Show[ObjectName] = new Show[ObjectName] {

    override def show(t: ObjectName): String =
      t.parent.fold(t.name)(p => s"${show(p)}.${t.name}")

  }

}

case class MethodName(whereDefined: ObjectName, name: String)

object MethodName {

  implicit val showForMethodName: Show[MethodName] =
    (t: MethodName) => s"${t.whereDefined.show}.${t.name}"

  implicit final class MethodNameOps(obj: String) {

    def %(method: String): MethodName =
      MethodName(ObjectName(None, obj), method)

  }

}
