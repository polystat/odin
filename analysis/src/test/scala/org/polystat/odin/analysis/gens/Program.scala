package org.polystat.odin.analysis.gens

import CallGraph._

case class Program(objs: List[Object]) {

  def containsObjectWithName(s: String): Boolean = {
    objs.exists(obj => obj.name.name == s)
  }

  def toEO: String = objs.map(_.toEO).mkString("\n")

}

case class Object(
  name: ObjectName, // full object name
  ext: Option[Object], // the object extended by this object
  nestedObjs: List[Object], // the objects inside this object
  callGraph: CallGraph, // the call graph for methods of this object
) {

  def extended(
    name: ObjectName, // name of the extending object
    cg: CallGraph // call graph containing method (re-)definitions
  ): Object =
    Object(
      name = name,
      ext = Some(this.copy()),
      nestedObjs = List(),
      callGraph = callGraph.extendWith(cg),
    )

  def toEO: String = {
    val renderMethod: CallGraphEntry => String = { case (name, calls) =>
      s"""[self] > ${name.name}
         |    ${if (calls.nonEmpty)
        calls
          .map(call => s"self.${call.name} self > @")
          .mkString("\n    ")
      else
        "self > @"}""".stripMargin
    }
    s"""[] > ${name.name}
       |  ${ext.fold("")(ext => s"${ext.name.name} > @\n  ")}${callGraph
      .filter { case (method, _) =>
        method.whereDefined.name == name.name
      }
      .map(renderMethod)
      .mkString("\n  ")}""".stripMargin

  }

  def toCPP: String = {
    val renderMethod: CallGraphEntry => String = { case (name, calls) =>
      s"""  virtual void ${name.name}(){${calls
        .map(call => s"${call.name}();")
        .mkString("\n")}};""".stripMargin
    }

    s"""
       |class ${name.name.toUpperCase()} ${ext
      .fold("")(ext => s": public ${ext.name.name.toUpperCase()}")}{
       |  public:
       |  ${callGraph
      .filter { case (method, _) =>
        method.whereDefined.name == name.name
      }
      .map(renderMethod)
      .mkString("\n  ")}
       |};""".stripMargin
  }

}

case class ObjectName(parent: Option[ObjectName], name: String) {
  def show: String = parent.fold(name)(p => s"${p.show}.$name")
}

case class MethodName(whereDefined: ObjectName, name: String) {
  def show: String = s"${whereDefined.show}.$name"
}

object MethodName {

  implicit final class MethodNameOps(obj: String) {

    def %(method: String): MethodName =
      MethodName(ObjectName(None, obj), method)

  }

}
