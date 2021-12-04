package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._
import CallGraph._

case class Program(objs: List[Object]) {

  def containsObjectWithName(s: String): Boolean = {
    objs.exists(obj => obj.name.name == s)
  }

}

case class Object(
  where: Option[Object], // None for top-level objects
  name: ObjectName, // full object name
  ext: Option[Object], // the object extended by this object
  callGraph: CallGraph, // the call graph for methods of this object
) {

  def extended(
    where: Option[Object], // the object where the new object is defined
    name: String, // name of the extending object
    cg: CallGraph // call graph containing method (re-)definitions
  ): Object =
    Object(
      where = where,
      name = ObjectName(where.map(_.name), name),
      ext = Some(this.copy()),
      callGraph = callGraph.extendWith(cg),
    )

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
