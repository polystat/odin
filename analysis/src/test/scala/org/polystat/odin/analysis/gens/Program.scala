package org.polystat.odin.analysis.gens

import cats.Show
import cats.syntax.show._
import CallGraph._

object Program {
  case class ObjectName(parent: Option[ObjectName], name: String)

  object ObjectName {

    implicit val show: Show[ObjectName] =
      (t: ObjectName) => t.parent.fold(t.name)(p => s"${p.toString}.${t.name}")

  }

  case class MethodName(whereDefined: ObjectName, name: String)

  object MethodName {

    implicit val show: Show[MethodName] =
      (t: MethodName) => s"${t.whereDefined.show}.${t.name}"

  }

  case class Object(
    name: ObjectName,
    ext: Option[Object],
    callGraph: CallGraph,
  ) {

    def extended(name: String, cg: CallGraph): Object =
      copy(
        name = ObjectName(Some(this.name), name),
        ext = Some(this.copy()),
        callGraph = callGraph.extendWith(cg),
      )

  }

  implicit final class MethodNameOps(obj: String) {

    def %(method: String): MethodName =
      MethodName(ObjectName(None, obj), method)

  }

}
