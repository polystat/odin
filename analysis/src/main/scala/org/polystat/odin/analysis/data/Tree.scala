package org.polystat.odin.analysis.data

import org.polystat.odin.analysis.{Method, NestedObject, PartialCallGraph}
import Program.ObjectName

case class ObjectInfo(
  parent: Option[ObjectName],
  methods: Vector[Method],
  nestedObjects: Vector[NestedObject]
)

case class PartialObject(
  name: ObjectName,
  cg: PartialCallGraph,
  parentName: Option[ObjectName], // parent or decoratee
)

final case class Tree[A](node: A, children: List[Tree[A]]) {

  private def untilDefined[B, C](lst: List[B])(f: B => Option[C]): Option[C] =
    lst match {
      case Nil => None
      case head :: tail => f(head).orElse(untilDefined(tail)(f))
    }

  def find(predicate: A => Boolean): Option[A] =
    if (predicate(node))
      Some(node)
    else
      untilDefined(children)(_.find(predicate))

}
