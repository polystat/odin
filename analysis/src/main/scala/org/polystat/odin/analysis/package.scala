package org.polystat.odin

import cats.data.{NonEmptyList => Nel}
import cats.syntax.foldable._

package analysis {

  case class ObjectName(names: Nel[String]) {
    def name: String = names.last

    def show: String = names.mkString_(".")
  }

  object ObjectName {

    def apply(name: String): ObjectName =
      ObjectName(Nel.one(name))

    def fromContainer(
      container: Option[ObjectName],
      name: String
    ): ObjectName = {
      container
        .map(c => ObjectName(c.names.append(name)))
        .getOrElse(ObjectName(Nel.one(name)))
    }

  }

}
