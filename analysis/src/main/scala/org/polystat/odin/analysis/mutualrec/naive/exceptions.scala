package org.polystat.odin.analysis.mutualrec.naive

object exceptions {

  case class DuplicatedMethodAttributes(
    objectName: String,
    methodAttrName: String
  ) extends Exception(
      s"""Object $objectName defines $methodAttrName multiple times."""
    )

  case class UnsupportedDecoration(decorateeName: String)
    extends Exception(
      s"""Decoration used by object $decorateeName is not supported.
         |Only the following types of decoration is supported:
         |  - base > @
         |  - base attr1 ... attrN > @""".stripMargin
    )

  case class DecorateeNotFound(decoratorName: String, decorateeName: String)
    extends Exception(
      s"""Object $decoratorName tried to decorate $decorateeName, but the
         |decoratee object with such name is not defined
         |on a top level""".stripMargin
    )

}
