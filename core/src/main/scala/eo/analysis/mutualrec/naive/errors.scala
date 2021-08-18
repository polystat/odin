package eo.analysis.mutualrec.naive

object errors {
  case class DuplicatedMethodAttributes(
    val objectName: String,
    val methodAttrName: String
  ) extends Exception(
    s"""Object ${objectName} defines ${methodAttrName} multiple times."""
  )

  case class UnsupportedDecoration(val decorateeName: String) extends Exception(
    s"""Decoration used by object ${decorateeName} is not supported.
       |Only the following types of decoration is supported:
       |  - base > @
       |  - base attr1 ... attrN > @""".stripMargin
  )

  case class DecorateeNotFound(val decoratorName: String, val decorateeName: String) extends Exception(
    s"""Object ${decoratorName} tried to decorate ${decorateeName}, but the
       |decoratee object with such name is not defined
       |on a top level""".stripMargin
  )
}
