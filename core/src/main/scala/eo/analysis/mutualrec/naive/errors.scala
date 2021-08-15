package eo.analysis.mutualrec.naive

object errors {
  case class DuplicatedMethodAttributes(
    val objectName: String,
    val methodAttrName: String
  ) extends Exception(
    s"""Object ${objectName} defines ${methodAttrName} multiple times."""
  )
}
