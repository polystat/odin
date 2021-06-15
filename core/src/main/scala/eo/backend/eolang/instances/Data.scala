package eo.backend.eolang.instances

import eo.backend.eolang.{ Inline, InlineOrLines, Lines }
import eo.backend.eolang.abstractions.{ EORepr, ToEO, ToEOBnd }
import eo.backend.eolang.abstractions.ToEO.ops._
import eo.backend.eolang.instances.Bindings._
import eo.core.ast._
import utils.string.indent

object Data {
  implicit val singleByteToEO: ToEO[EOSingleByte, String] =
    new ToEO[EOSingleByte, String] {
      override def toEO(node: EOSingleByte): String = node.byte.formatted("%X")
    }

  implicit val bytesDataToEO: ToEO[EOBytesData, String] =
    new ToEO[EOBytesData, String] {
      override def toEO(node: EOBytesData): String = node.bytes.mkString("-")
    }

  implicit def bytesDataToEOBnd(
    implicit toEO: ToEO[EOBytesData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOBytesData, String, String] =
    new ToEOBnd[EOBytesData, String, String]()(toEO, eoRepr) { }

  implicit val srtDataToEO: ToEO[EOStrData, String] =
    new ToEO[EOStrData, String] {
      override def toEO(node: EOStrData): String = s"\"${node.str}\""
    }

  implicit def strDataToEOBnd(
    implicit toEO: ToEO[EOStrData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOStrData, String, String] =
    new ToEOBnd[EOStrData, String, String]()(toEO, eoRepr) { }

  implicit val regexDataToEO: ToEO[EORegexData, String] =
    new ToEO[EORegexData, String] {
      // TODO: support suffixes
      override def toEO(node: EORegexData): String = s"/${node.regex.regex}/"
    }

  implicit def regexDataToEOBnd(
    implicit toEO: ToEO[EORegexData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EORegexData, String, String] =
    new ToEOBnd[EORegexData, String, String]()(toEO, eoRepr) { }

  implicit val intDataToEO: ToEO[EOIntData, String] =
    new ToEO[EOIntData, String] {
      override def toEO(node: EOIntData): String = node.int.toString
    }

  implicit def intDataToEOBnd(
    implicit toEO: ToEO[EOIntData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOIntData, String, String] =
    new ToEOBnd[EOIntData, String, String]()(toEO, eoRepr) { }

  implicit val floatDataToEO: ToEO[EOFloatData, String] =
    new ToEO[EOFloatData, String] {
      override def toEO(node: EOFloatData): String = node.num.toString
    }

  implicit def floatDataToEOBnd(
    implicit toEO: ToEO[EOFloatData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOFloatData, String, String] =
    new ToEOBnd[EOFloatData, String, String]()(toEO, eoRepr) { }

  implicit val charDataToEO: ToEO[EOCharData, String] =
    new ToEO[EOCharData, String] {
      override def toEO(node: EOCharData): String = s"'${node.char.toString}'"
    }

  implicit def charDataToEOBnd(
    implicit toEO: ToEO[EOCharData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOCharData, String, String] =
    new ToEOBnd[EOCharData, String, String]()(toEO, eoRepr) { }

  implicit val boolDataToEO: ToEO[EOBoolData, String] =
    new ToEO[EOBoolData, String] {
      override def toEO(node: EOBoolData): String = node.bool.toString
    }

  implicit def boolDataToEOBnd(
    implicit toEO: ToEO[EOBoolData, String],
    eoRepr: EORepr[String, String]
  ): ToEOBnd[EOBoolData, String, String] =
    new ToEOBnd[EOBoolData, String, String]()(toEO, eoRepr) { }

  implicit val arrayDataToEO: ToEO[EOArray, InlineOrLines] =
    new ToEO[EOArray, InlineOrLines] {
      override def toEO(node: EOArray): InlineOrLines =
        if (node.elems.isEmpty)
          Inline("*")
        else
          Lines(
            Vector(Constants.SYMBS.ARRAY_START) ++
              node.elems.flatMap(_.toEO : Iterable[String]).map(indent)
          )
    }

  implicit def arrayDataToEOBnd(
    implicit toEO: ToEO[EOArray, InlineOrLines],
    eoRepr: EORepr[InlineOrLines, String]
  ): ToEOBnd[EOArray, InlineOrLines, String] =
    new ToEOBnd[EOArray, InlineOrLines, String]()(toEO, eoRepr) { }
}
