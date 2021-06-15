package eo.backend.eolang.transformations

import eo.backend.eolang.{ Inline, InlineOrLines, Lines }
//import eo.backend.eolang.Utils.withBindToName
//import eo.backend.eolang.transformations.Bindings.bndToEO
import eo.core.ast._
import utils.string.indent

object Data {
//  def singleByteToEO(byte: EOSingleByte): String = byte.byte.formatted("%X")
//  def bytesDataToEO(bytes: EOBytesData): String = bytes.bytes.mkString("-")
//  def bytesDataBndToEO: EOBytesData => String => String = withBindToName(bytesDataToEO)
//
//  def strDataToEO(str: EOStrData): String = s"\"${str.str}\""
//  def strDataBndToEO: EOStrData => String => String = withBindToName(strDataToEO)
//
//  // TODO: support suffixes
//  def regexDataToEO(eoReg: EORegexData): String = s"/${eoReg.regex.regex}/"
//  def regexDataBndToEO: EORegexData => String => String = withBindToName(regexDataToEO)
//
//  def intDataToEO(int: EOIntData): String = int.int.toString
//  def intDataBndToEO: EOIntData => String => String = withBindToName(intDataToEO)
//
//  def floatDataToEO(eoFloat: EOFloatData): String = eoFloat.num.toString
//  def floatDataBndToEO: EOFloatData => String => String = withBindToName(floatDataToEO)
//
//  def charDataToEO(eoChar: EOCharData): String = s"'${eoChar.char.toString}'"
//  // TODO: with binding
//
//  def boolDataToEO(eoBool: EOBoolData): String = eoBool.bool.toString
//
//  def arrayToEO(eoArr: EOArray): InlineOrLines =
//    if (eoArr.elems.isEmpty) {
//      Inline("*")
//    } else {
//      Lines(
//        Vector(Constants.SYMBS.ARRAY_START) ++
//          eoArr.elems.flatMap(bndToEO).map(indent)
//      )
//    }
//
//  def dataToEO: EOData => InlineOrLines = {
//    case bs@EOBytesData(_) => Inline(bytesDataToEO(bs))
//    case s@EOStrData(_)    => Inline(strDataToEO(s))
//    case r@EORegexData(_)  => Inline(regexDataToEO(r))
//    case i@EOIntData(_)    => Inline(intDataToEO(i))
//    case f@EOFloatData(_)  => Inline(floatDataToEO(f))
//    case c@EOCharData(_)   => Inline(charDataToEO(c))
//    case b@EOBoolData(_)   => Inline(boolDataToEO(b))
//    case a@EOArray(_)      => arrayToEO(a)
//  }
}
