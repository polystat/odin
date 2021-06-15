
package eo.core

import com.github.tarao.nonempty.collection.NonEmpty

import scala.util.matching.Regex

package ast {
  // Program ///////////////////////////////////////////////////////////////////
  sealed case class EOProg(
    metas: EOMetas,
    bnds: Vector[EOBnd],
  )

  // Metas  ////////////////////////////////////////////////////////////////////

  sealed case class EOMetas(
    pack: Option[String],
    metas: Vector[EOMeta]
  )

  // Concrete metas ////////////////////////////////////////////////////////////
  sealed trait EOMeta

  sealed case class EOAliasMeta(
    alias: String,
    src: String
  ) extends EOMeta

  sealed case class EORTMeta(
    rtName: String,
    src: String
  ) extends EOMeta

  // Binding name //////////////////////////////////////////////////////////////
  sealed trait BndName {
    val name: String
  }

  sealed case class LazyBnd(
    override val name: String,
  ) extends BndName

  sealed case class ConstBnd(
    override val name: String,
  ) extends BndName

  // Binding ///////////////////////////////////////////////////////////////////
  sealed trait EOBnd {
    val expr: EOExpr
  }

  sealed case class EOAnonExpr(
    override val expr: EOExpr,
  ) extends EOBnd

  sealed case class EOBndExpr(
    val bndName: EONamedBnd,
    override val expr: EOExpr,
  ) extends EOBnd

  sealed trait EONamedBnd {
    val name: BndName
  }

  sealed case class EOAnyNameBnd(
    override val name: BndName
  ) extends EONamedBnd

  sealed case class EODecoration(
    override val name: BndName = LazyBnd(Constants.ATTRS.DECORATION)
  ) extends EONamedBnd

  // Expression ////////////////////////////////////////////////////////////////
  sealed trait EOExpr

  // / Object //////////////////////////////////////////////////////////////////
  sealed case class EOObj(
    freeAttrs: Vector[LazyBnd],
    varargAttr: Option[LazyBnd],
    bndAttrs: Vector[EOBndExpr],
  ) extends EOExpr

  // / Application /////////////////////////////////////////////////////////////
  sealed trait EOApp extends EOExpr

  sealed case class EOSimpleApp(name: String) extends EOApp

  sealed case class EODot(src: EOExpr, name: String) extends EOApp

  sealed case class EOCopy(trg: EOExpr, args: NonEmpty[EOBnd, Vector[EOBnd]]) extends EOApp

  // / Data ////////////////////////////////////////////////////////////////////
  sealed trait EOData extends EOExpr

  sealed case class EOSingleByte(byte: Byte)
  sealed case class EOBytesData(bytes: NonEmpty[Byte, Vector[Byte]]) extends EOData

  sealed case class EOStrData(str: String) extends EOData

  sealed case class EORegexData(regex: Regex) extends EOData

  sealed case class EOIntData(int: Int) extends EOData

  sealed case class EOFloatData(num: Float) extends EOData

  sealed case class EOCharData(char: Char) extends EOData

  sealed case class EOBoolData(bool: Boolean) extends EOData

  sealed case class EOArray(elems: Vector[EOBnd]) extends EOData
}
